import java.time.Instant
import java.nio.file.{Path, Paths}
import scala.io.StdIn

object App:

  def run(config: AppConfig, logger: Logger): Unit =
    var state: AppState = AppState.NoStorage
    var running = true

    while running do
      printMenu()
      val line = StdIn.readLine()

      if line == null then running = false
      else
        Command.parse(line) match
          case None =>
            println("Некорректный пункт меню.")
          case Some(Command.Exit) =>
            println("До свидания.")
            running = false
          case Some(cmd) =>
            state = handleCommand(cmd, state, config, logger)

  private def handleCommand(cmd: Command, state: AppState, config: AppConfig, logger: Logger): AppState =
    (cmd, state) match
      // --- СОЗДАНИЕ ---
      case (Command.Create, _) =>
        println("Введите имя файла хранилища (например, my-backup.store):")
        val fileName = StdIn.readLine().trim
        if fileName.isEmpty then return state

        val path = Paths.get(fileName)
        val newSession = StorageSession(
          storageFile = path,
          name = "Мое хранилище",
          encrypted = false,
          manifest = Manifest(VersionId(0), Instant.now(), Map.empty),
          versions = Nil,
          sources = Nil
        )

        Storage.createNew(path, newSession) match
          case Right(_) =>
            println("✅ Хранилище создано.")
            logger.info(s"Создано хранилище: $fileName")
            AppState.Opened(newSession)
          case Left(err) =>
            println(s"❌ ${err.message}")
            state

      // --- ОТКРЫТИЕ ---
      case (Command.Open, _) =>
        println("Введите путь к файлу хранилища:")
        val path = Paths.get(StdIn.readLine().trim)
        Storage.readSession(path) match
          case Right(session) =>
            println(s"✅ Хранилище '${session.name}' открыто.")
            logger.info(s"Открыто хранилище: $path")
            AppState.Opened(session)
          case Left(err) =>
            println(s"❌ ${err.message}")
            state

      // --- ИНФОРМАЦИЯ ---
      case (Command.Info, AppState.Opened(session)) =>
        println(s"Хранилище: ${session.name}")
        println(s"Файл: ${session.storageFile.toAbsolutePath}")
        println(s"Источников: ${session.sources.size}")
        println(s"Версий: ${session.versions.size}")
        println(s"Файлов в базе: ${session.manifest.files.size}")
        state

      // --- ИСТОЧНИКИ (Добавление) ---
      case (Command.Sources, AppState.Opened(session)) =>
        println("Текущие источники:")
        session.sources.foreach(s => println(s"  - ${s.path}"))
        println("\nДобавить новый источник? (y/n)")
        if StdIn.readLine().trim.toLowerCase == "y" then
          println("Введите путь к папке:")
          val p = Paths.get(StdIn.readLine().trim)
          val newSource = Source(SourceId(session.sources.size + 1L), p)
          val updated = session.copy(sources = session.sources :+ newSource)
          Storage.saveSession(updated, Map.empty) match
            case Right(_) => println("✅ Источник добавлен.")
            case Left(err) => println(s"❌ Ошибка сохранения: ${err.message}")
          AppState.Opened(updated)
        else state

      // --- СКАНИРОВАНИЕ ---
      case (Command.Scan, AppState.Opened(session)) =>
        if session.sources.isEmpty then
          println("⚠️ Сначала добавьте источники (пункт 4).")
          state
        else
          println("Сканирование...")
          val scanner = Scanner(config.hashAlgorithm)
          val result = scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            println("✨ Изменений не обнаружено.")
          else
            println(s"📊 Изменения: +${stats.added} новых, ~${stats.modified} измененных, -${stats.removed} удаленных.")
            if stats.unreadable + stats.unavailable > 0 then
              println(s"⚠️ Проблемных файлов: ${stats.unreadable + stats.unavailable}")
          state

      // --- РЕЗЕРВНОЕ КОПИРОВАНИЕ (BACKUP) ---
      case (Command.Backup, AppState.Opened(session)) =>
        if session.sources.isEmpty then
          println("⚠️ Нет источников для копирования.")
          state
        else
          println("Сканирование перед копированием...")
          val scanner = Scanner(config.hashAlgorithm)
          val result = scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            println("✨ Нет новых данных для сохранения.")
            state
          else
            println(s"Обнаружено: +${stats.added}, ~${stats.modified}, -${stats.removed} файлов.")

            // Спрашиваем подтверждение, если включено в конфиге
            if config.confirmBeforeBackup then
              print("Добавить изменения в хранилище? (y/n): ")
              if StdIn.readLine().trim.toLowerCase != "y" then
                println("Отменено.")
                return state

            // 1. Собираем физические пути для новых и измененных файлов
            val filesToAdd = changes.collect {
              case Change.Added(entry) => entry
              case Change.Modified(_, current) => current
            }.flatMap { entry =>
              // Находим базовый путь источника, чтобы восстановить полный путь к файлу
              session.sources.find(_.id == entry.sourceId).map { src =>
                (entry.hash, src.path.resolve(entry.relPath.value))
              }
            }.toMap // Map[Hash, Path]

            // 2. Строим новый манифест
            val prevFiles = session.manifest.files
            val removedKeys = changes.collect { case Change.Removed(e) => (e.sourceId, e.relPath) }.toSet
            val addedEntries = changes.collect { case Change.Added(e) => (e.sourceId, e.relPath) -> e }.toMap
            val modifiedEntries = changes.collect { case Change.Modified(_, curr) => (curr.sourceId, curr.relPath) -> curr }.toMap

            // (Старые - Удаленные) + Новые + Измененные
            val newFilesMap = (prevFiles -- removedKeys) ++ addedEntries ++ modifiedEntries

            // 3. Создаем новую версию
            val newVersionId = VersionId(session.manifest.version.value + 1)
            val newManifest = Manifest(newVersionId, Instant.now(), newFilesMap)

            val newRecord = VersionRecord(
              id = newVersionId,
              createdAt = newManifest.createdAt,
              added = stats.added,
              modified = stats.modified,
              removed = stats.removed,
              result = if result.problems.isEmpty then BackupOutcome.Success else BackupOutcome.Partial,
              comment = None
            )

            val newSession = session.copy(
              manifest = newManifest,
              versions = newRecord +: session.versions // Новые версии сверху
            )

            println("Запись данных в хранилище (ZIP)...")
            Storage.saveSession(newSession, filesToAdd) match
              case Right(_) =>
                println(s"✅ Резервное копирование завершено.")
                println(s"Версия хранилища: ${newVersionId.value}")
                logger.info(s"Бэкап успешен. Версия: ${newVersionId.value}")
                AppState.Opened(newSession)
              case Left(err) =>
                println(s"❌ Ошибка записи: ${err.message}")
                logger.error(s"Ошибка бэкапа: ${err.message}")
                state

      // --- ПРОСМОТР ВЕРСИЙ ---
      case (Command.Versions, AppState.Opened(session)) =>
        if session.versions.isEmpty then
          println("Версий пока нет (резервное копирование не проводилось).")
        else
          println("Версии хранилища:")
          session.versions.foreach { v =>
            val dateStr = v.createdAt.toString.take(19).replace("T", " ")
            val comment = v.comment.getOrElse("")
            println(f"${v.id.value}%3d   $dateStr   +${v.added}%d  ~${v.modified}%d  -${v.removed}%d   $comment")
          }
        state

      // --- ОСТАЛЬНОЕ ---
      case (_, AppState.NoStorage) =>
        println("⚠️ Сначала откройте или создайте хранилище (пункты 1 или 2).")
        state

      case _ =>
        println("⚠️ Эта команда пока не реализована в меню.")
        state