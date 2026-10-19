import java.time.Instant
import java.nio.file.{Files, Path, Paths}
import scala.io.StdIn
import javax.crypto.spec.SecretKeySpec

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
          case None => println("Некорректный пункт меню.")
          case Some(Command.Exit) =>
            println("До свидания.")
            running = false
          case Some(cmd) => state = handleCommand(cmd, state, config, logger)

  private def handleCommand(cmd: Command, state: AppState, config: AppConfig, logger: Logger): AppState =
    (cmd, state) match
      // СОЗДАНИЕ
      case (Command.Create, _) =>
        println("Введите имя файла хранилища (например, my-backup.store):")
        val fileName = StdIn.readLine().trim
        if fileName.isEmpty then return state

        val path = Paths.get(fileName)

        println("Использовать шифрование? (y/n):")
        val isEncrypted = StdIn.readLine().trim.toLowerCase == "y"

        var keyOpt: Option[SecretKeySpec] = None

        if isEncrypted then
          println("Выберите способ хранения ключа:")
          println("1. Сгенерировать и сохранить в файл рядом с программой")
          println("2. Сгенерировать и показать мне")
          val choice = StdIn.readLine().trim

          val keyStr = Crypto.generateKey()
          keyOpt = Crypto.parseKey(keyStr.value).toOption

          if choice == "1" then
            val keyDir = Paths.get(config.keyDirectory)
            Files.createDirectories(keyDir)
            val keyFile = keyDir.resolve(s"${path.getFileName}.key")
            Files.writeString(keyFile, keyStr.value)
            println(s"Ключ сохранен в: ${keyFile.toAbsolutePath}")
          else
            println(s"\nВАШ КЛЮЧ: ${keyStr.value}")
            println("Запишите его! Без него вы не сможете открыть хранилище.")
            println("Подтвердите, что ключ сохранен (введите y):")
            if StdIn.readLine().trim.toLowerCase != "y" then
              println("Отменено.")
              return state

        val newSession = StorageSession(
          storageFile = path,
          name = "Мое хранилище",
          encrypted = isEncrypted,
          manifest = Manifest(VersionId(0), Instant.now(), Map.empty),
          versions = Nil,
          sources = Nil,
          key = keyOpt
        )

        Storage.createNew(path, newSession, keyOpt) match
          case Right(_) =>
            println("Хранилище создано.")
            logger.info(s"Создано хранилище: $fileName (зашифровано: $isEncrypted)")
            AppState.Opened(newSession)
          case Left(err) =>
            println(s"${err.message}")
            state

      // ОТКРЫТИЕ
      case (Command.Open, _) =>
        println("Введите путь к файлу хранилища:")
        val path = Paths.get(StdIn.readLine().trim)

        Storage.readSession(path, None) match
          case Right(sessionWithoutKey) =>
            if sessionWithoutKey.encrypted then
              println("Хранилище зашифровано. Введите ключ доступа:")
              val keyInput = StdIn.readLine().trim
              Crypto.parseKey(keyInput) match
                case Right(keySpec) =>
                  Storage.readSession(path, Some(keySpec)) match
                    case Right(finalSession) =>
                      println(s"Хранилище '${finalSession.name}' открыто.")
                      AppState.Opened(finalSession)
                    case Left(err) =>
                      println(s"${err.message}")
                      state
                case Left(err) =>
                  println(s"${err.message}")
                  state
            else
              println(s"Хранилище '${sessionWithoutKey.name}' открыто (без шифрования).")
              AppState.Opened(sessionWithoutKey)

          case Left(err) =>
            println(s"Ошибка чтения: ${err.message}")
            state

      // ИНФОРМАЦИЯ
      case (Command.Info, AppState.Opened(session)) =>
        println(s"Хранилище: ${session.name}")
        println(s"Шифрование: ${if session.encrypted then "включено" else "выключено"}")
        println(s"Источников: ${session.sources.size}")
        println(s"Версий: ${session.versions.size}")
        println(s"Файлов в базе: ${session.manifest.files.size}")
        state

      // ИСТОЧНИКИ
      case (Command.Sources, AppState.Opened(session)) =>
        println("Текущие источники:")
        session.sources.zipWithIndex.foreach { case (s, idx) =>
          val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
          println(s"${idx + 1}. ${s.path} [$status]")
        }

        println("\nМеню управления:")
        println("1. Добавить новый")
        println("2. Отключить/Включить")
        println("3. Удалить")
        println("0. Назад")
        print("Выберите действие: ")

        val updatedSources = StdIn.readLine().trim match
          case "1" =>
            println("Введите путь к папке:")
            val p = Paths.get(StdIn.readLine().trim)
            val newId = SourceId(session.sources.map(_.id.value).maxOption.getOrElse(0L) + 1L)
            session.sources :+ Source(newId, p)
          case "2" =>
            print("Введите номер источника для переключения: ")
            StdIn.readLine().trim.toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              session.sources.map(s => if s.id == src.id then s.copy(enabled = !s.enabled) else s)
            }.getOrElse(session.sources)
          case "3" =>
            print("Введите номер источника для удаления: ")
            StdIn.readLine().trim.toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              session.sources.filterNot(_.id == src.id)
            }.getOrElse(session.sources)
          case _ => session.sources

        if updatedSources != session.sources then
          val updated = session.copy(sources = updatedSources)
          Storage.saveSession(updated, Map.empty, session.key) match
            case Right(_) =>
              println("Источники обновлены и сохранены.")
              updated.sources.zipWithIndex.foreach { case (s, idx) =>
                val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
                println(s"  ${idx + 1}. ${s.path} [$status]")
              }
            case Left(err) => println(s"Ошибка сохранения: ${err.message}")
          AppState.Opened(updated)
        else state

      // СКАНИРОВАНИЕ
      case (Command.Scan, AppState.Opened(session)) =>
        if session.sources.isEmpty then
          println("Сначала добавьте источники (пункт 4).")
          state
        else
          println("Сканирование...")
          val scanner = Scanner(config.hashAlgorithm)
          val result = scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result, session.sources)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            println("Изменений не обнаружено.")
          else
            println(s"Изменения: +${stats.added} новых, ~${stats.modified} измененных, -${stats.removed} удаленных.")
          state

      // БЭКАП
      case (Command.Backup, AppState.Opened(session)) =>
        if session.sources.isEmpty then
          println("Нет источников для копирования.")
          state
        else
          println("Сканирование перед копированием...")
          val scanner = Scanner(config.hashAlgorithm)
          val result = scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result, session.sources)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            println("Нет новых данных для сохранения.")
            state
          else
            println(s"Обнаружено: +${stats.added}, ~${stats.modified}, -${stats.removed} файлов.")

            if config.confirmBeforeBackup then
              print("Добавить изменения в хранилище? (y/n): ")
              if StdIn.readLine().trim.toLowerCase != "y" then return state

            val filesToAdd = changes.collect {
              case Change.Added(entry) => entry
              case Change.Modified(_, current) => current
            }.flatMap { entry =>
              session.sources.find(_.id == entry.sourceId).map(src => (entry.hash, src.path.resolve(entry.relPath.value)))
            }.toMap

            val prevFiles = session.manifest.files
            val removedKeys = changes.collect { case Change.Removed(e) => (e.sourceId, e.relPath) }.toSet
            val addedEntries = changes.collect { case Change.Added(e) => (e.sourceId, e.relPath) -> e }.toMap
            val modifiedEntries = changes.collect { case Change.Modified(_, curr) => (curr.sourceId, curr.relPath) -> curr }.toMap

            val newFilesMap = (prevFiles -- removedKeys) ++ addedEntries ++ modifiedEntries
            val newVersionId = VersionId(session.manifest.version.value + 1)
            val newManifest = Manifest(newVersionId, Instant.now(), newFilesMap)

            val newRecord = VersionRecord(
              id = newVersionId,
              createdAt = newManifest.createdAt,
              added = stats.added,
              modified = stats.modified,
              removed = stats.removed,
              result = if result.problems.isEmpty then BackupOutcome.Success else BackupOutcome.Partial,
              comment = None,
              manifest = newManifest
            )

            val newSession = session.copy(
              manifest = newManifest,
              versions = (newRecord +: session.versions).take(config.maximumVersionsPerFile)
            )

            println("Запись данных в хранилище...")
            Storage.saveSession(newSession, filesToAdd, session.key) match
              case Right(_) =>
                println(s"Бэкап завершен. Версия: ${newVersionId.value}")
                AppState.Opened(newSession)
              case Left(err) =>
                println(s"Ошибка записи: ${err.message}")
                state

      // ВЕРСИИ
      case (Command.Versions, AppState.Opened(session)) =>
        if session.versions.isEmpty then
          println("Версий пока нет.")
          state
        else
          println("Доступные версии хранилища:")
          session.versions.foreach { v =>
            val dateStr = v.createdAt.toString.take(19).replace("T", " ")
            val filesCount = v.manifest.files.size
            println(f"Версия ${v.id.value.toInt}%3d | $dateStr%s | Файлов: $filesCount%3d | +${v.added}%d ~${v.modified}%d -${v.removed}%d")
          }
          state

      // ВОССТАНОВЛЕНИЕ
      case (Command.Restore, AppState.Opened(session)) =>
        if session.versions.isEmpty then
          println("Нет сохраненных версий.")
          state
        else
          println("Доступные версии:")
          println("  0 - Восстановить последнюю (текущую)")
          session.versions.foreach(v => println(s"  ${v.id.value.toInt} - версия от ${v.createdAt.toString.take(19)}"))

          print("Введите номер версии для восстановления: ")
          val vChoice = StdIn.readLine().trim

          val manifestToRestore = if vChoice == "0" then
            Some(session.manifest)
          else
            vChoice.toIntOption.flatMap(id => session.versions.find(_.id.value == id).map(_.manifest))

          manifestToRestore match
            case None =>
              println("Неверный номер версии.")
              state
            case Some(targetManifest) =>
              println("Введите каталог назначения (например, restored):")
              val destRoot = Paths.get(StdIn.readLine().trim)

              val (restoredCount, skippedCount) = targetManifest.files.values.foldLeft((0, 0)) {
                case ((restored, skipped), entry) =>
                  val destPath = destRoot.resolve(entry.relPath.value)
                  if Files.exists(destPath) && config.confirmBeforeRestoreOverwrite then {
                    print(s"'${entry.relPath.value}' существует. Перезаписать? (y/n): ")
                    if StdIn.readLine().trim.toLowerCase != "y" then (restored, skipped + 1)
                    else doRestore(session, entry, destPath, restored, skipped)
                  } else {
                    doRestore(session, entry, destPath, restored, skipped)
                  }
              }
              println(s"Восстановлено: $restoredCount, Пропущено: $skippedCount.")
              state

      // ПРОВЕРКА
      case (Command.Verify, AppState.Opened(session)) =>
        println("Проверка целостности...")
        val (checked, errors) = session.manifest.files.values.foldLeft((0, 0)) {
          case ((c, e), entry) =>
            val tempDest = Files.createTempFile("verify_", ".tmp")
            Storage.extractFile(session.storageFile, entry.hash, tempDest, session.key) match
              case Right(_) =>
                Files.deleteIfExists(tempDest)
                (c + 1, e)
              case Left(err) =>
                Files.deleteIfExists(tempDest)
                println(s"Повреждение: ${entry.relPath.value} - ${err.message}")
                (c + 1, e + 1)
        }
        if errors == 0 then println(s"Целостность в порядке. Проверено: $checked.")
        else println(s"Ошибки: $errors из $checked.")
        state

      case (_, AppState.NoStorage) =>
        println("Сначала откройте или создайте хранилище.")
        state

      case _ =>
        println("Команда не реализована.")
        state

  private def doRestore(session: StorageSession, entry: FileEntry, destPath: Path, restored: Int, skipped: Int): (Int, Int) =
    Storage.extractFile(session.storageFile, entry.hash, destPath, session.key) match
      case Right(_) => (restored + 1, skipped)
      case Left(err) =>
        println(s"Ошибка ${entry.relPath.value}: ${err.message}")
        (restored, skipped + 1)