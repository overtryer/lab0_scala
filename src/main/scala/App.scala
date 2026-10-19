import java.time.Instant
import java.nio.file.{Files, Path, Paths}
import scala.io.StdIn
import scala.jdk.CollectionConverters.*
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
          case None =>
            println("Некорректный пункт меню.")
            logger.warn("Введен неизвестный пункт меню")
          case Some(Command.Exit) =>
            println("До свидания.")
            running = false
          case Some(cmd) => state = handleCommand(cmd, state, config, logger)

  private def handleCommand(cmd: Command, state: AppState, config: AppConfig, logger: Logger): AppState =
    (cmd, state) match
      case (Command.Create, _) =>
        logger.info("Начало создания хранилища")
        val roots = java.nio.file.FileSystems.getDefault.getRootDirectories.asScala.toList

        println("Доступные устройства:")
        roots.zipWithIndex.foreach { case (root, idx) => println(s"  ${idx + 1}. $root") }
        print("Выберите устройство (номер): ")
        val driveIdx = StdIn.readLine().trim.toIntOption.getOrElse(0) - 1

        if driveIdx < 0 || driveIdx >= roots.size then
          println("Неверный выбор устройства.")
          return state

        val selectedRoot = roots(driveIdx)

        println("Введите имя файла хранилища (например, backup.store):")
        val fileName = StdIn.readLine().trim
        if fileName.isEmpty then return state
        val path = selectedRoot.resolve(fileName)

        println("Введите название хранилища:")
        val storeName = StdIn.readLine().trim
        if storeName.isEmpty then return state

        println("Использовать шифрование? (y/n):")
        val isEncrypted = StdIn.readLine().trim.toLowerCase == "y"

        var keyOpt: Option[SecretKeySpec] = None
        if isEncrypted then
          println("Способ хранения ключа: 1 - в файл, 2 - показать пользователю")
          val choice = StdIn.readLine().trim
          val keyStr = Crypto.generateKey()
          keyOpt = Crypto.parseKey(keyStr.value).toOption

          if choice == "1" then
            val keyDir = Paths.get(config.keyDirectory)
            Files.createDirectories(keyDir)
            val keyFile = keyDir.resolve(s"${path.getFileName}.key")
            Files.writeString(keyFile, keyStr.value)
            println(s"Ключ сохранен в: ${keyFile.toAbsolutePath}")
            logger.info(s"Ключ шифрования сохранен в файл: $keyFile")
          else
            println(s"КЛЮЧ: ${keyStr.value}")
            println("Подтвердите, что ключ сохранен (y):")
            if StdIn.readLine().trim.toLowerCase != "y" then
              logger.info("Создание хранилища отменено пользователем")
              return state

        println("Добавьте первоначальные источники (пути к папкам). Оставьте пустым для завершения:")
        var sourcesList = List.empty[Source]
        var adding = true
        while adding do
          print(s"Путь к источнику ${sourcesList.size + 1}: ")
          val p = StdIn.readLine().trim
          if p.isEmpty then adding = false
          else
            val newId = SourceId(sourcesList.map(_.id.value).maxOption.getOrElse(0L) + 1L)
            sourcesList = sourcesList :+ Source(newId, Paths.get(p))

        val newSession = StorageSession(
          storageFile = path, name = storeName, encrypted = isEncrypted,
          manifest = Manifest(VersionId(0), Instant.now(), Map.empty),
          versions = Nil, sources = sourcesList, key = keyOpt
        )

        Storage.createNew(path, newSession, keyOpt) match
          case Right(_) =>
            println(s"Хранилище создано по пути: ${path.toAbsolutePath}")
            logger.info(s"Создано хранилище: $path, Источников: ${sourcesList.size}, Шифрование: $isEncrypted")
            AppState.Opened(newSession)
          case Left(err) =>
            println(s"Ошибка: ${err.message}")
            logger.error(s"Ошибка создания хранилища: ${err.message}")
            state

      case (Command.Open, _) =>
        logger.info("Попытка открытия хранилища")
        println("Введите путь к файлу хранилища:")
        val path = Paths.get(StdIn.readLine().trim)

        Storage.readSession(path, None) match
          case Right(sessionWithoutKey) =>
            if sessionWithoutKey.encrypted then
              println("Введите ключ доступа:")
              val keyInput = StdIn.readLine().trim
              Crypto.parseKey(keyInput) match
                case Right(keySpec) =>
                  Storage.readSession(path, Some(keySpec)) match
                    case Right(finalSession) =>
                      println(s"Хранилище '${finalSession.name}' открыто.")
                      logger.info(s"Хранилище успешно открыто: ${finalSession.name}")
                      AppState.Opened(finalSession)
                    case Left(err) =>
                      println(s"Ошибка: ${err.message}")
                      logger.warn(s"Не удалось открыть зашифрованное хранилище: ${err.message}")
                      state
                case Left(err) =>
                  println(s"Ошибка: ${err.message}")
                  logger.warn("Не удалось открыть хранилище: неверный формат ключа")
                  state
            else
              println(s"Хранилище '${sessionWithoutKey.name}' открыто.")
              logger.info(s"Хранилище успешно открыто: ${sessionWithoutKey.name}")
              AppState.Opened(sessionWithoutKey)

          case Left(err) =>
            println(s"Ошибка чтения: ${err.message}")
            logger.error(s"Не удалось открыть хранилище: ${err.message}")
            state

      case (Command.Info, AppState.Opened(session)) =>
        println(s"Хранилище: ${session.name}")
        println(s"Шифрование: ${if session.encrypted then "включено" else "выключено"}")
        println(s"Источников: ${session.sources.size}")
        println(s"Версий: ${session.versions.size}")
        println(s"Файлов в базе: ${session.manifest.files.size}")
        state

      case (Command.Sources, AppState.Opened(session)) =>
        println("Текущие источники:")
        session.sources.zipWithIndex.foreach { case (s, idx) =>
          val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
          println(s"${idx + 1}. ${s.path} [$status]")
        }

        println("\nУправление:")
        println("1. Добавить")
        println("2. Отключить/Включить")
        println("3. Удалить")
        println("4. Изменить настройки")
        println("0. Назад")
        print("Выбор: ")

        val updatedSources = StdIn.readLine().trim match
          case "1" =>
            println("Введите путь к папке:")
            val p = Paths.get(StdIn.readLine().trim)
            val newId = SourceId(session.sources.map(_.id.value).maxOption.getOrElse(0L) + 1L)
            logger.info(s"Добавлен новый источник: $p")
            session.sources :+ Source(newId, p)
          case "2" =>
            print("Введите номер источника: ")
            val res = StdIn.readLine().trim.toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              logger.info(s"Переключен источник: ${src.path}")
              session.sources.map(s => if s.id == src.id then s.copy(enabled = !s.enabled) else s)
            }.getOrElse(session.sources)
            res
          case "3" =>
            print("Введите номер источника: ")
            val res = StdIn.readLine().trim.toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              logger.info(s"Удален источник: ${src.path}")
              session.sources.filterNot(_.id == src.id)
            }.getOrElse(session.sources)
            res
          case "4" =>
            print("Введите номер источника: ")
            val res = StdIn.readLine().trim.toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              println(s"Включать вложенные каталоги? (y/n) (сейчас: ${src.recursive}):")
              val newRec = StdIn.readLine().trim.toLowerCase == "y"
              println(s"Исключения через запятую (сейчас: ${src.exclusions.map(_.pattern).mkString(", ")}):")
              val exStr = StdIn.readLine().trim
              val newExclusions = if exStr.isEmpty then Nil else exStr.split(",").map(s => Glob(s.trim)).toList
              logger.info(s"Изменены настройки источника: ${src.path}")
              session.sources.map(s => if s.id == src.id then s.copy(recursive = newRec, exclusions = newExclusions) else s)
            }.getOrElse(session.sources)
            res
          case _ => session.sources

        if updatedSources != session.sources then
          val updated = session.copy(sources = updatedSources)
          Storage.saveSession(updated, Map.empty, session.key, config) match
            case Right(_) =>
              println("Источники обновлены.")
              logger.info("Конфигурация источников сохранена")
              updated.sources.zipWithIndex.foreach { case (s, idx) =>
                val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
                println(s"  ${idx + 1}. ${s.path} [$status]")
              }
            case Left(err) =>
              println(s"Ошибка сохранения: ${err.message}")
              logger.error(s"Не удалось сохранить источники: ${err.message}")
          AppState.Opened(updated)
        else state

      case (Command.Scan, AppState.Opened(session)) =>
        if session.sources.isEmpty then
          println("Сначала добавьте источники.")
          state
        else
          println("Сканирование...")
          logger.info("Начало сканирования изменений")
          val scanner = Scanner(config.hashAlgorithm)
          val result = scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result, session.sources)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            println("Изменений не обнаружено.")
            logger.info("Сканирование завершено. Изменений нет.")
          else
            println(s"Изменения: +${stats.added}, ~${stats.modified}, -${stats.removed}, пер.: ${stats.renamed}")
            logger.info(s"Сканирование завершено. Изменения: +${stats.added}, ~${stats.modified}, -${stats.removed}, пер.: ${stats.renamed}")
          state

      case (Command.Backup, AppState.Opened(session)) =>
        if session.sources.isEmpty then
          println("Нет источников.")
          state
        else
          println("Сканирование перед копированием...")
          logger.info("Начало резервного копирования")
          val scanner = Scanner(config.hashAlgorithm)
          val result = scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result, session.sources)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            println("Нет новых данных для сохранения.")
            logger.info("Копирование отменено. Нет изменений.")
            state
          else
            println(s"Обнаружено: +${stats.added}, ~${stats.modified}, -${stats.removed}, пер.: ${stats.renamed}")

            val usableSpace = try Files.getFileStore(session.storageFile).getUsableSpace catch { case _: Throwable => Long.MaxValue }
            if stats.newBytes > usableSpace then {
              println("Недостаточно свободного места на диске.")
              logger.error("Ошибка копирования: недостаточно места на диске")
              return state
            }

            if config.confirmBeforeBackup then
              print("Продолжить? (y/n): ")
              if StdIn.readLine().trim.toLowerCase != "y" then
                logger.info("Копирование отменено пользователем")
                return state

            val filesToAdd = changes.collect {
              case Change.Added(entry) => entry
              case Change.Modified(_, current) => current
            }.flatMap { entry =>
              session.sources.find(_.id == entry.sourceId).map(src => (entry.hash, src.path.resolve(entry.relPath.value)))
            }.toMap

            val prevFiles = session.manifest.files
            val removedKeys = changes.collect { case Change.Removed(e) => (e.sourceId, e.relPath) }.toSet
            val addedEntries = changes.collect {
              case Change.Added(e) => (e.sourceId, e.relPath) -> e
              case Change.Renamed(_, curr) => (curr.sourceId, curr.relPath) -> curr
            }.toMap
            val modifiedEntries = changes.collect { case Change.Modified(_, curr) => (curr.sourceId, curr.relPath) -> curr }.toMap

            val newFilesMap = (prevFiles -- removedKeys) ++ addedEntries ++ modifiedEntries
            val newVersionId = VersionId(session.manifest.version.value + 1)
            val newManifest = Manifest(newVersionId, Instant.now(), newFilesMap)

            val newRecord = VersionRecord(
              id = newVersionId, createdAt = newManifest.createdAt,
              added = stats.added + stats.renamed, modified = stats.modified, removed = stats.removed,
              result = if result.problems.isEmpty then BackupOutcome.Success else BackupOutcome.Partial,
              comment = None, manifest = newManifest
            )

            val newSession = session.copy(
              manifest = newManifest,
              versions = newRecord +: session.versions
            )

            println("Запись данных...")
            Storage.saveSession(newSession, filesToAdd, session.key, config) match
              case Right(_) =>
                val successCount = stats.added + stats.modified + stats.renamed
                val failCount = result.problems.size
                println(s"Резервное копирование завершено. Версия: ${newVersionId.value}")
                logger.info(s"Копирование завершено. Версия: ${newVersionId.value}. Успешно: $successCount, Проблемных: $failCount")
                AppState.Opened(newSession)
              case Left(err) =>
                println(s"Ошибка записи: ${err.message}")
                logger.error(s"Ошибка копирования: ${err.message}")
                state

      case (Command.Versions, AppState.Opened(session)) =>
        if session.versions.isEmpty then
          println("Версий пока нет.")
          state
        else
          println("Доступные версии:")
          session.versions.foreach { v =>
            val dateStr = v.createdAt.toString.take(19).replace("T", " ")
            val filesCount = v.manifest.files.size
            println(f"Версия ${v.id.value.toInt}%3d | $dateStr%s | Файлов: $filesCount%3d | +${v.added}%d ~${v.modified}%d -${v.removed}%d")
          }
          state

      case (Command.Restore, AppState.Opened(session)) =>
        if session.versions.isEmpty then
          println("Версий пока нет.")
          state
        else
          println("Доступные версии:")
          println("  0 - Последняя (текущая)")
          session.versions.foreach(v => println(s"  ${v.id.value.toInt} - ${v.createdAt.toString.take(19)}"))

          print("Номер версии: ")
          val vChoice = StdIn.readLine().trim

          val manifestToRestore = if vChoice == "0" then
            Some(session.manifest)
          else
            vChoice.toIntOption.flatMap(id => session.versions.find(_.id.value == id).map(_.manifest))

          manifestToRestore match
            case None =>
              println("Неверный номер версии.")
              logger.warn("Ошибка восстановления: выбран неверный номер версии")
              state
            case Some(targetManifest) =>
              println("Каталог назначения:")
              val destRoot = Paths.get(StdIn.readLine().trim)
              logger.info(s"Начало восстановления в $destRoot")

              val (restoredCount, skippedCount) = targetManifest.files.values.foldLeft((0, 0)) {
                case ((restored, skipped), entry) =>
                  val destPath = destRoot.resolve(entry.relPath.value)
                  if Files.exists(destPath) && config.confirmBeforeRestoreOverwrite then {
                    print(s"'${entry.relPath.value}' существует. Перезаписать? (y/n): ")
                    if StdIn.readLine().trim.toLowerCase != "y" then
                      logger.info(s"Восстановление пропущено: ${entry.relPath.value}")
                      (restored, skipped + 1)
                    else doRestore(session, entry, destPath, restored, skipped, logger)
                  } else {
                    doRestore(session, entry, destPath, restored, skipped, logger)
                  }
              }
              println(s"Восстановлено: $restoredCount, Пропущено: $skippedCount")
              logger.info(s"Восстановление завершено. Успешно: $restoredCount, Пропущено: $skippedCount")
              state

      case (Command.Verify, AppState.Opened(session)) =>
        println("Проверка целостности...")
        logger.info("Начало проверки целостности")
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
                logger.error(s"Проверка не пройдена: ${entry.relPath.value} - ${err.message}")
                (c + 1, e + 1)
        }
        if errors == 0 then
          println(s"Целостность в порядке. Проверено: $checked")
          logger.info(s"Проверка целостности пройдена. Проверено: $checked")
        else
          println(s"Ошибок: $errors из $checked")
          logger.error(s"Проверка целостности завершилась с $errors ошибками")
        state

      case (Command.Reload, _) =>
        println("Перезагрузка конфигурации...")
        val configPath = Paths.get("src/main/scala/config.json")
        ConfigLoader.load(configPath) match
          case Right(_) =>
            println("Конфигурация перезагружена.")
            logger.info("Конфигурация перезагружена")
            state
          case Left(err) =>
            println(s"Ошибка перезагрузки: ${err.message}")
            logger.error(s"Не удалось перезагрузить конфигурацию: ${err.message}")
            state

      case (_, AppState.NoStorage) =>
        println("Сначала откройте или создайте хранилище.")
        logger.warn("Команда отклонена: хранилище не открыто")
        state

      case _ =>
        println("Команда не реализована.")
        logger.warn("Команда не реализована")
        state

  private def doRestore(session: StorageSession, entry: FileEntry, destPath: Path, restored: Int, skipped: Int, logger: Logger): (Int, Int) =
    Storage.extractFile(session.storageFile, entry.hash, destPath, session.key) match
      case Right(_) =>
        logger.info(s"Восстановлен файл: ${entry.relPath.value}")
        (restored + 1, skipped)
      case Left(err) =>
        println(s"Ошибка ${entry.relPath.value}: ${err.message}")
        logger.error(s"Не удалось восстановить ${entry.relPath.value}: ${err.message}")
        (restored, skipped + 1)