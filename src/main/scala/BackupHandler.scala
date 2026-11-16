import java.nio.file.Files
import java.time.Instant

class BackupHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        if session.sources.isEmpty then
          ctx.ui.println("Нет источников.")
          state
        else
          ctx.ui.println("Сканирование перед копированием...")
          ctx.logger.info("Начало резервного копирования")
          val result = ctx.scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result, session.sources)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            ctx.ui.println("Нет новых данных для сохранения.")
            ctx.logger.info("Копирование отменено. Нет изменений.")
            state
          else
            ctx.ui.println(s"Обнаружено: +${stats.added}, ~${stats.modified}, -${stats.removed}, пер.: ${stats.renamed}")

            val usableSpace =
              try Files.getFileStore(session.storageFile).getUsableSpace
              catch { case _: Throwable => Long.MaxValue }
            if stats.newBytes > usableSpace then
              ctx.ui.println("Недостаточно свободного места на диске.")
              ctx.logger.error("Ошибка копирования: недостаточно места на диске")
              return state

            if ctx.config.confirmBeforeBackup then
              ctx.ui.print("Продолжить? (y/n): ")
              if ctx.ui.readLineTrimmed().toLowerCase != "y" then
                ctx.logger.info("Копирование отменено пользователем")
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

            val now = Instant.now()
            val unavailableIds = result.problems.collect { case Change.SourceUnavailable(sid, _) => sid }.toSet
            val updatedSources = session.sources.map { s =>
              if s.enabled && !unavailableIds.contains(s.id) then s.copy(lastBackup = Some(now)) else s
            }

            val newSession = session.copy(
              manifest = newManifest,
              versions = newRecord +: session.versions,
              sources = updatedSources
            )

            ctx.ui.println("Запись данных...")
            ctx.storage.saveSession(newSession, filesToAdd, session.key, ctx.config) match
              case Right(_) =>
                val successCount = stats.added + stats.modified + stats.renamed
                val failCount = result.problems.size
                ctx.ui.println(s"Резервное копирование завершено. Версия: ${newVersionId.value}")
                ctx.logger.info(s"Копирование завершено. Версия: ${newVersionId.value}. Успешно: $successCount, Проблемных: $failCount")
                AppState.Opened(newSession)
              case Left(err) =>
                ctx.ui.println(s"Ошибка записи: ${err.message}")
                ctx.logger.error(s"Ошибка копирования: ${err.message}")
                state

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state