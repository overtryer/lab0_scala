import java.nio.file.{Files, Path, Paths}

class RestoreHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        if session.versions.isEmpty then
          ctx.ui.println("Версий пока нет.")
          state
        else
          ctx.ui.println("Доступные версии:")
          ctx.ui.println("  0 - Последняя (текущая)")
          session.versions.foreach(v => ctx.ui.println(s"  ${v.id.value.toInt} - ${v.createdAt.toString.take(19)}"))

          ctx.ui.print("Номер версии: ")
          val vChoice = ctx.ui.readLineTrimmed()

          val manifestToRestore = if vChoice == "0" then
            Some(session.manifest)
          else
            vChoice.toIntOption.flatMap(id => session.versions.find(_.id.value == id).map(_.manifest))

          manifestToRestore match
            case None =>
              ctx.ui.println("Неверный номер версии.")
              ctx.logger.warn("Ошибка восстановления: выбран неверный номер версии")
              state
            case Some(targetManifest) =>
              ctx.ui.println("Каталог назначения:")
              val destRoot = Paths.get(ctx.ui.readLineTrimmed())
              ctx.logger.info(s"Начало восстановления в $destRoot")

              val (restoredCount, skippedCount) = targetManifest.files.values.foldLeft((0, 0)) {
                case ((restored, skipped), entry) =>
                  val destPath = destRoot.resolve(entry.relPath.value)
                  if Files.exists(destPath) && ctx.config.confirmBeforeRestoreOverwrite then {
                    ctx.ui.print(s"'${entry.relPath.value}' существует. Перезаписать? (y/n): ")
                    if ctx.ui.readLineTrimmed().toLowerCase != "y" then
                      ctx.logger.info(s"Восстановление пропущено: ${entry.relPath.value}")
                      (restored, skipped + 1)
                    else doRestore(session, entry, destPath, restored, skipped, ctx)
                  } else {
                    doRestore(session, entry, destPath, restored, skipped, ctx)
                  }
              }
              ctx.ui.println(s"Восстановлено: $restoredCount, Пропущено: $skippedCount")
              ctx.logger.info(s"Восстановление завершено. Успешно: $restoredCount, Пропущено: $skippedCount")
              state

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state

  private def doRestore(session: StorageSession, entry: FileEntry, destPath: Path, restored: Int, skipped: Int, ctx: AppContext): (Int, Int) =
    ctx.storage.extractFile(session.storageFile, entry.hash, destPath, session.key) match
      case Right(_) =>
        ctx.logger.info(s"Восстановлен файл: ${entry.relPath.value}")
        (restored + 1, skipped)
      case Left(err) =>
        ctx.ui.println(s"Ошибка ${entry.relPath.value}: ${err.message}")
        ctx.logger.error(s"Не удалось восстановить ${entry.relPath.value}: ${err.message}")
        (restored, skipped + 1)