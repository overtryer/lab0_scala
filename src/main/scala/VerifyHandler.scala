import java.nio.file.Files

class VerifyHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        ctx.ui.println("Проверка целостности...")
        ctx.logger.info("Начало проверки целостности")
        val (checked, errors) = session.manifest.files.values.foldLeft((0, 0)) {
          case ((c, e), entry) =>
            val tempDest = Files.createTempFile("verify_", ".tmp")
            ctx.storage.extractFile(session.storageFile, entry.hash, tempDest, session.key) match
              case Right(_) =>
                try
                  val actualHash = ctx.scanner.hashFile(tempDest)
                  if actualHash != entry.hash then
                    ctx.ui.println(s"Повреждение: ${entry.relPath.value} - хеш не совпадает")
                    ctx.logger.error(s"Проверка не пройдена (hash mismatch): ${entry.relPath.value}")
                    (c + 1, e + 1)
                  else
                    (c + 1, e)
                catch
                  case ex: Exception =>
                    ctx.ui.println(s"Ошибка проверки: ${entry.relPath.value} - ${ex.getMessage}")
                    ctx.logger.error(s"Ошибка проверки файла ${entry.relPath.value}: ${ex.getMessage}")
                    (c + 1, e + 1)
                finally
                  Files.deleteIfExists(tempDest)
              case Left(err) =>
                Files.deleteIfExists(tempDest)
                ctx.ui.println(s"Повреждение: ${entry.relPath.value} - ${err.message}")
                ctx.logger.error(s"Проверка не пройдена: ${entry.relPath.value} - ${err.message}")
                (c + 1, e + 1)
        }
        if errors == 0 then
          ctx.ui.println(s"Целостность в порядке. Проверено: $checked")
          ctx.logger.info(s"Проверка целостности пройдена. Проверено: $checked")
        else
          ctx.ui.println(s"Ошибок: $errors из $checked")
          ctx.logger.error(s"Проверка целостности завершилась с $errors ошибками")
        state

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state