import java.time.Instant

class InfoHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        ctx.ui.println(s"Хранилище: ${session.name}")
        ctx.ui.println(s"Шифрование: ${if session.encrypted then "включено" else "выключено"}")
        ctx.ui.println(s"Источников: ${session.sources.size}")
        val lastBackup = session.sources.flatMap(_.lastBackup).maxOption
        ctx.ui.println(s"Последняя резервная копия: ${lastBackup.map(formatMinute).getOrElse("—")}")
        ctx.ui.println(s"Версий: ${session.versions.size}")
        ctx.ui.println(s"Файлов в базе: ${session.manifest.files.size}")
        state
      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state

  private def formatMinute(i: Instant): String = i.toString.take(16).replace("T", " ")