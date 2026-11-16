class VersionsHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        if session.versions.isEmpty then
          ctx.ui.println("Версий пока нет.")
        else
          ctx.ui.println("Доступные версии:")
          session.versions.foreach { v =>
            val dateStr = v.createdAt.toString.take(19).replace("T", " ")
            val filesCount = v.manifest.files.size
            ctx.ui.println(f"Версия ${v.id.value.toInt}%3d | $dateStr%s | Файлов: $filesCount%3d | +${v.added}%d ~${v.modified}%d -${v.removed}%d")
          }
        state
      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state