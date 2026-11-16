class ScanHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        if session.sources.isEmpty then
          ctx.ui.println("Сначала добавьте источники.")
        else
          ctx.ui.println("Сканирование...")
          ctx.logger.info("Начало сканирования изменений")
          val result = ctx.scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result, session.sources)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            ctx.ui.println("Изменений не обнаружено.")
            ctx.logger.info("Сканирование завершено. Изменений нет.")
          else
            ctx.ui.println(s"Изменения: +${stats.added}, ~${stats.modified}, -${stats.removed}, пер.: ${stats.renamed}")
            ctx.logger.info(s"Сканирование завершено. Изменения: +${stats.added}, ~${stats.modified}, -${stats.removed}, пер.: ${stats.renamed}")
        state
      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state