final case class AppContext(
                             var config: AppConfig,
                             logger: Logger,
                             ui: Ui,
                             storage: StorageService,
                             var scanner: Scanner
                           )

trait CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState