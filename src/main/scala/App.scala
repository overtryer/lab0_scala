object App:

  def run(config: AppConfig, logger: Logger, ui: Ui): Unit =
    val scanner = new Scanner(config.hashAlgorithm)
    val ctx = AppContext(config, logger, ui, new ZipStorageService, scanner)
    val handlers: Map[Command, CommandHandler] = Map(
      Command.Info -> new InfoHandler,
      Command.Versions -> new VersionsHandler,
      Command.Scan -> new ScanHandler,
      Command.Reload -> new ReloadHandler,
      Command.Sources -> new SourcesHandler,
      Command.Open -> new OpenHandler,
      Command.Create -> new CreateHandler,
      Command.Verify -> new VerifyHandler,
      Command.Restore -> new RestoreHandler,
      Command.Backup -> new BackupHandler
    )
    var state: AppState = AppState.NoStorage
    var running = true

    while running do
      printMenu(ui)
      ui.readLine() match
        case None => running = false
        case Some(line) =>
          Command.parse(line) match
            case None =>
              ui.println("Некорректный пункт меню.")
              logger.warn("Введен неизвестный пункт меню")
            case Some(Command.Exit) =>
              ui.println("До свидания.")
              running = false
            case Some(cmd) => state = handleCommand(cmd, state, ctx, handlers)

  private def handleCommand(cmd: Command, state: AppState, ctx: AppContext, handlers: Map[Command, CommandHandler]): AppState =
    handlers.get(cmd) match
      case Some(handler) => handler.execute(state, ctx)
      case None =>
        ctx.ui.println("Команда не реализована.")
        ctx.logger.warn("Команда не реализована")
        state