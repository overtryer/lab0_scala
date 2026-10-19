import scala.io.StdIn
import java.nio.file.Paths

// =============================================================
//  КОМАНДЫ И МЕНЮ
// =============================================================
enum Command(val title: String):
  case Create   extends Command("Создать резервное хранилище")
  case Open     extends Command("Открыть существующее хранилище")
  case Info     extends Command("Показать сведения о хранилище")
  case Sources  extends Command("Настроить список резервируемых файлов и каталогов")
  case Scan     extends Command("Проверить наличие изменений")
  case Backup   extends Command("Добавить обнаруженные изменения в хранилище")
  case Versions extends Command("Просмотреть сохранённые версии")
  case Restore  extends Command("Восстановить файлы")
  case Verify   extends Command("Проверить целостность хранилища")
  case Reload   extends Command("Перезагрузить конфигурацию")
  case Exit     extends Command("Завершить работу")

object Command:
  val menuItems: List[Command] = Command.values.toList.filterNot(_ == Command.Exit)
  val exitNumber: Int = 0
  def parse(input: String): Option[Command] =
    input.trim.toIntOption.flatMap { n =>
      if n == exitNumber then Some(Command.Exit)
      else menuItems.lift(n - 1)
    }

def printMenu(): Unit =
  println()
  println("=== Меню ===")
  Command.menuItems.zipWithIndex.foreach { case (cmd, i) => println(f"${i + 1}%2d. ${cmd.title}") }
  println(f"${Command.exitNumber}%2d. ${Command.Exit.title}")
  print("Выберите пункт: ")

// =============================================================
//  ТОЧКА ВХОДА
// =============================================================
@main def run(): Unit =
  println("==============================================")
  println("   Переносное резервное хранилище (ЛР0)")
  println("==============================================")

  val configPath = Paths.get("src/main/scala/config.json")

  ConfigLoader.load(configPath) match
    case Right(config) =>
      // Инициализируем логгер
      val logger = Logger(config.logFile)
      logger.info("Приложение запущено.")
      logger.info(s"Конфигурация загружена успешно. Алгоритм: ${config.hashAlgorithm}")

      // Выводим информацию на экран (как было раньше)
      println(s"✅ Конфиг загружен.")
      println(s"📝 Логи пишутся в: ${config.logFile}")
      println(s"🔐 Алгоритм хеширования: ${config.hashAlgorithm}")
      println("==============================================\n")

      // Запускаем интерактивное меню
      App.run(config, logger)

    case Left(error) =>
      println(s"❌ Ошибка конфига: ${error.message}")
      println("Приложение не может быть запущено без корректной конфигурации.")