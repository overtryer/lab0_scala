import scala.io.StdIn
import java.nio.file.Paths

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

@main def run(): Unit =
  println("==============================================")
  println("   Переносное резервное хранилище")
  println("==============================================")

  val configPath = Paths.get("src/main/scala/config.json")

  ConfigLoader.load(configPath) match
    case Right(config) =>
      val logger = Logger(config.logFile)
      logger.info("Приложение запущено")
      logger.info("Конфигурация загружена успешно")

      println(s"Конфиг загружен.")
      println(s"Логи пишутся в: ${config.logFile}")
      println(s"Алгоритм хеширования: ${config.hashAlgorithm}")
      println("==============================================\n")

      App.run(config, logger)
      logger.info("Приложение завершило работу")

    case Left(error) =>
      println(s"Ошибка конфига: ${error.message}")