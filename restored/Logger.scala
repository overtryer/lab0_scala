import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

class Logger(logPath: String):
  private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  def info(message: String): Unit  = write("INFO", message)
  def warn(message: String): Unit  = write("WARN", message)
  def error(message: String): Unit = write("ERROR", message)

  private def write(level: String, message: String): Unit =
    val timestamp = LocalDateTime.now().format(formatter)
    val line = s"[$timestamp] [$level] $message\n"
    val path = Paths.get(logPath)

    // Эффект записи на диск оборачиваем в Try, чтобы падение лога не роняло программу
    Try {
      // Если указан путь с папками (например, logs/backup.log), создаем папку
      Option(path.getParent).foreach(Files.createDirectories(_))

      // CREATE - создаст файл, если нет. APPEND - допишет в конец.
      Files.writeString(path, line, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    }.failed.foreach { e =>
      System.err.println(s"Не удалось записать в лог: ${e.getMessage}")
    }