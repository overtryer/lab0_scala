trait Ui:
  def print(s: String): Unit
  def println(s: String = ""): Unit
  def readLine(): Option[String]
  def readLineTrimmed(): String = readLine().getOrElse("").trim

class ConsoleUi extends Ui:
  def print(s: String): Unit = scala.Console.print(s)
  def println(s: String = ""): Unit = scala.Console.println(s)
  def readLine(): Option[String] = Option(scala.io.StdIn.readLine())