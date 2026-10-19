import java.nio.file.{Files, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.util.Try

// Результат работы сканера: успешно прочитанные файлы и список проблем
case class ScanResult(
                       files: Map[(SourceId, RelPath), FileEntry],
                       problems: List[Change]
                     )

class Scanner(hashAlgorithm: String):

  def scan(sources: List[Source]): ScanResult =
    // Оставляем только включенные источники и сканируем каждый
    val results = sources.filter(_.enabled).map(scanSource)

    // Собираем все файлы в один Map, а все проблемы — в один List
    val allFiles = results.flatMap(_._1).map(e => (e.sourceId, e.relPath) -> e).toMap
    val allProblems = results.flatMap(_._2)

    ScanResult(allFiles, allProblems)

  private def scanSource(source: Source): (List[FileEntry], List[Change]) =
    // 1. Проверяем, доступен ли источник
    if !Files.exists(source.path) then
      (Nil, List(Change.SourceUnavailable(source.id, source.path)))
    else
      // 2. Обходим директорию. Используем Try, так как Files.walk может выбросить исключение
      val pathsTry = Try {
        val stream = if source.recursive then Files.walk(source.path) else Files.list(source.path)
        try stream.iterator().asScala.toList // Превращаем Java Stream в Scala List
        finally stream.close()
      }

      pathsTry match
        case scala.util.Failure(e) =>
          (Nil, List(Change.Unreadable(source.id, RelPath(""), s"Ошибка обхода: ${e.getMessage}")))

        case scala.util.Success(paths) =>
          // 3. Фильтруем: только файлы (не папки) и не попадающие под исключения
          val validFiles = paths
            .filter(Files.isRegularFile(_))
            .filterNot { p =>
              // Получаем путь относительно корня источника (например, "Documents/cache/temp.txt")
              val rel = source.path.relativize(p)
              // Проверяем, не исключено ли имя самого файла или имена его родительских папок
              rel.iterator().asScala.map(_.toString).exists(source.isExcluded)
            }

          // 4. Для каждого файла пытаемся прочитать атрибуты и хеш
          val entriesOrErrors = validFiles.map { p =>
            val rel = RelPath(source.path.relativize(p).toString)
            // Оборачиваем в Try, чтобы ошибка чтения одного файла не убила весь процесс
            Try {
              val attrs = Files.readAttributes(p, classOf[BasicFileAttributes])
              val hash = hashFile(p)
              FileEntry(source.id, rel, attrs.size(), hash, attrs.lastModifiedTime().toInstant)
            }.toEither.left.map(e => Change.Unreadable(source.id, rel, e.getMessage))
          }

          // 5. Разделяем на успешные и проблемные (используя pattern matching)
          val files = entriesOrErrors.collect { case Right(entry) => entry }
          val errors = entriesOrErrors.collect { case Left(change) => change }

          (files, errors)

  /** Потоковое вычисление хеша (чтобы не загружать огромные файлы в память целиком) */
  private def hashFile(path: Path): Hash =
    val digest = MessageDigest.getInstance(hashAlgorithm)
    val stream = Files.newInputStream(path)
    try
      val buffer = new Array[Byte](8192)
      var read = stream.read(buffer)
      while read != -1 do
        digest.update(buffer, 0, read)
        read = stream.read(buffer)

      val bytes = digest.digest()
      Hash(bytes.map("%02x".format(_)).mkString)
    finally
      stream.close()