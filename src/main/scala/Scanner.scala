import java.nio.file.{Files, Path}
import java.nio.file.attribute.BasicFileAttributes
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*
import scala.util.Try

case class ScanResult(
                       files: Map[(SourceId, RelPath), FileEntry],
                       problems: List[Change]
                     )

class Scanner(hashAlgorithm: String):

  def scan(sources: List[Source]): ScanResult =
    val results = sources.filter(_.enabled).map(scanSource)
    val allFiles = results.flatMap(_._1).map(e => (e.sourceId, e.relPath) -> e).toMap
    val allProblems = results.flatMap(_._2)
    ScanResult(allFiles, allProblems)

  private def scanSource(source: Source): (List[FileEntry], List[Change]) =
    if !Files.exists(source.path) then
      (Nil, List(Change.SourceUnavailable(source.id, source.path)))
    else
      val pathsTry = Try {
        val stream = if source.recursive then Files.walk(source.path) else Files.list(source.path)
        try stream.iterator().asScala.toList
        finally stream.close()
      }

      pathsTry match
        case scala.util.Failure(e) =>
          (Nil, List(Change.Unreadable(source.id, RelPath(""), s"Error: ${e.getMessage}")))

        case scala.util.Success(paths) =>
          val validFiles = paths
            .filter(Files.isRegularFile(_))
            .filterNot { p =>
              val rel = source.path.relativize(p)
              rel.iterator().asScala.map(_.toString).exists(source.isExcluded)
            }

          val entriesOrErrors = validFiles.map { p =>
            val rel = RelPath(source.path.relativize(p).toString)
            Try {
              val attrs = Files.readAttributes(p, classOf[BasicFileAttributes])
              val hash = hashFile(p)
              FileEntry(source.id, rel, attrs.size(), hash, attrs.lastModifiedTime().toInstant)
            }.toEither.left.map(e => Change.Unreadable(source.id, rel, e.getMessage))
          }

          val files = entriesOrErrors.collect { case Right(entry) => entry }
          val errors = entriesOrErrors.collect { case Left(change) => change }

          (files, errors)

  def hashFile(path: Path): Hash =
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