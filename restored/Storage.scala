import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import java.io.FileOutputStream
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

object Storage:

  // --- КОНВЕРТАЦИЯ В JSON И ОБРАТНО ---

  private def entryToJson(e: FileEntry): ujson.Obj = ujson.Obj(
    "sourceId" -> e.sourceId.value.toDouble,
    "relPath" -> e.relPath.value,
    "size" -> e.size.toDouble,
    "hash" -> e.hash.hex,
    "mtime" -> e.mtime.toString
  )

  private def entryFromJson(j: ujson.Value): FileEntry =
    val obj = j.obj
    FileEntry(
      sourceId = SourceId(obj("sourceId").num.toLong),
      relPath = RelPath(obj("relPath").str),
      size = obj("size").num.toLong,
      hash = Hash(obj("hash").str),
      mtime = java.time.Instant.parse(obj("mtime").str)
    )

  // ЭТОТ МЕТОД ДОЛЖЕН БЫТЬ НА УРОВНЕ ОБЪЕКТА, А НЕ ВНУТРИ ДРУГОГО МЕТОДА
  private def sourceToJson(s: Source): ujson.Obj = ujson.Obj(
    "id" -> s.id.value.toDouble,
    "path" -> s.path.toString,
    "recursive" -> s.recursive,
    "exclusions" -> ujson.Arr.from(s.exclusions.map(_.pattern)),
    "enabled" -> s.enabled,
    "lastBackup" -> s.lastBackup.map(v => ujson.Str(v.toString)).getOrElse(ujson.Null)
  )

  private def sourceFromJson(j: ujson.Value): Source =
    val o = j.obj
    Source(
      id = SourceId(o("id").num.toLong),
      path = java.nio.file.Paths.get(o("path").str),
      recursive = o("recursive").bool,
      exclusions = o("exclusions").arr.map(g => Glob(g.str)).toList,
      enabled = o("enabled").bool,
      lastBackup = if (o("lastBackup").isNull) None else Some(java.time.Instant.parse(o("lastBackup").str))
    )

  private def versionToJson(v: VersionRecord): ujson.Obj = ujson.Obj(
    "id" -> v.id.value.toDouble,
    "createdAt" -> v.createdAt.toString,
    "added" -> v.added.toDouble,
    "modified" -> v.modified.toDouble,
    "removed" -> v.removed.toDouble,
    "result" -> (v.result match {
      case BackupOutcome.Success => "success"
      case BackupOutcome.Partial => "partial"
      case BackupOutcome.Failed(m) => s"failed:$m"
      case BackupOutcome.Cancelled => "cancelled"
    }),
    "comment" -> v.comment.map(c => ujson.Str(c)).getOrElse(ujson.Null)
  )

  private def versionFromJson(j: ujson.Value): VersionRecord =
    val o = j.obj
    val res = o("result").str match {
      case "success" => BackupOutcome.Success
      case "partial" => BackupOutcome.Partial
      case "cancelled" => BackupOutcome.Cancelled
      case s if s.startsWith("failed:") => BackupOutcome.Failed(s.substring(7))
      case _ => BackupOutcome.Success
    }
    VersionRecord(
      id = VersionId(o("id").num.toLong),
      createdAt = java.time.Instant.parse(o("createdAt").str),
      added = o("added").num.toInt,
      modified = o("modified").num.toInt,
      removed = o("removed").num.toInt,
      result = res,
      comment = if (o("comment").isNull) None else Some(o("comment").str)
    )

  private def manifestToJson(m: Manifest): ujson.Obj = ujson.Obj(
    "version" -> m.version.value.toDouble,
    "createdAt" -> m.createdAt.toString,
    "files" -> ujson.Arr.from(m.files.values.map(entryToJson))
  )

  private def manifestFromJson(j: ujson.Value): Manifest =
    val obj = j.obj
    val filesList = obj("files").arr.map(entryFromJson).toList
    val filesMap = filesList.map(e => (e.sourceId, e.relPath) -> e).toMap
    Manifest(
      version = VersionId(obj("version").num.toLong),
      createdAt = java.time.Instant.parse(obj("createdAt").str),
      files = filesMap
    )

  private def sessionToJson(s: StorageSession): ujson.Obj = ujson.Obj(
    "name" -> s.name,
    "encrypted" -> s.encrypted,
    "manifest" -> manifestToJson(s.manifest),
    "versions" -> ujson.Arr.from(s.versions.map(versionToJson)),
    "sources" -> ujson.Arr.from(s.sources.map(sourceToJson))
  )

  private def sessionFromJson(j: ujson.Value): StorageSession =
    val o = j.obj
    StorageSession(
      storageFile = null, // Заполним позже
      name = o("name").str,
      encrypted = o("encrypted").bool,
      manifest = manifestFromJson(o("manifest")),
      versions = o("versions").arr.map(versionFromJson).toList,
      sources = o("sources").arr.map(sourceFromJson).toList
    )

  // --- ОПЕРАЦИИ ---

  def createNew(path: Path, session: StorageSession): Either[AppError, Unit] =
    if Files.exists(path) then Left(AppError.StorageAlreadyExists(path.toString))
    else
      Try {
        Using.resource(ZipOutputStream(FileOutputStream(path.toFile))) { zos =>
          zos.putNextEntry(ZipEntry("session.json"))
          val jsonStr = sessionToJson(session.copy(storageFile = path)).render(indent = 2)
          zos.write(jsonStr.getBytes("UTF-8"))
          zos.closeEntry()
        }
      }.toEither.left.map(e => AppError.StorageIoError(s"Не удалось создать: ${e.getMessage}"))

  def readSession(path: Path): Either[AppError, StorageSession] =
    if !Files.exists(path) then Left(AppError.StorageNotFound(path.toString))
    else
      Try {
        Using.resource(ZipFile(path.toFile)) { zip =>
          val entry = Option(zip.getEntry("session.json"))
            .getOrElse(throw new RuntimeException("В архиве нет session.json"))
          Using.resource(zip.getInputStream(entry)) { is =>
            val content = new String(is.readAllBytes(), "UTF-8")
            sessionFromJson(ujson.read(content)).copy(storageFile = path)
          }
        }
      }.toEither.left.map(e => AppError.StorageCorrupted(e.getMessage))

  def saveSession(session: StorageSession, filesToAdd: Map[Hash, Path]): Either[AppError, Unit] =
    val containerPath = session.storageFile
    val tempPath = containerPath.resolveSibling(containerPath.getFileName.toString + ".tmp")

    Try {
      Using.resources(
        ZipFile(containerPath.toFile),
        ZipOutputStream(FileOutputStream(tempPath.toFile))
      ) { (oldZip, zos) =>
        // 1. Копируем старые блобы
        oldZip.entries().asScala.toList.filter(_.getName.startsWith("blobs/")).foreach { entry =>
          zos.putNextEntry(ZipEntry(entry.getName))
          Using.resource(oldZip.getInputStream(entry)) { is => zos.write(is.readAllBytes()) }
          zos.closeEntry()
        }
        // 2. Добавляем новые
        filesToAdd.foreach { case (hash, filePath) =>
          val entryName = s"blobs/${hash.hex}"
          if oldZip.getEntry(entryName) == null then
            zos.putNextEntry(ZipEntry(entryName))
            Files.copy(filePath, zos)
            zos.closeEntry()
        }
        // 3. Пишем новую сессию
        zos.putNextEntry(ZipEntry("session.json"))
        zos.write(sessionToJson(session).render(indent = 2).getBytes("UTF-8"))
        zos.closeEntry()
      }
      Files.move(tempPath, containerPath, StandardCopyOption.REPLACE_EXISTING)
      ()
    }.toEither.left.map { e =>
      Files.deleteIfExists(tempPath)
      AppError.StorageIoError(s"Ошибка записи: ${e.getMessage}")
    }