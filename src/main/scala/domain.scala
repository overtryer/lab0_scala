import java.nio.file.Path
import java.time.Instant

opaque type SourceId = Long
object SourceId:
  def apply(v: Long): SourceId = v
  extension (s: SourceId) def value: Long = s

opaque type VersionId = Long
object VersionId:
  def apply(v: Long): VersionId = v
  extension (v: VersionId) def value: Long = v

final case class RelPath(value: String)

final case class Hash(hex: String)

final case class Secret(value: String):
  override def toString: String = "***"

final case class Glob(pattern: String):
  def matches(name: String): Boolean = Glob.matches(pattern, name)

object Glob:
  def matches(glob: String, name: String): Boolean =
    name.matches(globToRegex(glob))

  private def globToRegex(g: String): String =
    g.flatMap {
      case '*' => ".*"
      case '?' => "."
      case c if "\\^$.|+()[]{}".contains(c) => s"\\$c"
      case c => c.toString
    }

final case class Source(
                         id: SourceId,
                         path: Path,
                         recursive: Boolean = true,
                         exclusions: List[Glob] = Nil,
                         enabled: Boolean = true,
                         lastBackup: Option[Instant] = None
                       )

extension (s: Source)
  def isExcluded(name: String): Boolean =
    s.exclusions.exists(_.matches(name))

final case class FileEntry(
                            sourceId: SourceId,
                            relPath: RelPath,
                            size: Long,
                            hash: Hash,
                            mtime: Instant
                          )

final case class Manifest(
                           version: VersionId,
                           createdAt: Instant,
                           files: Map[(SourceId, RelPath), FileEntry]
                         )

object Manifest:
  def empty(version: VersionId, at: Instant): Manifest =
    Manifest(version, at, Map.empty)

sealed trait BackupOutcome
object BackupOutcome:
  case object Success extends BackupOutcome
  case object Partial extends BackupOutcome
  final case class Failed(message: String) extends BackupOutcome
  case object Cancelled extends BackupOutcome

final case class VersionRecord(
                                id: VersionId,
                                createdAt: Instant,
                                added: Int,
                                modified: Int,
                                removed: Int,
                                result: BackupOutcome,
                                comment: Option[String] = None,
                                manifest: Manifest
                              )

sealed trait Change
object Change:
  final case class Added(entry: FileEntry) extends Change
  final case class Modified(old: FileEntry, current: FileEntry) extends Change
  final case class Removed(old: FileEntry) extends Change
  final case class Renamed(old: FileEntry, current: FileEntry) extends Change
  final case class Unreadable(sourceId: SourceId, relPath: RelPath, reason: String) extends Change
  final case class SourceUnavailable(sourceId: SourceId, path: Path) extends Change

final case class ChangeStats(
                              added: Int = 0,
                              modified: Int = 0,
                              removed: Int = 0,
                              renamed: Int = 0,
                              unreadable: Int = 0,
                              unavailable: Int = 0,
                              newBytes: Long = 0L
                            ):
  def total: Int = added + modified + removed + renamed
  def isEmpty: Boolean =
    added + modified + removed + renamed + unreadable + unavailable == 0

object ChangeStats:
  def from(changes: List[Change]): ChangeStats =
    changes.foldLeft(ChangeStats()) { (acc, ch) =>
      ch match
        case Change.Added(e)                => acc.copy(added = acc.added + 1, newBytes = acc.newBytes + e.size)
        case Change.Modified(_, current)    => acc.copy(modified = acc.modified + 1, newBytes = acc.newBytes + current.size)
        case Change.Removed(_)              => acc.copy(removed = acc.removed + 1)
        case Change.Renamed(_, current)     => acc.copy(renamed = acc.renamed + 1, newBytes = acc.newBytes + current.size)
        case Change.Unreadable(_, _, _)     => acc.copy(unreadable = acc.unreadable + 1)
        case Change.SourceUnavailable(_, _) => acc.copy(unavailable = acc.unavailable + 1)
    }

sealed trait AppError:
  def message: String

object AppError:
  // конфигурация
  final case class ConfigParseError(detail: String) extends AppError:
    def message = s"Ошибка в файле конфигурации: $detail"
  case object LogPathMissing extends AppError:
    def message = "В конфигурации не указан путь к файлу журнала"
  final case class KeyDirUnavailable(path: String) extends AppError:
    def message = s"Каталог для ключей недоступен или не может быть создан: $path"
  final case class MaxVersionsInvalid(value: Int) extends AppError:
    def message = s"Максимальное количество версий должно быть >= 1 (получено $value)"
  final case class UnsupportedHash(algo: String) extends AppError:
    def message = s"Неподдерживаемый алгоритм хеширования: $algo"
  final case class BadExclusion(pattern: String) extends AppError:
    def message = s"Некорректное правило исключения: $pattern"

  // хранилище
  final case class StorageAlreadyExists(path: String) extends AppError:
    def message = s"Файл хранилища уже существует: $path"
  final case class StorageNotFound(path: String) extends AppError:
    def message = s"Файл хранилища не найден: $path"
  final case class StorageCorrupted(detail: String) extends AppError:
    def message = s"Хранилище повреждено: $detail"
  case object WrongKey extends AppError:
    def message = "Неверный ключ доступа"
  final case class KeyFileMissing(path: String) extends AppError:
    def message = s"Файл ключа не найден: $path"
  final case class StorageIoError(detail: String) extends AppError:
    def message = s"Ошибка чтения/записи хранилища: $detail"

  // устройства и источники
  final case class DeviceUnavailable(path: String) extends AppError:
    def message = s"Устройство недоступно: $path"
  final case class NotEnoughSpace(needBytes: Long) extends AppError:
    def message = s"Недостаточно свободного места (требуется $needBytes байт)"
  final case class SourceMissing(path: String) extends AppError:
    def message = s"Источник недоступен: $path"
  final case class FileUnreadable(path: String, reason: String) extends AppError:
    def message = s"Файл не читается: $path ($reason)"
  final case class FileChangedDuringCopy(path: String) extends AppError:
    def message = s"Файл изменился во время копирования: $path"

  // восстановление
  final case class RestoreDirUnavailable(path: String) extends AppError:
    def message = s"Каталог восстановления недоступен: $path"
  final case class RestoreFileExists(path: String) extends AppError:
    def message = s"Файл уже существует в каталоге назначения: $path"

  // прочее
  final case class CommandUnavailable(state: String, command: String) extends AppError:
    def message = s"Команда '$command' недоступна в состоянии $state"
  final case class LogIoError(detail: String) extends AppError:
    def message = s"Ошибка работы с журналом: $detail"
  final case class ConfigReloadFailed(detail: String) extends AppError:
    def message = s"Не удалось перезагрузить конфигурацию: $detail"
  final case class InternalError(detail: String) extends AppError:
    def message = s"Внутренняя ошибка: $detail"

final case class StorageSession(
                                 storageFile: Path,
                                 name: String,
                                 encrypted: Boolean,
                                 manifest: Manifest,
                                 versions: List[VersionRecord],
                                 sources: List[Source],
                                 key: Option[javax.crypto.spec.SecretKeySpec] = None
                               )

sealed trait AppState
object AppState:
  case object NoStorage extends AppState
  final case class Opened(session: StorageSession) extends AppState