import java.nio.file.Path
import javax.crypto.spec.SecretKeySpec

trait StorageService:
  def createNew(path: Path, session: StorageSession, key: Option[SecretKeySpec]): Either[AppError, Unit]
  def readSession(path: Path, key: Option[SecretKeySpec]): Either[AppError, StorageSession]
  def saveSession(session: StorageSession, filesToAdd: Map[Hash, Path], key: Option[SecretKeySpec], config: AppConfig): Either[AppError, Unit]
  def extractFile(containerPath: Path, hash: Hash, destPath: Path, key: Option[SecretKeySpec]): Either[AppError, Unit]

class ZipStorageService extends StorageService:
  def createNew(path: Path, session: StorageSession, key: Option[SecretKeySpec]): Either[AppError, Unit] =
    Storage.createNew(path, session, key)

  def readSession(path: Path, key: Option[SecretKeySpec]): Either[AppError, StorageSession] =
    Storage.readSession(path, key)

  def saveSession(session: StorageSession, filesToAdd: Map[Hash, Path], key: Option[SecretKeySpec], config: AppConfig): Either[AppError, Unit] =
    Storage.saveSession(session, filesToAdd, key, config)

  def extractFile(containerPath: Path, hash: Hash, destPath: Path, key: Option[SecretKeySpec]): Either[AppError, Unit] =
    Storage.extractFile(containerPath, hash, destPath, key)