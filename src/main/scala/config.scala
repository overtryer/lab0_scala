import java.nio.file.{Files, Path}
import scala.util.Try

final case class AppConfig(
                            logFile: String,
                            keyDirectory: String,
                            defaultContainerName: String,
                            checkOnStorageOpen: Boolean,
                            confirmBeforeBackup: Boolean,
                            confirmBeforeRestoreOverwrite: Boolean,
                            maximumVersionsPerFile: Int,
                            hashAlgorithm: String,
                            defaultExclusions: List[String]
                          )

object ConfigLoader:
  
  def load(path: Path): Either[AppError, AppConfig] =
    for
      content <- Try(Files.readString(path)).toEither
        .left.map(e => AppError.ConfigParseError(s"Не удалось прочитать файл: ${e.getMessage}"))
      
      json    <- Try(ujson.read(content)).toEither
        .left.map(e => AppError.ConfigParseError(s"Неверный JSON: ${e.getMessage}"))
      
      config  <- parse(json)
    yield config

  private def parse(json: ujson.Value): Either[AppError, AppConfig] =
    Try(json.obj).toEither
      .left.map(_ => AppError.ConfigParseError("Корневой элемент JSON должен быть объектом {}"))
      .flatMap { obj =>
        val logFileOpt       = obj.get("logFile").flatMap(_.strOpt)
        val keyDir           = obj.get("keyDirectory").flatMap(_.strOpt).getOrElse("keys")
        val defaultContainer = obj.get("defaultContainerName").flatMap(_.strOpt).getOrElse("backup.store")
        val checkOpen        = obj.get("checkOnStorageOpen").flatMap(_.boolOpt).getOrElse(true)
        val confirmBackup    = obj.get("confirmBeforeBackup").flatMap(_.boolOpt).getOrElse(true)
        val confirmRestore   = obj.get("confirmBeforeRestoreOverwrite").flatMap(_.boolOpt).getOrElse(true)
        val maxVersions      = obj.get("maximumVersionsPerFile").flatMap(_.numOpt).map(_.toInt).getOrElse(10)
        val hashAlgo         = obj.get("hashAlgorithm").flatMap(_.strOpt).getOrElse("SHA-256")
        val exclusions       = obj.get("defaultExclusions").flatMap(_.arrOpt)
          .map(_.flatMap(_.strOpt).toList)
          .getOrElse(List("*.tmp", "*.log", "cache", "node_modules"))
        
        for
          logFile <- logFileOpt.toRight(AppError.LogPathMissing)
          _       <- if maxVersions >= 1 then Right(())
          else Left(AppError.MaxVersionsInvalid(maxVersions))
          _       <- if List("SHA-256", "SHA-1", "MD5").contains(hashAlgo) then Right(())
          else Left(AppError.UnsupportedHash(hashAlgo))
          _       <- if exclusions.exists(_.trim.isEmpty) then Left(AppError.BadExclusion("пустая строка"))
          else Right(())
        yield AppConfig(
          logFile, keyDir, defaultContainer, checkOpen,
          confirmBackup, confirmRestore, maxVersions, hashAlgo, exclusions
        )
      }