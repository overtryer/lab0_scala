import java.nio.file.{Files, Path}
import scala.util.Try

// =============================================================
//  МОДЕЛЬ КОНФИГУРАЦИИ
// =============================================================
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

// =============================================================
//  ЗАГРУЗКА И ВАЛИДАЦИЯ
// =============================================================
object ConfigLoader:

  /** Читает файл и превращает его в AppConfig. Если что-то не так — возвращает AppError. */
  def load(path: Path): Either[AppError, AppConfig] =
    for
      // 1. Пытаемся прочитать файл. Try -> Either
      content <- Try(Files.readString(path)).toEither
        .left.map(e => AppError.ConfigParseError(s"Не удалось прочитать файл: ${e.getMessage}"))

      // 2. Парсим JSON. Try -> Either
      json    <- Try(ujson.read(content)).toEither
        .left.map(e => AppError.ConfigParseError(s"Неверный JSON: ${e.getMessage}"))

      // 3. Извлекаем и валидируем поля
      config  <- parse(json)
    yield config

  private def parse(json: ujson.Value): Either[AppError, AppConfig] =
    Try(json.obj).toEither
      .left.map(_ => AppError.ConfigParseError("Корневой элемент JSON должен быть объектом {}"))
      .flatMap { obj =>
        // Извлекаем значения с дефолтными, если их нет
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

        // Валидация (for-comprehension по Either)
        for
          // Обязательное поле: путь к логу
          logFile <- logFileOpt.toRight(AppError.LogPathMissing)

          // Валидация максимального количества версий
          _       <- if maxVersions >= 1 then Right(())
          else Left(AppError.MaxVersionsInvalid(maxVersions))

          // Валидация алгоритма хеширования
          _       <- if List("SHA-256", "SHA-1", "MD5").contains(hashAlgo) then Right(())
          else Left(AppError.UnsupportedHash(hashAlgo))

          // Валидация исключений (проверка на пустые строки)
          _       <- if exclusions.exists(_.trim.isEmpty) then Left(AppError.BadExclusion("пустая строка"))
          else Right(())
        yield AppConfig(
          logFile, keyDir, defaultContainer, checkOpen,
          confirmBackup, confirmRestore, maxVersions, hashAlgo, exclusions
        )
      }