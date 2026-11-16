import java.nio.file.Paths
import java.time.Instant

class SourcesHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        ctx.ui.println("Текущие источники:")
        session.sources.zipWithIndex.foreach { case (s, idx) =>
          val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
          ctx.ui.println(s"${idx + 1}. ${s.path} [$status]")
          ctx.ui.println(s"   Последняя копия: ${s.lastBackup.map(formatMinute).getOrElse("—")}")
        }

        ctx.ui.println("\nУправление:")
        ctx.ui.println("1. Добавить")
        ctx.ui.println("2. Отключить/Включить")
        ctx.ui.println("3. Удалить")
        ctx.ui.println("4. Изменить настройки")
        ctx.ui.println("0. Назад")
        ctx.ui.print("Выбор: ")

        val updatedSources = ctx.ui.readLineTrimmed() match
          case "1" =>
            ctx.ui.println("Введите путь к папке:")
            val p = Paths.get(ctx.ui.readLineTrimmed())
            val newId = SourceId(session.sources.map(_.id.value).maxOption.getOrElse(0L) + 1L)
            ctx.logger.info(s"Добавлен новый источник: $p")
            session.sources :+ Source(newId, p)
          case "2" =>
            ctx.ui.print("Введите номер источника: ")
            ctx.ui.readLineTrimmed().toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              ctx.logger.info(s"Переключен источник: ${src.path}")
              session.sources.map(s => if s.id == src.id then s.copy(enabled = !s.enabled) else s)
            }.getOrElse(session.sources)
          case "3" =>
            ctx.ui.print("Введите номер источника: ")
            ctx.ui.readLineTrimmed().toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              ctx.logger.info(s"Удален источник: ${src.path}")
              session.sources.filterNot(_.id == src.id)
            }.getOrElse(session.sources)
          case "4" =>
            ctx.ui.print("Введите номер источника: ")
            ctx.ui.readLineTrimmed().toIntOption.flatMap(n => session.sources.lift(n - 1)).map { src =>
              ctx.ui.println(s"Включать вложенные каталоги? (y/n) (сейчас: ${src.recursive}):")
              val newRec = ctx.ui.readLineTrimmed().toLowerCase == "y"
              ctx.ui.println(s"Исключения через запятую (сейчас: ${src.exclusions.map(_.pattern).mkString(", ")}):")
              val exStr = ctx.ui.readLineTrimmed()
              val newExclusions = if exStr.isEmpty then Nil else exStr.split(",").map(s => Glob(s.trim)).toList
              ctx.logger.info(s"Изменены настройки источника: ${src.path}")
              session.sources.map(s => if s.id == src.id then s.copy(recursive = newRec, exclusions = newExclusions) else s)
            }.getOrElse(session.sources)
          case _ => session.sources

        if updatedSources != session.sources then
          val updated = session.copy(sources = updatedSources)
          ctx.storage.saveSession(updated, Map.empty, session.key, ctx.config) match
            case Right(_) =>
              ctx.ui.println("Источники обновлены.")
              ctx.logger.info("Конфигурация источников сохранена")
              updated.sources.zipWithIndex.foreach { case (s, idx) =>
                val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
                ctx.ui.println(s"  ${idx + 1}. ${s.path} [$status]")
              }
            case Left(err) =>
              ctx.ui.println(s"Ошибка сохранения: ${err.message}")
              ctx.logger.error(s"Не удалось сохранить источники: ${err.message}")
          AppState.Opened(updated)
        else state

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state

  private def formatMinute(i: Instant): String = i.toString.take(16).replace("T", " ")