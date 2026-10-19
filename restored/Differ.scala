object Differ:

  /**
   * Чистая функция сравнения.
   * Принимает предыдущее состояние (манифест) и текущее (результат сканера).
   * Возвращает список изменений.
   */
  def diff(prev: Manifest, current: ScanResult): List[Change] =
    val prevFiles = prev.files
    val currFiles = current.files

    val prevKeys = prevFiles.keySet
    val currKeys = currFiles.keySet

    // 1. Удаленные файлы (были в prev, но пропали из curr)
    val removed = prevKeys.diff(currKeys).toList.map { key =>
      Change.Removed(prevFiles(key))
    }

    // 2. Новые файлы (появились в curr, их не было в prev)
    val added = currKeys.diff(prevKeys).toList.map { key =>
      Change.Added(currFiles(key))
    }

    // 3. Измененные файлы (есть и там, и там, но хеши разные)
    val modified = prevKeys.intersect(currKeys).toList.collect {
      case key if prevFiles(key).hash != currFiles(key).hash =>
        Change.Modified(prevFiles(key), currFiles(key))
    }

    // 4. Добавляем проблемы, обнаруженные сканером (нечитаемые файлы, недоступные источники)
    removed ++ added ++ modified ++ current.problems
