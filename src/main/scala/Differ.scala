object Differ:

  def diff(prev: Manifest, current: ScanResult, sources: List[Source]): List[Change] =
    val activeSourceIds = sources.filter(_.enabled).map(_.id).toSet
    
    val prevFiles = prev.files.filter { case ((sid, _), _) => activeSourceIds.contains(sid) }
    val currFiles = current.files

    val prevKeys = prevFiles.keySet
    val currKeys = currFiles.keySet

    val removed = prevKeys.diff(currKeys).toList.map { key => Change.Removed(prevFiles(key)) }
    val added = currKeys.diff(prevKeys).toList.map { key => Change.Added(currFiles(key)) }

    val modified = prevKeys.intersect(currKeys).toList.collect {
      case key if prevFiles(key).hash != currFiles(key).hash =>
        Change.Modified(prevFiles(key), currFiles(key))
    }

    removed ++ added ++ modified ++ current.problems