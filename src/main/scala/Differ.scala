object Differ:

  def diff(prev: Manifest, current: ScanResult, sources: List[Source]): List[Change] =
    val activeSourceIds = sources.filter(_.enabled).map(_.id).toSet
    val prevFiles = prev.files.filter { case ((sid, _), _) => activeSourceIds.contains(sid) }
    val currFiles = current.files

    val prevKeys = prevFiles.keySet
    val currKeys = currFiles.keySet

    val rawRemoved = prevKeys.diff(currKeys).toList.map { key => prevFiles(key) }
    val rawAdded = currKeys.diff(prevKeys).toList.map { key => currFiles(key) }

    val modified = prevKeys.intersect(currKeys).toList.collect {
      case key if prevFiles(key).hash != currFiles(key).hash =>
        Change.Modified(prevFiles(key), currFiles(key))
    }

    val (renamed, removed) = rawRemoved.foldLeft((List.empty[Change], List.empty[Change])) {
      case ((accRenamed, accRemoved), oldEntry) =>
        rawAdded.find(newEntry => newEntry.hash == oldEntry.hash && newEntry.size == oldEntry.size) match
          case Some(matchedNew) =>
            (Change.Renamed(oldEntry, matchedNew) :: accRenamed, accRemoved)
          case None =>
            (accRenamed, Change.Removed(oldEntry) :: accRemoved)
    }

    val renamedNewHashes = renamed.collect { case Change.Renamed(_, curr) => curr.hash }.toSet
    val added = rawAdded.filterNot(newEntry => renamedNewHashes.contains(newEntry.hash)).map(Change.Added(_))

    removed ++ added ++ modified ++ renamed ++ current.problems