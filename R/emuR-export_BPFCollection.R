##' Exports an emuDB into a BAS Partitur File (BPF) Collection
##' 
##' This function exports an emuDB into the BAS Partitur File format, with one BPF file per bundle.
##' The user must pass a list of matching label names and BPF keys. 
##' \strong{Important:} The BPF format does not support explicit hierarchies with more than three
##' levels. Hence, you will probably lose information when exporting complex hierarchies.
##'
##' @param handle handle to the emuDB
##' @param targetDir directory where the BPF collection should be saved
##' @param extractLevels list containing the names of labels (not levels!) that should be extracted, and their
##' matching BPF keys, e.g. extractLevels = list(SampleRate="SAM", Text="ORT", Phonemes="SAP")
##' @param refLevel optional name of level (not label!) used as reference for symbolic links. If NULL (the default), a link-less BPF collection is created.
##' @param newLevels optional vector containing names of levels in the BPF collection that are not part of the standard 
##' BPF levels. See \url{https://www.bas.uni-muenchen.de/forschung/Bas/BasFormatseng.html#Partitur_tiersdef} for details on 
##'standard BPF levels.
##' @param newLevelClasses optional vector containing the classes of levels in the newLevels vector as integers. 
##' Must have the same length and order as newLevels.
##' @param copyAudio if true, audio files are copied to the new BPF collection
##' @param verbose display infos, warnings and show progress bar
##' @return NULL
##' @seealso export_TextGridCollection
##' @export

export_BPFCollection <- function(handle,
                                 targetDir,
                                 extractLevels,
                                 refLevel = NULL,
                                 verbose = TRUE,
                                 newLevels = NULL,
                                 newLevelClasses = NULL,
                                 copyAudio = FALSE)
{
  check_emuDBhandle(handle)
  levelClasses = as.list(BPF_STANDARD_LEVEL_CLASSES)
  names(levelClasses) = BPF_STANDARD_LEVELS
  levelClasses[newLevels] = newLevelClasses
  
  bundles = list_bundles(handle)
  
  if(nrow(bundles) == 0)
  {
    stop("The database does not appear to contain any bundles.")
  }
  
  build_skeleton(handle, targetDir, copyAudio, verbose)
  
  if(verbose)
  {
    cat("INFO: Exporting emuDB containing", nrow(bundles), "bundle(s) to BPF collection ...\n")
    pb = utils::txtProgressBar(min = 0, max = nrow(bundles), initial = 0, style=3)
    utils::setTxtProgressBar(pb, 0)
  }
  
  
  for(kdx in 1:nrow(bundles))
  {
    bundle = bundles[kdx,"name"]
    session = bundles[kdx,"session"]
    
    bpfLines = c("LBD:")
    headerLines = c("LHD: Partitur 1.3")
    seen_sam = FALSE
    
    bpf_target = file.path(targetDir, handle$dbName, session, paste0(bundle, ".par"))
    
    link_map = make_link_map(handle, session, bundle, refLevel)
    
    extract_set_string = paste0("('", paste(names(extractLevels), collapse="','"), "')")
    
    queryTxt = paste0("SELECT ",
                      " items.item_id, ",
                      " items.sample_start, ",
                      " items.sample_dur, ",
                      " items.sample_point, ",
                      " labels.name, ",
                      " labels.label ",
                      "FROM items ",
                      "JOIN labels ",
                      "ON items.item_id = labels.item_id ",
                      " AND items.db_uuid = labels.db_uuid ",
                      " AND items.session = labels.session ",
                      " AND items.bundle = labels.bundle",
                      basic_cond(handle, session, bundle, prefix = "items"), 
                      " AND labels.name in ", extract_set_string, " ",
                      "ORDER BY labels.name, items.seq_idx")
    
    labels = DBI::dbGetQuery(handle$connection, queryTxt)
    
    if(nrow(labels) > 0)
    {
      for(idx in 1:nrow(labels))
      {
        key = labels[idx, "name"]
        bpfkey = extractLevels[[key]]
        label = labels[idx, "label"]
        
        if(!(bpfkey %in% names(levelClasses)))
        {
          queryTxt = paste0("SELECT items.item_id ",
                            "FROM items ",
                            "JOIN labels ",
                            "ON items.item_id = labels.item_id ",
                            " AND items.db_uuid = labels.db_uuid ",
                            " AND items.session = labels.session ",
                            " AND items.bundle = labels.bundle ",
                            basic_cond(handle, session, bundle, prefix = "items"), 
                            " AND labels.name = '", key, "'")
          tmp = DBI::dbGetQuery(handle$connection, queryTxt)
          
          if(nrow(tmp) > 1)
          {
            stop("More than one item with a ", key, " label. Cannot use this for header")
          }
          
          headerLines = c(headerLines, paste0(bpfkey, ": ", label))
          if(bpfkey == "SAM")
          {
            seen_sam = TRUE
          }
        }
        
        else
        {
          class = levelClasses[[bpfkey]]
          
          label = labels[idx, "label"]
          item_id = labels[idx, "item_id"]
          
          if(class %in% c(1,4,5))
          {
            link = "-1"
            
            if(toString(item_id) %in% names(link_map))
            {
              link = paste(unique(link_map[[toString(item_id)]]), collapse=",")
            }
          }
          
          if(class %in% c(2,4))
          {
            start = labels[idx, "sample_start"]
            dur = labels[idx, "sample_dur"]
            
            if(is.na(start) || is.na(dur))
            {
              container = infer_temporal_info(handle, 
                                              session, 
                                              bundle, 
                                              item_id, 
                                              type = "SEGMENT")
              start = container$sample_start
              dur = container$sample_dur
            }
            
            if(is.na(start) || is.na(dur))
            {
              stop("Invalid segment of class 2 or 4 ", labels[idx,], ". Could not infer start and duration.")
            }
          }
          
          if(class %in% c(3,5))
          {
            point = labels[idx, "sample_point"]
            if(is.na(point))
            {
              container = infer_temporal_info(handle, 
                                              session, 
                                              bundle, 
                                              item_id, 
                                              type="EVENT")
              point = container$sample_point
            }
            if(is.na(point))
            {
              stop("Invalid segment of class 3 or 5 ", labels[idx,], ". Could not infer sample point.")
            }
          }
          
          if(class == 1)
          {
            bpfLines = c(bpfLines, paste0(bpfkey, ": ", link, " ", label))
          }
          else if(class == 2)
          {
            bpfLines = c(bpfLines, paste0(bpfkey, ": ", start, " ", dur, " ", label))
          }
          else if(class == 3)
          {
            bpfLines = c(bpfLines, paste0(bpfkey, ": ", point, " ", label))
          }
          else if(class == 4)
          {
            bpfLines = c(bpfLines, paste0(bpfkey, ": ", start, " ", dur, " ", link, " ", label))
          }
          else if(class == 5)
          {
            bpfLines = c(bpfLines, paste0(bpfkey, ": ", point, " ", link, " ", label))
          }
        }
      }
    }
    
    
    if(!seen_sam)
    {
      queryTxt = paste0("SELECT DISTINCT sample_rate ",
                        "FROM items", basic_cond(handle, session, bundle))
      samplerate = DBI::dbGetQuery(handle$connection, queryTxt)
      
      if(nrow(samplerate) > 0)
      {
        headerLines = c(headerLines, paste0("SAM: ", samplerate[1,1]))
      }
    }
    
    writeLines(c(headerLines, bpfLines), con = bpf_target, sep = "\n", useBytes = TRUE)
    
    if(verbose)
    {
      utils::setTxtProgressBar(pb, kdx)
    }
  }
  if(verbose)
  {
    cat("\n")
  }
}

make_link_map <- function(handle, session, bundle, refLevel)
{
  link_map = list()
  if(!is.null(refLevel))
  {
    ref_id_map = list()
    queryTxt = paste0("SELECT item_id ",
                      "FROM items", basic_cond(handle, session, bundle), 
                      " AND level = '", refLevel, "' ",
                      "ORDER BY seq_idx")
    refItems = DBI::dbGetQuery(handle$connection, queryTxt)
    
    if(nrow(refItems) > 0)
    {
      ref_id_map[refItems$item_id] = 0:(nrow(refItems)-1)
      
      for(ref_id in refItems$item_id)
      {
        for(id in get_links(handle, session, bundle, ref_id))
        {
          if(!(id %in% names(link_map)))
          {
            link_map[[toString(id)]] = list()
          }
          link_map[[toString(id)]][[length(link_map[[toString(id)]]) + 1]] = ref_id_map[[ref_id]]
        }
      }
    }
  }
  return(link_map)
}

get_links <- function(handle, session, bundle, ref_id, direction="all", 
                      level=NULL, item_table = "items", link_table = "links")
{
  links = c(ref_id)
  
  if(direction == "all")
  {
    directions = list(c("from_id", "to_id"), c("to_id", "from_id"))
  }
  else if(direction == "up")
  {
    directions = list(c("from_id", "to_id"))
  }
  else if(direction == "down")
  {
    directions = list(c("to_id", "from_id"))
  }
  
  for(direction in directions)
  {
    current_anchors = c(ref_id)
    this = direction[1]
    other = direction[2]
    
    while(length(current_anchors) > 0)
    {
      id_set_string = paste0("(", paste(current_anchors, collapse = "," ) ,")")
      
      if(is.null(level))
      {
        queryTxt = paste0("SELECT l.", this, " FROM ", link_table, " AS l", 
                          basic_cond(handle, session, bundle, prefix = "l"), 
                          " AND l.", other, " in ", id_set_string)
      }
      else
      {
        queryTxt = paste0("SELECT l.", this, " FROM ", link_table, " AS l ",
                          "JOIN ", item_table, " AS i ",
                          "ON l.db_uuid == i.db_uuid ",
                          " AND l.session == i.session ",
                          " AND l.bundle == i.bundle ",
                          " AND l.", this, " == i.item_id",
                          basic_cond(handle, session, bundle, prefix = "i"), 
                          " AND l.", other, " in ", id_set_string, 
                          " AND i.level=='", level, "'")
      }
      
      current_anchors = DBI::dbGetQuery(handle$connection, queryTxt)[[this]]
      links = c(links, current_anchors)
    }
  }
  
  if(!is.null(level))
  {
    id_set_string = paste0("(", paste(links, collapse = "," ) ,")")
    queryTxt = paste0("SELECT item_id FROM ", item_table, " ", 
                      basic_cond(handle, session, bundle), 
                      " AND level=='", level, "' ",
                      " AND item_id in ", id_set_string) 
    links = DBI::dbGetQuery(handle$connection, queryTxt)[["item_id"]]
  }
  
  return(links)
}


build_skeleton <- function(handle, targetDir, copyAudio, verbose)
{
  if(file.exists(file.path(targetDir, handle$dbName)))
  {
    stop(file.path(targetDir, handle$dbName), " already exists.")
  }
  
  created = dir.create(file.path(targetDir, handle$dbName))
  if(!created){
    stop("Couldn't create ", file.path(targetDir, handle$dbName))
  }
  
  bundles = list_bundles(handle)
  sessions = list_sessions(handle)
  
  if(verbose)
  {
    max = nrow(sessions)
    if(copyAudio)
    {
      max = nrow(bundles) + nrow(sessions)
    }
    cat("INFO: Building BPF collection skeleton ...\n")
    progress = 0
    pb = utils::txtProgressBar(min = 0, max = max, initial = 0, style=3)
    utils::setTxtProgressBar(pb, 0)
  }
  
  for(session in sessions$name)
  {
    created = dir.create(file.path(targetDir, handle$dbName, session))
    if(!created){
      stop("Couldn't create ", file.path(targetDir, handle$dbName, session))
    }
    if(verbose)
    {
      progress = progress + 1
      utils::setTxtProgressBar(pb, progress)
    }
    
    if(copyAudio)
    {
      for(bundle in list_bundles(handle, session = session)$name)
      {
        queryTxt = paste0("SELECT annotates FROM bundle", 
                          basic_cond(handle, session, bundle, bundlename = "name"))
        
        annotates = DBI::dbGetQuery(handle$connection, queryTxt)[1,1]
        
        wav_target = file.path(targetDir, 
                               handle$dbName,
                               session,
                               annotates)
        
        wav_source = file.path(handle$basePath,
                               paste0(session, session.suffix),
                               paste0(bundle, bundle.dir.suffix),
                               annotates)
        
        file.copy(wav_source, wav_target)
        if(verbose)
        {
          progress = progress + 1
          utils::setTxtProgressBar(pb, progress)
        }
      }
    }
  }
  if(verbose)
  {
    cat("\n")
  }
}


basic_cond <- function(handle, 
                       session, 
                       bundle, 
                       bundlename = "bundle", 
                       prefix = NULL)
{
  if(is.null(prefix))
  {
    prefix = "";
  }
  else
  {
    prefix = paste0(prefix, ".")
  }
  return(paste0(" WHERE ", prefix, "db_uuid='", handle$UUID, "' AND ", prefix, "session='", 
                session, "' AND ", prefix, bundlename, "='", bundle, "' "))
}

infer_temporal_info <- function(handle, session, bundle, item_id, type="SEGMENT")
{
  if(type == "SEGMENT")
  {
    needed = c("sample_start", "sample_dur")
  }
  else if(type == "EVENT")
  {
    needed = c("sample_point")
  }
  
  container = list()
  container[needed] = NA
  
  links_down = get_links(handle, session, bundle, item_id, direction="down")
  
  queryTxt = paste0("SELECT DISTINCT type FROM items", 
                    basic_cond(handle, session, bundle),
                    " AND type=='", type, "'")
  
  types = DBI::dbGetQuery(handle$connection, queryTxt)
  if(nrow(types) > 0)
  {
    ids_as_set = paste0("(", paste(links_down, collapse = ","), ")")
    needed_as_set = paste(needed, collapse = ",")
    
    for(t in types$type)
    {
      queryTxt = paste0("SELECT ", needed_as_set, " FROM items", 
                        basic_cond(handle, session, bundle),
                        " AND type='", t, "' ",
                        " AND item_id in ", ids_as_set, 
                        " ORDER by ", needed[1])
      tmp = DBI::dbGetQuery(handle$connection, queryTxt)
      
      if(nrow(tmp) > 0)
      {
        if(type == "EVENT")
        {
          if(is.numeric(tmp[1,1]))
          {
            container$sample_point = tmp[1,1]
            break
          }
        }
        else if(type == "SEGMENT")
        {
          start = tmp[1,1]
          dur = tmp[nrow(tmp), 1] + tmp[nrow(tmp), 2] - tmp[1,1]
          if(is.numeric(start) && is.numeric(dur))
          {
            container$sample_start = start
            container$sample_dur = dur
            break
          }
        }
      }
    }
  }
  
  return(container)
}


