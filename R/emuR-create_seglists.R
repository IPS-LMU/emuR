
convert_queryEmuRsegsToTibble <- function(emuDBhandle, emuRsegs){
  
  if(nrow(emuRsegs) == 0){
    return(dplyr::tibble(labels = character(), start = numeric(), end = numeric(),
                         db_uuid = character(), session = character(), 
                         bundle = character(), start_item_id = integer(), end_item_id = integer(),
                         level = character(), attribute = character(), start_item_seq_idx = integer(), 
                         end_item_seq_idx = integer(), type = character(), sample_start = integer(), 
                         sample_end = integer(), sample_rate = integer()))
  }
  
  resultAttrDef = unique(emuRsegs$level[!is.na(emuRsegs$level)])
  
  if(length(resultAttrDef) > 1){
    stop("Could not convert the emuRsegs object to a tibble as it contains multiple attribute definitions.")
  }
  attrDefLn = get_levelNameForAttributeName(emuDBhandle, resultAttrDef)
  # fix attribute/level 
  emuRsegs$attribute = resultAttrDef
  emuRsegs$level = attrDefLn
  if(any(is.na(emuRsegs$labels))){
    emuRsegs[is.na(emuRsegs$labels),]$attribute = NA
    emuRsegs[is.na(emuRsegs$labels),]$level = NA
  }
  
  # select columns in correct order
  res_tibble = emuRsegs %>% 
    dplyr::select("labels", "start", "end",
                  "db_uuid", "session", 
                  "bundle", "start_item_id", "end_item_id",
                  "level", "attribute", "start_item_seq_idx", 
                  "end_item_seq_idx", "type", "sample_start", 
                  "sample_end", "sample_rate") %>%
    dplyr::as_tibble()
  
  return(res_tibble)
  
}

convert_queryResultToEmusegs<-function(emuDBhandle, timeRefSegmentLevel=NULL, filteredTablesSuffix, calcTimes = T, verbose){
  queryStr = DBI::dbGetQuery(emuDBhandle$connection, "SELECT query_str FROM interm_res_meta_infos_tmp_root")$query_str
  emuRsegs = convert_queryResultToEmuRsegs(emuDBhandle, 
                                           timeRefSegmentLevel, 
                                           filteredTablesSuffix, 
                                           queryStr = queryStr, 
                                           calcTimes = calcTimes, 
                                           verbose = verbose)
  emusegs = as.emusegs(emuRsegs)
  return(emusegs)
}

##################################
#
convert_queryResultToEmuRsegs <- function(emuDBhandle, 
                                          timeRefSegmentLevel=NULL, 
                                          filteredTablesSuffix, 
                                          queryStr = "", 
                                          calcTimes = TRUE, 
                                          preserveAnchorLength = FALSE, # only set T by requery_hier
                                          verbose){
  
  itemsTableName = paste0("items", filteredTablesSuffix)
  labelsTableName = paste0("labels", filteredTablesSuffix)
  projectionItemsN = 0 
  if(DBI::dbExistsTable(emuDBhandle$connection, "interm_res_proj_items_tmp_root")){
    projectionItemsN = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n ",
                                                                      "FROM interm_res_proj_items_tmp_root"))$n
  }
  
  if(projectionItemsN > 0){
    
    # as this function uses the query_databaseHier function which only works on
    # the interm_res_items_tmp_root table -> insert everything into interm_res_items_tmp_root
    DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM interm_res_items_tmp_root"))
    DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_root ",
                                                  "SELECT DISTINCT ",
                                                  " db_uuid, ",
                                                  " session, ",
                                                  " bundle, ",
                                                  " p_seq_start_id AS seq_start_id, ",
                                                  " p_seq_end_id AS seq_end_id, ",
                                                  " p_seq_len AS seq_len, ",
                                                  " p_level AS level, ",
                                                  " p_seq_start_seq_idx AS seq_start_seq_idx, ",
                                                  " p_seq_end_seq_idx AS seq_end_seq_idx ", 
                                                  "FROM interm_res_proj_items_tmp_root"))
    
  }
  
  # check for empty result 
  itemsN = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT COUNT(*) AS n ",
                                                          "FROM interm_res_items_tmp_root ",
                                                          "WHERE db_uuid IS NOT NULL"))$n
  if(itemsN > 0 ){
    
    # use "normal" items
    itsTableName = "interm_res_items_tmp_root"
    seqStartIdColName = "seq_start_id"
    seqEndIdColName = "seq_end_id"
    seqLenColName = "seq_len"
    levelColName = "level"
    
    # set type of join depending on preserve*Length args
    if(preserveAnchorLength){
      joinType = "LEFT JOIN"
      orderByString = "" # don't reorder if left joining to perserve NA/NULL row placement
    }else{
      joinType = "INNER JOIN"
      orderByString = paste0("ORDER BY items_seq_start.db_uuid, ",
                             " items_seq_start.session, ", 
                             " items_seq_start.bundle, ", 
                             " items_seq_start.level, ", 
                             " items_seq_start.seq_idx")
    }
    
    
    dbConfig = load_DBconfig(emuDBhandle)
    
    resultAttrDef = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT ", levelColName, 
                                                                   " FROM ", itsTableName, 
                                                                   " WHERE level IS NOT NULL"))[[levelColName]]

    attrDefLn = get_levelNameForAttributeName(emuDBhandle, resultAttrDef)
    ld = get_levelDefinition(emuDBhandle, attrDefLn)
    
    
    # create temp table that holds emuRsegs without labels
    DBI::dbExecute(emuDBhandle$connection, paste0("CREATE TEMP TABLE emursegs_tmp ( ",
                                                  " labels TEXT, ",
                                                  " start REAL, ",
                                                  " end REAL, ",
                                                  " utts TEXT, ",
                                                  " db_uuid VARCHAR(36), ",
                                                  " session TEXT, ",
                                                  " bundle TEXT, ",
                                                  " start_item_id INTEGER, ",
                                                  " end_item_id INTEGER, ",
                                                  " level TEXT, ",
                                                  " start_item_seq_idx INTEGER, ",
                                                  " end_item_seq_idx INTEGER, ",
                                                  " type TEXT, ",
                                                  " sample_start INTEGER, ",
                                                  " sample_end INTEGER, ",
                                                  " sample_rate INTEGER",
                                                  ");"))
    
    if(!calcTimes){ # no times are requested then that makes things a lot easier :-)
      
      # set type of join depending on preserveAnchorLength
      if(preserveAnchorLength){
        joinType = "LEFT JOIN"
        orderByString = "ORDER BY interm_res_items_tmp_root.rowid" # don't reorder if left joining to perserve NA/NULL row placement
      }else{
        joinType = "INNER JOIN"
        orderByString = paste0("ORDER BY interm_res_items_tmp_root.db_uuid, ",
                               " interm_res_items_tmp_root.session, ",
                               " interm_res_items_tmp_root.bundle, ",
                               " interm_res_items_tmp_root.seq_start_seq_idx")
      }
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO emursegs_tmp ",
                                                    "SELECT 'XXX' AS labels, ",
                                                    " NULL AS start, ",
                                                    " NULL AS end, ",
                                                    " interm_res_items_tmp_root.session || ':' || interm_res_items_tmp_root.bundle AS utts, ",
                                                    " interm_res_items_tmp_root.db_uuid, ",
                                                    " interm_res_items_tmp_root.session, ",
                                                    " interm_res_items_tmp_root.bundle, ",
                                                    " interm_res_items_tmp_root.seq_start_id AS start_item_id, ",
                                                    " interm_res_items_tmp_root.seq_end_id AS end_item_id, ",
                                                    " interm_res_items_tmp_root.level AS level, ",
                                                    " interm_res_items_tmp_root.seq_start_seq_idx AS start_item_seq_idx, ",
                                                    " interm_res_items_tmp_root.seq_end_seq_idx AS end_item_seq_idx, ",
                                                    " items_seq_start.type AS type, ",
                                                    " NULL AS sampleStart, ",
                                                    " NULL AS sample_end, ",
                                                    " items_seq_start.sample_rate AS sample_rate ",
                                                    "FROM interm_res_items_tmp_root ", 
                                                    joinType, " items AS items_seq_start ",
                                                    "ON interm_res_items_tmp_root.db_uuid = items_seq_start.db_uuid ",
                                                    " AND interm_res_items_tmp_root.session = items_seq_start.session ",
                                                    " AND interm_res_items_tmp_root.bundle = items_seq_start.bundle ",
                                                    " AND interm_res_items_tmp_root.seq_start_id = items_seq_start.item_id ",
                                                    joinType, " items AS items_seq_end ",
                                                    "ON interm_res_items_tmp_root.db_uuid = items_seq_end.db_uuid ",
                                                    " AND interm_res_items_tmp_root.session = items_seq_end.session ",
                                                    " AND interm_res_items_tmp_root.bundle = items_seq_end.bundle ",
                                                    " AND interm_res_items_tmp_root.seq_end_id = items_seq_end.item_id ",
                                                    joinType, " labels ",
                                                    "ON interm_res_items_tmp_root.db_uuid = labels.db_uuid ",
                                                    " AND interm_res_items_tmp_root.session = labels.session ",
                                                    " AND interm_res_items_tmp_root.bundle = labels.bundle ",
                                                    " AND interm_res_items_tmp_root.seq_end_id = labels.item_id ",
                                                    " AND labels.name = '", resultAttrDef, "' ",
                                                    orderByString,
                                                    ""))
      
    }else if(ld$type != "ITEM"){ # if level has time information, time can be calculated from sample values directly
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO emursegs_tmp ",
                                                    "SELECT 'XXX' AS labels, ",
                                                    " CASE items_seq_start.type ",
                                                    "  WHEN 'SEGMENT' THEN ",
                                                    "   CASE items_seq_start.sample_start ",
                                                    "   WHEN 0 THEN CAST(0.0 AS REAL) ",
                                                    "   ELSE (CAST (items_seq_start.sample_start AS REAL) - 0.5 ) / CAST(items_seq_start.sample_rate AS REAL) * 1000.0 ",
                                                    "   END",
                                                    "  WHEN 'EVENT' THEN CAST (items_seq_start.sample_point AS REAL) / CAST(items_seq_start.sample_rate AS REAL) * 1000.0 ",
                                                    "  ELSE NULL ",
                                                    " END AS start, ",
                                                    " CASE items_seq_start.type ",
                                                    "  WHEN 'SEGMENT' THEN (CAST (items_seq_end.sample_start + items_seq_end.sample_dur AS REAL) + 0.5) / CAST (items_seq_end.sample_rate AS REAL) * 1000.0 ",
                                                    "  WHEN 'EVENT' THEN 0.0",
                                                    "  ELSE NULL ",
                                                    " END AS end, ",
                                                    " interm_res_items_tmp_root.session || ':' || interm_res_items_tmp_root.bundle AS utts, ",
                                                    " interm_res_items_tmp_root.db_uuid, ",
                                                    " interm_res_items_tmp_root.session, ",
                                                    " interm_res_items_tmp_root.bundle, ",
                                                    " interm_res_items_tmp_root.seq_start_id AS start_item_id, ",
                                                    " interm_res_items_tmp_root.seq_end_id AS end_item_id, ",
                                                    " interm_res_items_tmp_root.level AS level, ",
                                                    " interm_res_items_tmp_root.seq_start_seq_idx, ",
                                                    " interm_res_items_tmp_root.seq_end_seq_idx AS end_item_seq_idx, ",
                                                    " items_seq_start.type AS type, ",
                                                    " CASE items_seq_start.type ",
                                                    "    WHEN 'SEGMENT' THEN items_seq_start.sample_start ",
                                                    "    WHEN 'EVENT' THEN items_seq_start.sample_point ",
                                                    " END AS sample_start, ",
                                                    " CASE items_seq_start.type ",
                                                    "    WHEN 'SEGMENT' THEN (items_seq_end.sample_start + items_seq_end.sample_dur) ",
                                                    "    WHEN 'EVENT' THEN items_seq_start.sample_point ",
                                                    " END AS sample_end, ",
                                                    " items_seq_start.sample_rate AS sample_rate ",
                                                    "FROM interm_res_items_tmp_root ",
                                                    joinType, " items AS items_seq_start ", 
                                                    "ON interm_res_items_tmp_root.db_uuid = items_seq_start.db_uuid ",
                                                    " AND interm_res_items_tmp_root.session = items_seq_start.session ",
                                                    " AND interm_res_items_tmp_root.bundle = items_seq_start.bundle ",
                                                    " AND interm_res_items_tmp_root.seq_start_id = items_seq_start.item_id ",
                                                    joinType, " items AS items_seq_end ",
                                                    "ON interm_res_items_tmp_root.db_uuid = items_seq_end.db_uuid ",
                                                    " AND interm_res_items_tmp_root.session = items_seq_end.session ", 
                                                    " AND interm_res_items_tmp_root.bundle = items_seq_end.bundle ", 
                                                    " AND interm_res_items_tmp_root.seq_end_id = items_seq_end.item_id ",
                                                    joinType, " labels ",
                                                    "ON interm_res_items_tmp_root.db_uuid = labels.db_uuid ", 
                                                    " AND interm_res_items_tmp_root.session = labels.session ", 
                                                    " AND interm_res_items_tmp_root.bundle = labels.bundle ", 
                                                    " AND interm_res_items_tmp_root.seq_end_id = labels.item_id ", 
                                                    " AND labels.name = '", resultAttrDef, "' ",
                                                    orderByString, 
                                                    ""))
      
    }else{
      
      segLvlNms = find_segmentLevels(emuDBhandle, resultAttrDef)
      
      if(!is.null(timeRefSegmentLevel)){
        if(!(timeRefSegmentLevel %in% segLvlNms)){
          stop("Cannot resolve time information for result level '",
               resultAttrDef,
               "' using segment time reference level '",
               timeRefSegmentLevel,
               "'\nPlease set one of these levels for timeRefSegmentLevel parameter: ",
               paste(segLvlNms,collapse=', '),".")
        }
        lnwt = timeRefSegmentLevel # level name with time
      }else{
        segLvlsCnt=length(segLvlNms)
        if(segLvlsCnt>1){
          stop("Segment time information derivation for level '",
               resultAttrDef,
               "' is ambiguous:\nThe level is linked to multiple segment levels: ",
               paste(segLvlNms,collapse=', '),
               "\nPlease select one of these levels using the 'timeRefSegmentLevel' query parameter.")
        }else if(segLvlsCnt == 0){
          stop("Could not find a time bearing sub-level connected to '", 
               resultAttrDef, 
               "'. Consider either using 'calcTimes=F' or adding potentially missing link definitions in your emuDB.")
        }
        lnwt = segLvlNms[1] # level name with time
      }
      
      # insert all time items into new table
      timeItemsTableSuffix = "time_level_items"
      create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = timeItemsTableSuffix)
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", timeItemsTableSuffix, " ",
                                                    "SELECT ",
                                                    " db_uuid, ",
                                                    " session, ",
                                                    " bundle, ",
                                                    " item_id AS seq_start_id, ",
                                                    " item_id AS seq_end_id, ", 
                                                    " 1 AS seq_len, ",
                                                    " level, ",
                                                    " seq_idx AS start_seq_idx, ",
                                                    " seq_idx AS end_seq_idx  ",
                                                    "FROM ", itemsTableName, " ",
                                                    "WHERE db_uuid ='", emuDBhandle$UUID, "' ",
                                                    " AND level = '", lnwt, "' ",
                                                    " AND session IN (SELECT session FROM interm_res_items_tmp_root) ",
                                                    " AND bundle IN (SELECT bundle FROM interm_res_items_tmp_root) ",
                                                    ""))
      
      query_databaseHier(emuDBhandle, 
                         firstLevelName = lnwt, 
                         secondLevelName = attrDefLn, 
                         leftTableSuffix = timeItemsTableSuffix, 
                         rightTableSuffix = "root", 
                         filteredTablesSuffix, 
                         minMaxSeqIdxLeafOnly = F, # remove this parameter from function?
                         preserveLeafLength = F,
                         preserveAnchorLength = preserveAnchorLength,
                         verbose = verbose) # result written to lr_exp_res_tmp table

      # set type of join depending on preserveAnchorLength
      if(preserveAnchorLength){
        joinType = "LEFT JOIN"
        orderByString = "ORDER BY irit.rowid" # don't reorder if left joining to perserve NA/NULL row placement
      }else{
        joinType = "INNER JOIN"
        orderByString = paste0("ORDER BY lr_exp_res_tmp.db_uuid, ",
                               " lr_exp_res_tmp.session, ",
                               " lr_exp_res_tmp.bundle, ",
                               " min(itl.sample_start)")
      }
      
      # calculate left and right times and store in tmp table
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO emursegs_tmp ",
                                                    "SELECT 'XXX' AS labels, ",
                                                    " CASE itl.type ",
                                                    "  WHEN 'SEGMENT' THEN ",
                                                    "   CASE min(itl.sample_start) ",
                                                    "   WHEN 0 THEN CAST(0.0 AS REAL) ",
                                                    "   ELSE (CAST (min(itl.sample_start) AS REAL) - 0.5 ) / CAST(itl.sample_rate AS REAL) * 1000.0 ",
                                                    "   END",
                                                    "  WHEN 'EVENT' THEN 'Not implemented yet!'",
                                                    "  ELSE NULL ",
                                                    " END AS start, ",
                                                    " CASE itl.type ",
                                                    "  WHEN 'SEGMENT' THEN (CAST (max(itr.sample_start + itr.sample_dur) AS REAL) + 0.5) / CAST (itr.sample_rate AS REAL) * 1000.0 ",
                                                    "  WHEN 'EVENT' THEN 0.0",
                                                    "  ELSE NULL ",
                                                    " END AS end, ",
                                                    " lr_exp_res_tmp.session || ':' || lr_exp_res_tmp.bundle AS utts, ",
                                                    " lr_exp_res_tmp.db_uuid, ", 
                                                    " lr_exp_res_tmp.session, ",
                                                    " lr_exp_res_tmp.bundle, ",
                                                    " lr_exp_res_tmp.r_seq_start_id AS start_item_id, ",
                                                    " lr_exp_res_tmp.r_seq_end_id AS end_item_id, ",
                                                    " '", resultAttrDef, "' AS level, ",
                                                    " lr_exp_res_tmp.r_seq_start_seq_idx AS start_item_seq_idx, ",
                                                    " lr_exp_res_tmp.r_seq_end_seq_idx AS end_item_seq_idx, '", ld$type, "' AS type, ",
                                                    " min(itl.sample_start + 0) AS sample_start, max(itr.sample_start + itr.sample_dur) AS sample_end, ",
                                                    " itl.sample_rate AS sample_rate ",
                                                    "FROM interm_res_items_tmp_root AS irit ", 
                                                    joinType, " lr_exp_res_tmp ", 
                                                    "ON irit.db_uuid = lr_exp_res_tmp.db_uuid ", 
                                                    " AND irit.session = lr_exp_res_tmp.session ",
                                                    " AND irit.bundle = lr_exp_res_tmp.bundle ", 
                                                    " AND irit.seq_start_id = lr_exp_res_tmp.r_seq_start_id ", 
                                                    " AND irit.seq_end_id = lr_exp_res_tmp.r_seq_end_id ",
                                                    joinType, " ", itemsTableName, " AS itl ", 
                                                    "ON lr_exp_res_tmp.db_uuid = itl.db_uuid ", 
                                                    " AND lr_exp_res_tmp.session = itl.session ", 
                                                    " AND lr_exp_res_tmp.bundle = itl.bundle ",
                                                    " AND lr_exp_res_tmp.l_seq_start_id = itl.item_id ",
                                                    joinType, " ", itemsTableName, " AS itr ", # items table left & right
                                                    "ON lr_exp_res_tmp.db_uuid = itr.db_uuid ", 
                                                    " AND lr_exp_res_tmp.session = itr.session ", 
                                                    " AND lr_exp_res_tmp.bundle = itr.bundle ", 
                                                    " AND lr_exp_res_tmp.l_seq_end_id = itr.item_id ",
                                                    "GROUP BY irit.rowid, ", # using irit.rowid to preserve duplicates (requery only)
                                                    " irit.db_uuid, ", 
                                                    " irit.session, ",
                                                    " irit.bundle, ", 
                                                    " irit.seq_start_id, ", 
                                                    " irit.seq_end_id ", 
                                                    orderByString,
                                                    ""))
      
    }
    ################################
    # construct labels
    DBI::dbExecute(emuDBhandle$connection, paste0("CREATE INDEX IF NOT EXISTS emursegs_tmp_idx ON emursegs_tmp(db_uuid, session, bundle, start_item_id, end_item_id)"))
    
    # set type of join depending on preserve*Length args
    if(preserveAnchorLength){
      joinType = "LEFT JOIN"
      # orderByString =  "" # don't reorder if left joining to perserve NA/NULL row placement
    }else{
      joinType = "INNER JOIN"
    }
    
    seglist = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT ",
                                                             " GROUP_CONCAT(ungrouped.label, '->') AS labels, ",
                                                             " start, ",
                                                             " end, ", 
                                                             " utts, ",
                                                             " db_uuid, ",
                                                             " session, ", 
                                                             " bundle, ",
                                                             " start_item_id, ",
                                                             " end_item_id, ",
                                                             " level, ", " start_item_seq_idx, ", 
                                                             " end_item_seq_idx, ", 
                                                             " type, ",
                                                             " sample_start, ", 
                                                             " sample_end, ", 
                                                             " sample_rate ",
                                                             "FROM ",
                                                             " (SELECT ",
                                                             "  emursegs_tmp.rowid, ",
                                                             "  labels.label, ",
                                                             "  emursegs_tmp.start, ", 
                                                             "  emursegs_tmp.end, ",
                                                             "  emursegs_tmp.utts, ",
                                                             "  emursegs_tmp.db_uuid, ",
                                                             "  emursegs_tmp.session, ",
                                                             "  emursegs_tmp.bundle, ",
                                                             "  emursegs_tmp.start_item_id, ",
                                                             "  emursegs_tmp.end_item_id, ",
                                                             "  emursegs_tmp.level, ",
                                                             "  emursegs_tmp.start_item_seq_idx, ",
                                                             "  emursegs_tmp.end_item_seq_idx, ",
                                                             "  emursegs_tmp.type, ",
                                                             "  emursegs_tmp.sample_start, ",
                                                             "  emursegs_tmp.sample_end, ",
                                                             "  emursegs_tmp.sample_rate ",
                                                             "FROM emursegs_tmp ", 
                                                             joinType, " ", itemsTableName, " AS itl ", 
                                                             "ON emursegs_tmp.db_uuid = itl.db_uuid ",
                                                             " AND emursegs_tmp.session = itl.session ",
                                                             " AND emursegs_tmp.bundle = itl.bundle ",
                                                             " AND emursegs_tmp.start_item_id = itl.item_id ",
                                                             joinType, " ", itemsTableName, " AS itr ", 
                                                             "ON emursegs_tmp.db_uuid = itr.db_uuid ",
                                                             " AND emursegs_tmp.session = itr.session ",
                                                             " AND emursegs_tmp.bundle = itr.bundle ",
                                                             " AND emursegs_tmp.end_item_id = itr.item_id ",
                                                             joinType, " ", itemsTableName, " AS iseq ", 
                                                             "ON itl.db_uuid = iseq.db_uuid ",
                                                             " AND itl.session = iseq.session ",
                                                             " AND itl.bundle = iseq.bundle ",
                                                             " AND itl.level = iseq.level ",
                                                             " AND iseq.seq_idx >= itl.seq_idx ",
                                                             " AND iseq.seq_idx <= itr.seq_idx ", # join all seq. items
                                                             joinType, " ", labelsTableName, " AS labels ", # items table left & right
                                                             "ON iseq.db_uuid = labels.db_uuid ", 
                                                             " AND iseq.session = labels.session ", 
                                                             " AND iseq.bundle = labels.bundle ",
                                                             " AND iseq.item_id = labels.item_id ",
                                                             " AND labels.name = '", resultAttrDef, "' ",
                                                             "ORDER BY ",
                                                             " emursegs_tmp.db_uuid, ",
                                                             " emursegs_tmp.session, ",
                                                             " emursegs_tmp.bundle, ",
                                                             " emursegs_tmp.level, ",
                                                             " iseq.seq_idx",
                                                             ") AS ungrouped ",
                                                             "GROUP BY rowid",
                                                             ""))
    
    # drop temp table
    DBI::dbExecute(emuDBhandle$connection, paste0("DROP TABLE IF EXISTS emursegs_tmp"))
  }else{
    seglist = data.frame(labels = character(), start = numeric(), end = numeric(), utts = character(),
                         db_uuid = character(), session = character(), bundle = character(),
                         start_item_id = numeric(), end_item_id = numeric(), level = character(),
                         type = character(), sample_start = numeric(), sample_end = numeric(), sample_rate = numeric(),
                         stringsAsFactors = F)
  }
  # set emusegs type attribute, default 'segment'
  slType='segment'
  if(nrow(seglist)>0){
    # set to event only if all rows are of type EVENT
    if(all(seglist$type == "EVENT" || is.na(seglist$type) )){
      slType='event'
    }
  }
  # queryStr = DBI::dbGetQuery(emuDBhandle$connection, "SELECT query_str FROM interm_res_meta_infos_tmp_root")$query_str
  segmentList = make.emuRsegs(dbName = emuDBhandle$dbName, seglist = seglist, query = queryStr, type = slType)
  
  # if contains NAs -> also set everything to NA
  if(any(is.na(segmentList$labels))){
    segmentList[is.na(segmentList$labels),] = NA
  }
  
  return(segmentList)
  
}

##################################
##################################
##################################
equal.emusegs<-function(seglist1,seglist2,compareAttributes=TRUE,tolerance=0.0,uttsPrefix2=''){
  
  if(!inherits(seglist1,"emusegs")){
    stop("seglist1 is not of class emusegs")
  }
  if(!inherits(seglist2,"emusegs")){
    stop("seglist2 is not of class emusegs")
  }
  if(tolerance<0){
    stop("tolerance must be greater or equal 0")
  }
  
  sl1RowCnt=nrow(seglist1)
  sl2RowCnt=nrow(seglist2)
  if(sl1RowCnt!=sl2RowCnt){
    return(FALSE)
  }
  if(compareAttributes){
    attrEq=((attr(seglist1,'query')==attr(seglist2,'query'))&attr(seglist1,'database')==attr(seglist2,'database'))
    if(!attrEq){
      return(FALSE)
    }
  }
  # seglist have no implicit order
  sl1Order=order('[[.data.frame'(seglist1,'utts'),'[[.data.frame'(seglist1,'start'),'[[.data.frame'(seglist1,'end'),'[[.data.frame'(seglist1,'labels'))
  sl1=`[.data.frame`(seglist1,sl1Order,)
  sl2Order=order('[[.data.frame'(seglist2,'utts'),'[[.data.frame'(seglist2,'start'),'[[.data.frame'(seglist2,'end'),'[[.data.frame'(seglist2,'labels'))
  sl2=`[.data.frame`(seglist2,sl2Order,)
  equal=TRUE
  rs=1:sl1RowCnt
  for(i in rs){
    l1='[[.data.frame'(sl1,i,'labels')
    l2='[[.data.frame'(sl2,i,'labels')
    s1='[[.data.frame'(sl1,i,'start')
    s2='[[.data.frame'(sl2,i,'start')
    e1='[[.data.frame'(sl1,i,'end')
    e2='[[.data.frame'(sl2,i,'end')
    u1='[[.data.frame'(sl1,i,'utts')
    u2='[[.data.frame'(sl2,i,'utts')
    u2Sess=paste0(uttsPrefix2,u2)
    if(l1!=l2 | u1!=u2Sess){
      return(FALSE)
    }
    sdAbs=abs(s2-s1)
    if(sdAbs>tolerance){
      equal=FALSE
      #cat("Start differs ",s1,s2,sdAbs,"\n")
    }
    edAbs=abs(e2-e1)
    if(edAbs>tolerance){
      equal=FALSE
      #cat("End differs ",e1,e2,edAbs,"\n")
    }
    
  }
  return(equal)
  
}
