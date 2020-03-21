##' Replace item labels
##' 
##' Replace the labels of all annotation items, or more specifically 
##' of attribute definitions belonging to annotation items, in an emuDB that 
##' match the provided \code{origLabels} character vector which the 
##' corresponding labels provided by the \code{newLabels} character vector. 
##' The indicies of the label vectors provided are used to match the labels 
##' (i.e. \code{origLabels[i]} will be replaced by \code{newLabels[i]}).
##' 
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param attributeDefinitionName name of a attributeDefinition of a emuDB 
##' where the labels are to be replaced
##' @param origLabels character vector containing labels that are to be replaced
##' @param newLabels character vector containing labels that are to replaced 
##' the labels of \code{origLabels}. This vector has to be of equal length 
##' to the \code{origLabels} vector.
##' @param verbose Show progress bars and further information
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # replace all "I" and "p" labels with "I_replaced" and "p_replaced"
##' replace_itemLabels(ae, attributeDefinitionName = "Phonetic", 
##'                        origLabels = c("I", "p"), 
##'                        newLabels = c("I_replaced", "p_replaced"))
##' 
##' }
##' 
replace_itemLabels <- function(emuDBhandle, 
                               attributeDefinitionName, 
                               origLabels, 
                               newLabels, 
                               verbose = TRUE) {
  
  #############################
  # check input parameters
  
  check_emuDBhandle(emuDBhandle)
  
  allAttrNames = get_allAttributeNames(emuDBhandle)
  if(!attributeDefinitionName %in% allAttrNames){
    stop(paste0("No attributeDefinitionName: ", attributeDefinitionName, 
                " found in emuDB! The available attributeNames are: ", 
                paste0(get_allAttributeNames(emuDBhandle), collapse = "; ")))
  }
  
  if(class(origLabels) != "character" | class(newLabels) != "character" | length(origLabels) != length(newLabels)){
    stop("origLabels and newLabels have to be a character vector of the same length!")  
  }
  
  #
  #############################
  if(verbose){
    cat("\n  INFO: creating temporary index...\n")
  }
  # create temp index
  DBI::dbExecute(emuDBhandle$connection, paste0("CREATE INDEX IF NOT EXISTS label_replace_tmp_idx ",
                                                "ON labels(db_uuid, name, label)"))
  
  # progressbar 
  if(verbose){
    cat("\n  INFO: replacing ", length(origLabels), " attribute labels\n")
    pb <- utils::txtProgressBar(min = 0, max = length(origLabels), style = 3)
  }
  
  # transaction start
  DBI::dbBegin(emuDBhandle$connection)
  
  for(i in 1:length(origLabels)){
    DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE labels ",
                                                  "SET label = '", newLabels[i], "' ",
                                                  "WHERE db_uuid='", emuDBhandle$UUID, "' ",
                                                  " AND name = '", attributeDefinitionName, "' ",
                                                  " AND label = '", origLabels[i], "'"))
    if(verbose){
      utils::setTxtProgressBar(pb, i)
    }
  }
  
  # transaction end
  DBI::dbCommit(emuDBhandle$connection)
  
  # remove temp index
  DBI::dbExecute(emuDBhandle$connection, paste0("DROP INDEX IF EXISTS label_replace_tmp_idx"))
  
  # close progress bar if open
  if(exists('pb')){
    close(pb)
    cat("\n")
  }
  
  rewrite_annots(emuDBhandle, verbose = verbose)
  
}

##' Duplicate level
##' 
##' Duplicate level of emuDB including all of its items and its various 
##' attributeDefinitions. If the \code{duplicateLinks} variable is set 
##' to \code{TRUE} all the links to and from the original items are 
##' duplicated.
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param levelName name of level in emuDB that is to be duplicated
##' @param duplicateLevelName name given to newly duplicated level
##' @param duplicateLinks if set to \code{TRUE} (the default) all the
##' links to and from the original items are duplicated to point to the 
##' new items of the new duplicate level.
##' @param linkDuplicates link the duplicated ITEMs to the originals. This
##' can only be set to \code{TRUE} if \code{duplicateLinks} is set to \code{FALSE}.
##' @param linkDefType type given to link definition. Only relevant if \code{linkDuplicates}
##' is set to \code{TRUE}.
##' @param verbose show progress bars and further information
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # duplicate Phonetic level
##' duplicate_level(ae, levelName = "Phonetic",
##'                     duplicateLevelName = "Phonetic2")
##' 
##' }
##' 
duplicate_level <- function(emuDBhandle, 
                            levelName, 
                            duplicateLevelName, 
                            duplicateLinks = TRUE, 
                            linkDuplicates = FALSE, 
                            linkDefType = "ONE_TO_ONE", 
                            verbose = TRUE) {
  
  check_emuDBhandle(emuDBhandle)
  
  ldefs = list_levelDefinitions(emuDBhandle)
  
  if(!levelName %in% ldefs$name){
    stop(paste0(levelName, " is not a valid level name! Available levels are: ", 
                paste0(ldefs$name, collapse = "; ")))
  }
  
  if(duplicateLevelName %in% ldefs$name){
    stop(paste0(duplicateLevelName, " already exists in the emuDB: ", emuDBhandle$dbName))
  }
  
  if(duplicateLinks & linkDuplicates){
    stop(paste0("duplicateLinks & linkDuplicates are both set to TRUE! This is not allowed!"))
  }
  
  ldef = ldefs[ldefs$name == levelName,]
  
  #########################
  # duplicate item entries
  
  # create temp tables
  DBI::dbExecute(emuDBhandle$connection, paste0("CREATE TEMP TABLE IF NOT EXISTS bndl_max_item_id_tmp ",
                                                "(db_uuid VARCHAR(36), ",
                                                " session TEXT, ",
                                                " bundle TEXT, ",
                                                " bndl_max_item_id INTEGER, ",
                                                "PRIMARY KEY (db_uuid, session, bundle))"))
  # create bndl_max_item_id_tmp table
  DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO bndl_max_item_id_tmp ",
                                                "SELECT ",
                                                " db_uuid, ",
                                                " session, ",
                                                " bundle, ",
                                                " max(item_id) AS bndl_max_item_id ",
                                                "FROM items ",
                                                "WHERE db_uuid = '", emuDBhandle$UUID, "' ",
                                                "GROUP BY db_uuid, session, bundle"))
  # duplicate level items table elements
  DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items ",
                                                "SELECT ",
                                                " items.db_uuid, ",
                                                " items.session, ",
                                                " items.bundle, ",
                                                " (item_id + bndl_max_item_id) AS item_id, ", 
                                                "'", duplicateLevelName, "' AS level, ",
                                                " type, ",
                                                " seq_idx, ",
                                                " sample_rate, ",
                                                " sample_point, ",
                                                " sample_start, ",
                                                " sample_dur ",
                                                "FROM items, ",
                                                " bndl_max_item_id_tmp ", 
                                                "WHERE items.db_uuid = bndl_max_item_id_tmp.db_uuid ",
                                                " AND items.session = bndl_max_item_id_tmp.session ",
                                                " AND items.bundle = bndl_max_item_id_tmp.bundle ",
                                                " AND items.level = '", levelName, "'"))
  
  ##########################
  # duplicate labels entries
  DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO labels ",
                                                "SELECT l.db_uuid, ",
                                                " l.session, ",
                                                " l.bundle, ",
                                                " (l.item_id + mid.bndl_max_item_id) AS item_id, ",
                                                " l.label_idx, ",
                                                "CASE WHEN l.name = '", levelName, "' ",
                                                "THEN '", duplicateLevelName, "' ",
                                                "ELSE l.name END AS name, l.label ",
                                                "FROM items AS it, ",
                                                " labels AS l, ",
                                                " bndl_max_item_id_tmp AS mid ",
                                                "WHERE it.db_uuid = l.db_uuid ",
                                                " AND it.session = l.session ",
                                                " AND it.bundle = l.bundle ",
                                                " AND it.item_id = l.item_id ",
                                                " AND it.db_uuid = mid.db_uuid ",
                                                " AND it.session = mid.session ",
                                                " AND it.bundle = mid.bundle ",
                                                " AND it.level = '", levelName, "'"))
  
  if(duplicateLinks){
    ##########################
    # duplicate links entries
    
    # where duplicate items are parents
    DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO links ",
                                                  "SELECT ",
                                                  " li.db_uuid, ",
                                                  " li.session, ",
                                                  " li.bundle, ",
                                                  " (li.from_id + mid.bndl_max_item_id) AS from_id, ",
                                                  " li.to_id, ",
                                                  " li.label ",
                                                  "FROM items AS it, ",
                                                  " links AS li, ",
                                                  " bndl_max_item_id_tmp AS mid ",
                                                  "WHERE it.db_uuid = li.db_uuid ",
                                                  " AND it.session = li.session ",
                                                  " AND it.bundle = li.bundle ",
                                                  " AND it.item_id = li.from_id ",
                                                  " AND it.db_uuid = mid.db_uuid ",
                                                  " AND it.session = mid.session ",
                                                  " AND it.bundle = mid.bundle ",
                                                  " AND it.level = '", levelName, "'"))
    
    # where duplicate items are children
    DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO links ",
                                                  "SELECT ",
                                                  " li.db_uuid, ",
                                                  " li.session, ",
                                                  " li.bundle, ",
                                                  " li.from_id, ",
                                                  " (li.to_id + mid.bndl_max_item_id) AS to_id, ",
                                                  " li.label ",
                                                  "FROM items AS it, ",
                                                  " links AS li, ",
                                                  " bndl_max_item_id_tmp AS mid ",
                                                  "WHERE it.db_uuid = li.db_uuid ",
                                                  " AND it.session = li.session ",
                                                  " AND it.bundle = li.bundle ",
                                                  " AND it.item_id = li.to_id ",
                                                  " AND it.db_uuid = mid.db_uuid ",
                                                  " AND it.session = mid.session ",
                                                  " AND it.bundle = mid.bundle ",
                                                  " AND it.level = '", levelName, "'"))
    
  }else{
    if(linkDuplicates){
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO links ",
                                                    "SELECT ",
                                                    " it1.db_uuid, ",
                                                    " it1.session, ",
                                                    " it1.bundle, ",
                                                    " it1.item_id AS from_id, ",
                                                    " it2.item_id AS to_id, ",
                                                    " null AS label ",
                                                    "FROM items AS it1, ",
                                                    " items AS it2 ",
                                                    "WHERE it1.db_uuid = it2.db_uuid ",
                                                    " AND it1.session = it2.session ",
                                                    " AND it1.bundle = it2.bundle ",
                                                    " AND it1.level = '", levelName,"' ",
                                                    " AND it2.level = '", duplicateLevelName,"' ",
                                                    " AND it1.type = it2.type ",
                                                    " AND it1.seq_idx = it2.seq_idx"))
    }
  }
  
  # drop temp tables
  DBI::dbExecute(emuDBhandle$connection, paste0("DROP TABLE IF EXISTS ", "bndl_max_item_id_tmp"))
  
  ########################
  # add levelDefs 
  add_levelDefinition(emuDBhandle, 
                      duplicateLevelName, 
                      type = ldef$type, 
                      rewriteAllAnnots = FALSE, 
                      verbose = verbose)
  
  ########################
  # add linkDefinitions
  if(duplicateLinks){
    linkDefs = list_linkDefinitions(emuDBhandle)
    # super 
    superLds = linkDefs[linkDefs$superlevelName == levelName,]
    if(nrow(superLds) > 0){
      for(i in 1:nrow(superLds)){
        add_linkDefinition(emuDBhandle, 
                           type = superLds[i,]$type, 
                           superlevelName = duplicateLevelName, 
                           sublevelName = superLds[i,]$sublevelName)
      }
    }
    
    # sub 
    subLds = linkDefs[linkDefs$sublevelName == levelName,]
    if(nrow(subLds) > 0){
      for(i in 1:nrow(subLds)){
        add_linkDefinition(emuDBhandle, 
                           type = subLds[i,]$type, 
                           superlevelName = subLds[i,]$superlevelName, 
                           sublevelName = duplicateLevelName)
      }
    }
  }
  
  if(linkDuplicates){
    add_linkDefinition(emuDBhandle, 
                       type = linkDefType, 
                       superlevelName = levelName, 
                       sublevelName = duplicateLevelName)
  }
  
  ########################
  # add attributeDefintions
  attrDefs = list_attributeDefinitions(emuDBhandle, levelName)
  for(i in 1:nrow(attrDefs)){
    if(attrDefs[i,]$name != levelName){
      internal_add_attributeDefinition(emuDBhandle,
                                       levelName = duplicateLevelName,
                                       name = attrDefs[i,]$name,
                                       type = attrDefs[i,]$type, 
                                       rewriteAllAnnots = FALSE, 
                                       verbose = verbose, 
                                       insertLabels = F)
    }
    # copy legalLabels
    ll = get_legalLabels(emuDBhandle, levelName, attrDefs[i,]$name)
    if(!is.na(ll)){
      set_legalLabels(emuDBhandle, 
                      duplicateLevelName, 
                      attrDefs[i,]$name, 
                      legalLabels = ll)
    }
    # copy labelGroups
    attrDefLgs = list_attrDefLabelGroups(emuDBhandle, 
                                         levelName, 
                                         attributeDefinitionName = attrDefs[i,]$name)
    if(nrow(attrDefLgs) > 0){
      for(j in 1:nrow(attrDefLgs)){
        if(attrDefs[i,]$name == levelName){
          tmpAttrDefName = duplicateLevelName
        }else{
          tmpAttrDefName = attrDefs[i,]$name
        }
        add_attrDefLabelGroup(emuDBhandle, duplicateLevelName, 
                              attributeDefinitionName = tmpAttrDefName, 
                              labelGroupName = attrDefLgs[j,]$name, 
                              labelGroupValues = unlist(stringr::str_split(attrDefLgs[j,]$values, "; ")))
      }
    }
  }
  
  rewrite_annots(emuDBhandle, verbose = verbose)
  
}


##' List sample rates of media and annotation (_annot.json) files
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param sessionPattern A regular expression pattern matching session names to be searched from the database
##' @param bundlePattern A regular expression pattern matching bundle names to be searched from the database
##' 
##' @return tibble with the columns 
##' \itemize{
##' \item session
##' \item bundle
##' \item sample_rate_annot_json
##' \item sample_rate_media_file
##' }
##' \code{session}, \code{b}
##' @export
list_sampleRates <- function(emuDBhandle, sessionPattern = '.*', bundlePattern = '.*'){
  
  db_config = load_DBconfig(emuDBhandle)
  
  bndls = DBI::dbReadTable(emuDBhandle$connection, "bundle")
  
  # filter sessions/bundles
  ses_bool = emuR_regexprl(sessionPattern, bndls$session)
  bndl_bool = emuR_regexprl(bundlePattern, bndls$name)
  bndls = bndls[ses_bool & bndl_bool,]
  
  if(nrow(bndls) < 1) stop("no bundles found that match the sessionPattern & bundlePattern")
  
  bndls$sample_rate_media_file = -1
  
  bndls$sample_rate_annot_json = -1
  
  for(row_idx in 1:nrow(bndls)){
    annot_json_path = file.path(emuDBhandle$basePath, 
                                paste0(bndls[row_idx,]$session, session.suffix), 
                                paste0(bndls[row_idx,]$name, bundle.dir.suffix),
                                paste0(bndls[row_idx,]$name, bundle.annotation.suffix, ".json"))

    media_file_path = file.path(emuDBhandle$basePath, 
                                paste0(bndls[row_idx,]$session, session.suffix), 
                                paste0(bndls[row_idx,]$name, bundle.dir.suffix),
                                paste0(bndls[row_idx,]$name, ".", db_config$mediafileExtension))
    
        
    annot_json_sample_rate = jsonlite::fromJSON(annot_json_path)$sampleRate
    bndls$sample_rate_annot_json[row_idx] = annot_json_sample_rate
    
    media_file_sample_rate = attr(wrassp::read.AsspDataObj(media_file_path, end = 20), "sampleRate")
    bndls$sample_rate_media_file[row_idx] = media_file_sample_rate
  }
  
  res = dplyr::as_tibble(dplyr::select(bndls, 
                                       "session", 
                                       "bundle" = "name", 
                                       "sample_rate_media_file", 
                                       "sample_rate_annot_json"))
  return(res)
}

##' Resample annotations (\code{_annot.json}) files of emuDB
##' 
##' Resample all annotations (\code{_annot.json}) files of emuDB to a specified 
##' sample rate. It is up to the user to ensure that the samplerates of 
##' the annot.json files match those of the \code{.wav} files.
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param newSampleRate target sample rate
##' @param verbose show progress bars and further information
##' @export
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # resample
##' resample_annots(ae, newSampleRate = 16000)
##' 
##' }
resample_annots <- function(emuDBhandle, newSampleRate, verbose = TRUE) {
  
  stop("not implemented yet!!!")
  
  DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE items ",
                                                "SET sample_rate =  ", newSampleRate, ", ",
                                                " sample_point = ROUND((sample_point / sample_rate) * ", newSampleRate, ") ",
                                                " sample_start = ROUND(((sample_start - 0.5) / sample_rate) * ", newSampleRate, ") ",
                                                " sample_dur = sample_dur "))
  
  DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items WHERE level = 'Tone'"))
}



## Append an item (on a given level) to each bundle
##
## @param emuDBhandle emuDB handle object (see \link{load_emuDB})
## @param levelName Name of the level to which to append the new items
## @param labels Character vector containing one label for each attributeDefinition of \code{levelName}
## @param sessionPattern A (RegEx) pattern matching sessions to be included in the operation
## @param bundlePattern A (RegEx) pattern matching bundles to be included in the operation
## @param verbose Show progress bars and further information
## @export
## @seealso \code{\link{change_labels}}
## @keywords emuDB
## @examples
## \dontrun{
## TO DO - Add example
## }
append_itemsToLevel = function(emuDBhandle,
                               levelName,
                               labels,
                               sessionPattern = ".*",
                               bundlePattern = ".*",
                               verbose = T) {
  ##
  ## Check pre-conditions
  ##
  levelDefinition = get_levelDefinition(emuDBhandle, levelName)
  
  if (is.null(levelDefinition)) {
    print("Error: The given level does not exist ")
    return(invisible(NULL))
  }
  
  if (length(labels) != length(levelDefinition$attributeDefinitions)) {
    print (
      paste0(
        "Error: The number of labels (",
        length(labels),
        ") must match the number of attribute definitions (",
        length(levelDefinition$attributeDefinitions),
        ") for the given level (",
        levelName,
        ")"
      )
    )
    
    return(invisible(NULL))
  }
  
  
  ##
  ## Filter bundles according to sessionPattern and bundlePattern
  ##
  bundles = list_bundles(emuDBhandle)
  sessionMatch = emuR_regexprl (sessionPattern, bundles$session)
  bundleMatch = emuR_regexprl (bundlePattern, bundles$name)
  bundles = bundles [sessionMatch & bundleMatch, ]
  
  
  ##
  ## Get sample rate
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT sample_rate FROM bundle
    WHERE
    db_uuid = ? AND
    session = ? AND
    name = ?"
  )
  
  DBI::dbBind(statement,
              list(rep(emuDBhandle$UUID, nrow(bundles)),
                   bundles$session,
                   bundles$name))
  sampleRate = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  
  ##
  ## Get appropriate item ID
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT max(item_id) FROM items
    WHERE
    db_uuid = ? AND
    session = ? AND
    bundle = ?"
  )
  
  DBI::dbBind(statement,
              list(rep(emuDBhandle$UUID, nrow(bundles)),
                   bundles$session,
                   bundles$name))
  itemID = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  itemID[is.na(itemID),1] = 0
  itemID = itemID + 1
  
  
  ##
  ## Get appropriate sequence index
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "SELECT max(seq_idx) FROM items
    WHERE
    db_uuid = ? AND
    session = ? AND
    bundle = ? AND
    level = ?"
  )
  
  DBI::dbBind(statement,
              list(rep(emuDBhandle$UUID, nrow(bundles)),
                   bundles$session,
                   bundles$name,
                   rep(levelName, nrow(bundles))))
  sequenceIndex = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  sequenceIndex[is.na(sequenceIndex),1] = 0
  sequenceIndex = sequenceIndex + 1
  
  
  ##
  ## Insert items
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    "INSERT INTO items
    (db_uuid, session, bundle, item_id, level, type, seq_idx, sample_rate)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
  )
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(bundles)),
      bundles$session,
      bundles$name,
      itemID[,1],
      rep(levelName, nrow(bundles)),
      rep("ITEM", nrow(bundles)),
      sequenceIndex[,1],
      sampleRate$sample_rate
    )
  )
  ## @todo check success
  DBI::dbClearResult(statement)
  
  
  ##
  ## Insert labels
  ##
  
  for (i in 1:length(labels)) {
    attributeDefinition = emuR::list_attributeDefinitions(emuDBhandle,
                                                          levelName)[i, "name"]
    
    statement = DBI::dbSendStatement(
      emuDBhandle$connection,
      "INSERT INTO labels
      (db_uuid, session, bundle, item_id, label_idx, name, label)
      VALUES (?, ?, ?, ?, ?, ?, ?)"
    )
    
    DBI::dbBind(
      statement,
      list(
        rep(emuDBhandle$UUID, nrow(bundles)),
        bundles$session,
        bundles$name,
        itemID[,1],
        rep(i, nrow(bundles)),
        rep(attributeDefinition, nrow(bundles)),
        rep(labels[i], nrow(bundles))
      )
    )
    
    ## @todo check success
    DBI::dbClearResult(statement)    
  }
  
  rewrite_annots(emuDBhandle, verbose = verbose)
}

# ##' Add items to an empty level
# ##'
# ##'
# ##' Although an object of class emuRsegs may be passed into this function
# ##' it is not obligatory. The requiered column names of the data.frame
# ##' object passed into this function are
# ##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
# ##' @param levelName Name of the level to which to add the items
# ##' @param seglist Segmentlist containing information about the items that are
# ##' to be added to the specified level. 
# ##' @param verbose Show progress bars and further information
# ##' @export
# ##' @seealso \code{\link{change_labels}}
# ##' @keywords emuDB
# ##' @examples
# ##' \dontrun{
# ##' TO DO - Add example
# ##' }
# add_itemsToEmptyLevel <- function(emuDBhandle, levelName, seglist, verbose = TRUE){
#   
#   levelDef = get_levelDefinition(emuDBhandle, levelName)
#   
#   if(is.null(levelDef)){
#     stop("Specified level does not exist!")
#   }
#   
#   # check input parameters
#   if(levelDef$type == "SEGMENT"){
#     # if(is.null(sampleStart) || is.null(sampleEnd)){
#     #   stop("Specified level is of type SEGMENT! Both sampleStart and sampleEnd have to be set!")
#     # }
#     # if(length(labels) != length(sampleStart) || length(labels) != length(sampleEnd)){
#     #   stop("labels, sampleStart and sampleEnd have to be of the same length!")
#     # }
#   }else if(levelDef$type == "EVENT"){
#     # if(is.null(sampleStart)){
#     #   stop("Specified level is of type EVENT! sampleStart has to be set!")
#     # }
#     # if(length(labels) != length(sampleStart)){
#     #   stop("labels and sampleStart have to be of the same length!")
#     # }
#   }else{
#     stop("ITEM levels not supported yet!")
#   }
#   
#   # check that level is empty
#   res = DBI::dbGetQuery(emuDBhandle$connection, statement = paste0("SELECT * FROM items WHERE level='", levelName, "'"))
#   if(nrow(res) != 0){
#     stop("Specified level is not empty!")
#   }
#   
#   # use dplyr to sort seglist (in case these go mixed up somehow)
#   sortedSl = seglist %>% dplyr::arrange(session, bundle, sample_start)
#   
#   
#   if(levelDef$type == "SEGMENT"){
#   }else if(levelDef$type == "EVENT"){
#     
#     sortedSl %>%
#       dplyr::group_by(session, bundle) %>% 
#       dplyr::mutate(seq_idx = row_number())
#     
#     # create items df
#     itemsDf = data.frame(db_uuid = emuDBhandle$UUID, 
#                          session = sortedSl$session, 
#                          bundle = sortedSl$bundle, 
#                          item_id = sortedSl$start_item_id, # SIC!!! 
#                          level = rep(levelName, length), 
#                          type = ,
#                          seq_idx = ,
#                          sample_rate = ,
#                          sample_point = ,
#                          sample_start = ,
#                          sample_dur = )
#     
#   }else{
#     stop("ITEM levels not supported yet!")
#   }
# }

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuR-autoproc_annots.R')
