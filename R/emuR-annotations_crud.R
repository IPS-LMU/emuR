##' Create new items programmatically
##' 
##' @description Allows creating annotation items programmatically on a single level. 
##' You have to pass in a data frame describing the new items. Each new item is identified by
##' its \code{session}, \code{bundle}, \code{level}, and depending on the 
##' level type either:
##' 
##' \itemize{
##' \item \code{sequence index (start_item_seq_idx)}: when level type = \code{ITEM}
##' \item \code{start}: start time in ms * 1000 (see output of \link{query}) when level type = \code{EVENT}
##' \item \code{start} and \code{end}: start and end time in ms * 1000 () when level type = \code{SEGMENT} (currently not yet supported!)
##' } 
##' .
##' The \code{level} with its associated \code{attributes} determines how many
##' labels must be provided. You must provide a label for every existing attribute.
##' 
##' Sessions, bundles, levels and attributes must exist beforehand. The sequence
##' index is explained below.
##' 
##' Within each bundle, there can be multiple annotation items on every level.
##' Their order within the level is given by their sequence index. All *existing*
##' items have a natural-valued sequence index and there are no gaps in the
##' sequences (i.e. if a level contains n annotation items, they are indexed 1..n).
##' 
##' Any newly created item must be given a sequence index. The sequence index may
##' be real-valued (it will automatically be replaced with a natural value). To
##' prepend the new item to the existing one, pass a value lower than one. To
##' append it to the existing items, you can either pass \code{NA} or any value
##' that you know is greater than n (the number of existing items in that level).
##' It does not need to be exactly n+1. To place the new item between two
##' existing ones, use any real value between the sequence indexes of the existing
##' neighbors.
##' 
##' If you are appending multiple items at the same time, every sequence index
##' (including NA) can only be used once per session/bundle/level combination
##' (because session/bundle/level/sequence index are a unique identifier of an item's ).
##' 
##' After creating the items, all sequence indexes (which may now be real-valued,
##' natural-valued or NA) are sorted in ascending order and then replaced with
##' the values 1..n, where n is the number of items on that level. While sorting,
##' NA values are placed at the end.
##' 
##' Currently it is not possible to insert items into levels of type SEGMENT.
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToCreate A data frame with the columns
##' \itemize{
##' \item\code{session},
##' \item\code{bundle},
##' \item\code{level},
##' \item\code{start_item_seq_idx}(\code{start_item_seq_idx} is used instead of 
##' \code{seq_idx} so that the result of a \code{\link{query}} call can be used directly. 
##' \code{\link{query}} can return a sequence of items defined by \code{start_item_seq_idx} 
##' and \code{end_item_seq_idx} which have the same value if single items are returned),
##' \item\code{attribute}, and
##' \item \code{labels}.
##' }
##' *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (can be real-valued or natural-valued),
##' all other columns must be of type character.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' 
##' @export
create_itemsInLevel = function(emuDBhandle,
                               itemsToCreate,
                               rewriteAllAnnots = TRUE,
                               verbose = TRUE) {
  # check that only one level is provided
  levelName = unique(itemsToCreate$level)
  if(length(levelName) > 1){
    stop("'itemsToCreate' contains multiple levels! The created ITEMs have to be on the same level!")
  }
  
  # check that only one attribute is provided (currenlty only single attributes allowed)
  attributeName = unique(itemsToCreate$attribute)
  if(length(levelName) > 1){
    stop("'itemsToCreate' contains multiple attributes! The created ITEMs have to be on the same attribute!")
  }
  
  ## check the level is of type ITEM or EVENT (SEGMENT to follow)
  levelDefinition = get_levelDefinition(emuDBhandle, levelName)
  if(is.null(levelDefinition)) stop("level '", levelName, "' doesn't exist!")
  if (!(levelDefinition$type %in% c("ITEM", "EVENT"))) {
    stop(paste0("The level:", levelName, " provided is not of type ITEM or EVENT"))
  }
  
  ## check that all required columns are available
  required_colnames = c("session", "bundle", "level", "attribute", "labels")
  if(levelDefinition$type == "ITEM"){
    required_colnames = c(required_colnames, "start_item_seq_idx")
  }else if(levelDefinition$type == "EVENT"){
    required_colnames = c(required_colnames, "start")
  }
  
  if(!all(required_colnames %in% names(itemsToCreate))){
    stop(paste0("Not all required columns are available in itemsToCreate data.frame! ",
                "The required columns are: ", paste(required_colnames, collapse = "; ")))
  }
  
  ## check types of columns
  if(!is.character(itemsToCreate$session) | 
     !is.character(itemsToCreate$bundle) | 
     !is.character(itemsToCreate$level) |
     !is.character(itemsToCreate$attribute) |
     !is.character(itemsToCreate$labels)
  ){
    stop(paste0("Not all columns match the required type!"))
  }
  if(levelDefinition$type == "ITEM"){
    if(!is.numeric(itemsToCreate$start_item_seq_idx)) stop(paste0("Not all columns match the required type!"))
  }else if(levelDefinition$type == "EVENT"){
    if(!is.numeric(itemsToCreate$start)) stop(paste0("Not all columns match the required type!"))
  }
  
  # rename labels column to label to match labels SQL table column name
  colnames(itemsToCreate)[colnames(itemsToCreate)=="labels"] <- "label"
  
  ## check that every session/bundle combination exists
  bundleList = list_bundles(emuDBhandle)
  
  invalidItems = dplyr::anti_join(x = itemsToCreate,
                                  y = bundleList,
                                  by = c("session",
                                         "bundle" = "name"))
  
  if (nrow(invalidItems) != 0) {
    stop("Some of the session/bundle combinations provided are invalid: ", invalidItems)
  }
  
  # extract all items
  items_all = DBI::dbReadTable(emuDBhandle$connection, "items")
  
  itemsToUpdate = NULL
  
  ## for ITEM levels
  if(levelDefinition$type == "ITEM"){
    
    ## Make sure all sequence indexes are unique within their respective level/attribute pair.
    itemsToCreate %>%
      dplyr::group_by(.data$session, .data$bundle, .data$level, .data$attribute) %>%
      dplyr::do(ensureSequenceIndexesAreUnique(.data))
    
    ## check for conflicting seq index
    in_both = dplyr::inner_join(itemsToCreate, 
                                items_all, 
                                by = c("session", 
                                       "bundle", 
                                       "level", 
                                       "start_item_seq_idx" = "seq_idx"))
    if(nrow(in_both) > 0){
      stop("Found existing items with same 'session', 'bundle', 'level', 'start_item_seq_idx'!")
    }
  }
  ## for EVENT
  if(levelDefinition$type == "EVENT"){
    
    # get sample_rate vector
    bundles_df = DBI::dbReadTable(emuDBhandle$connection, "bundle")
    sample_rate = dplyr::left_join(itemsToCreate, 
                                   bundles_df, 
                                   by = c("session", "bundle" = "name")) %>% 
      dplyr::select(dplyr::starts_with("sample_rate"))
    
    sample_rate = dplyr::as_tibble(sample_rate) # incase it isn't already
    sample_rate = dplyr::pull(sample_rate[,ncol(sample_rate)]) # relevant when multiple sample_rate.x sample_rate.y cols are present
    
    
    # calc. sample_point
    itemsToCreate$sample_point = round((itemsToCreate$start / 1000) * sample_rate)
    
    # check that times don't exist
    in_both = dplyr::inner_join(itemsToCreate, 
                                items_all, 
                                by = c("session", 
                                       "bundle", 
                                       "level", 
                                       "sample_point"))
    
    if(nrow(in_both) > 0){
      stop("Found existing items with same 'session', 'bundle', 'level', 'sample_point'!")
    }
    
    # calculate correct start_item_seq_idx
    
    # find existing items on same levels that are in itemsToCreate
    items_exist_in_levels = dplyr::left_join(items_all,
                                             itemsToCreate,
                                             by = c("session", 
                                                    "bundle", 
                                                    "level"))
    
    items_exist_in_levels = items_exist_in_levels[!is.na(items_exist_in_levels$db_uuid.y),]
    
    # create data.frame object that contains them both
    # that can be used to sort by sample_point to calcualte start_item_seq_idx
    items_to_sort = rbind(
      dplyr::tibble(session = items_exist_in_levels$session, 
                    bundle = items_exist_in_levels$bundle,
                    level = items_exist_in_levels$level,
                    attribute = attributeName,
                    label = "XXX",
                    item_id = items_exist_in_levels$item_id,
                    sample_point = items_exist_in_levels$sample_point.x,
                    item_from = "items_exist_in_levels"
      ),
      dplyr::tibble(session = itemsToCreate$session, 
                    bundle = itemsToCreate$bundle,
                    level = itemsToCreate$level,
                    attribute = itemsToCreate$attribute,
                    label = itemsToCreate$label,
                    item_id = -1,
                    sample_point = itemsToCreate$sample_point,
                    item_from = "itemsToCreate"
      )
    )
    
    items_sorted = items_to_sort %>% 
      dplyr::group_by(.data$session, .data$bundle, .data$level) %>% 
      dplyr::arrange(.data$sample_point, .by_group = T) %>%
      dplyr::mutate(start_item_seq_idx = dplyr::row_number())
    
    itemsToCreate = items_sorted %>% 
      dplyr::filter(.data$item_from == "itemsToCreate") %>% 
      dplyr::ungroup()
    
    itemsToUpdate = items_sorted %>% 
      dplyr::filter(.data$item_from == "items_exist_in_levels") %>% 
      dplyr::ungroup()
    
    
    
  }
  ## for SEGMENT
  if(levelDefinition$type == "SEGMENT"){
    
    # get sample_rate vector
    bundles_df = DBI::dbReadTable(emuDBhandle$connection, "bundle")
    sample_rate = dplyr::left_join(itemsToCreate, 
                                   bundles_df, 
                                   by = c("session", "bundle" = "name")) %>% 
      dplyr::select(dplyr::starts_with("sample_rate"))
    
    sample_rate = dplyr::as_tibble(sample_rate) # incase it isn't already
    sample_rate = dplyr::pull(sample_rate[,ncol(sample_rate)]) # relevant when multiple sample_rate.x sample_rate.y cols are present
    
    
    # calc. sample_start & sample_dur
    # TODO: fix the calculation (look at create_seglist code for time calc)
    itemsToCreate$sample_start = round((itemsToCreate$start / 1000) * sample_rate)
    itemsToCreate$sample_dur = round(((itemsToCreate$end - itemsToCreate$start) / 1000) * sample_rate)
    
    }
  
  ##
  ## Get the label index for each attribute (the label index marks the order of attributes within their level)
  ##
  itemsToCreate$labelIndex = get_labelIndex(emuDBhandle = emuDBhandle,
                                            levelName = itemsToCreate$level,
                                            attributeName = itemsToCreate$attribute)
  
  
  # remove old tmp tables if they exist
  remove_annotCrudTmpTables(emuDBhandle)
  
  ##
  ## Copy items data into temporary table
  ##
  create_annotCrudTmpTables(emuDBhandle)
  
  # update start_item_seq_idx in items_annot_crud_tmp table with itemsToUpdate
  if(!is.null(itemsToUpdate)){
    if(nrow(itemsToUpdate) >= 1){
      statement = DBI::dbSendStatement(
        emuDBhandle$connection,
        paste0("UPDATE items_annot_crud_tmp ",
               "SET seq_idx = ? ",
               "WHERE db_uuid = ? ",
               " AND session = ? ",
               " AND bundle = ? ",
               " AND item_id = ?"))
      
      DBI::dbBind(
        statement,
        list(
          itemsToUpdate$start_item_seq_idx,
          rep(emuDBhandle$UUID,length(itemsToUpdate$start_item_seq_idx)),
          itemsToUpdate$session,
          itemsToUpdate$bundle,
          itemsToUpdate$item_id
        )
      )
      
      DBI::dbClearResult(statement)
      
    }
  }
  
  ##
  ## Split the item list into individual items (identified by the session/bundle/level/sequenceIndex tuple),
  ## and proceed separately for each of them
  ##
  itemsToCreate %>%
    dplyr::group_by(.data$session, 
                    .data$bundle, 
                    .data$level, 
                    .data$start_item_seq_idx) %>%
    dplyr::do(insertItemIntoDatabase(emuDBhandle, 
                                     .data, 
                                     levelDefinition$type))
  
  
  ##
  ## Rewrite sequence indexes
  ##
  rewrite_allSequenceIndexes(emuDBhandle)
  ## @todo fail if the user is trying to add sequence indexes that are already
  ## there - or accept it silently, producing undefined order?
  
  ##
  ## Move data from temporary items table back to normal table
  ##
  moveback_annotCrudTmpTables(emuDBhandle)
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   verbose = verbose)
  }
  
  invisible(NULL)
}

## A function called read_itemsInLevel will not exist - query() is the function that does the job.
# read_itemsInLevel = function (...)


##' Update items programmatically
##' @export
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToUpdate A data frame with the columns
##' \itemize{
##' \item \code{session},
##' \item \code{bundle},
##' \item \code{level},
##' \item \code{start_item_seq_idx} (\code{start_item_seq_idx} is used instead of e.g.
##' \code{seq_idx} so that the result of a \code{\link{query}} call can be used directly. 
##' \code{\link{query}} can return a sequence of items defined by \code{start_item_seq_idx} 
##' and \code{end_item_seq_idx} which have the same value if single items are returned), 
##' \item \code{attribute}, and
##' \item \code{labels}.
##' }
##' *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (natural-valued), all other columns
##' must be of type character.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' 
##' @importFrom rlang .data
update_itemsInLevel = function (emuDBhandle,
                                itemsToUpdate,
                                rewriteAllAnnots = TRUE,
                                verbose = TRUE) {
  
  
  ##
  ## Find the index of each attribute definition on its respective level
  ##
  itemsToUpdate$label_index = get_labelIndex(emuDBhandle = emuDBhandle,
                                             levelName = itemsToUpdate$level,
                                             attributeName = itemsToUpdate$attribute)
  
  
  ##
  ## First thing, make sure all the items whose labels are gonna be changed do exist
  ##
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("SELECT count(*) ",
           "FROM items ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " AND level = ? ",
           " AND seq_idx = ?"))
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      itemsToUpdate$level,
      itemsToUpdate$start_item_seq_idx
    )
  )
  
  existenceMatrix = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  if (!all(existenceMatrix)) {
    total = nrow(itemsToUpdate)
    notFound = length(existenceMatrix[existenceMatrix == 0, ])
    
    warning (paste(
      "Error:",
      notFound,
      "of the",
      total,
      "specified items do/does not exist"
    ))
    
    if (verbose) {
      warning(itemsToUpdate[existenceMatrix == 0, ])
    } else {
      warning ("Set verbose to TRUE to see them listed.")
    }
    
    return (invisible(NULL))
  }
  
  #  get item_ids of matching entries
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("SELECT item_id ",
           "FROM items ",
           "WHERE db_uuid = ? ",
           " AND session = ? ",
           " AND bundle = ? ",
           " AND level = ? ",
           " AND seq_idx = ?"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      itemsToUpdate$level,
      itemsToUpdate$start_item_seq_idx
    )
  )
  
  item_id_list = DBI::dbFetch(statement)
  DBI::dbClearResult(statement)
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("INSERT OR REPLACE INTO labels (",
           " db_uuid, ",
           " session, ",
           " bundle, ",
           " item_id, ",
           " label_idx, ",
           " name, ",
           " label ",
           ") VALUES (?, ?, ?, ?, ?, ?, ?)"))
  # rename labels column to label to match labels SQL table column name
  colnames(itemsToUpdate)[colnames(itemsToUpdate) == "labels"] <- "label"
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(itemsToUpdate)),
      itemsToUpdate$session,
      itemsToUpdate$bundle,
      item_id_list$item_id,
      itemsToUpdate$label_index,
      itemsToUpdate$attribute,
      itemsToUpdate$label
    )
  )
  rowsAffected = DBI::dbGetRowsAffected(statement)
  DBI::dbClearResult(statement)
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   bundles = unique(data.frame(session = itemsToUpdate$session,
                                               name = itemsToUpdate$bundle)), 
                   verbose = verbose)
  }
  
  invisible(NULL)
}



##' Delete new items programmatically
##' 
##' @description Allows to delete annotation items programmatically.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param itemsToDelete A data frame with the columns
##' \itemize{
##' \item\code{session},
##' \item\code{bundle},
##' \item\code{level}, and
##' \item\code{sequenceIndex}.
##' }
##' *None* of the columns should be factors.
##' \code{sequenceIndex} must be numeric (natural-valued), all other columns must
##' be of type character.
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' @export
delete_itemsInLevel = function (emuDBhandle,
                                itemsToDelete,
                                rewriteAllAnnots = TRUE,
                                verbose = TRUE) {
  stop("Not implemented yet!")
  print(itemsToDelete)
  
  ##
  ## ...
  ##
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   verbose = verbose)
  }
  
  invisible(NULL)
}

##' create links between items
##'
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param links data.frame like object containing linking information. The required columns
##' are: 
##' \itemize{
##' \item \code{session}: 
##' \item \code{bundle}
##' \item \code{from_id}
##' \item \code{to_id}
##' }
##' @param rewriteAllAnnots should changes be written to file system (_annot.json
##'                         files) (intended for expert use only)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' @export
create_links = function(emuDBhandle,
                        links,
                        rewriteAllAnnots = TRUE,
                        verbose = TRUE) {
  
  input_key <- readline(prompt = "Currently no checks are performed so use at own risk! Do you wish to continue anyway (y/N)?")
  if(input_key != "y") return()
  
  # todo check if items are all present in database
  # todo check that all links are valid
  # todo check that no links cross each other
  
  
  statement = DBI::dbSendStatement(
    emuDBhandle$connection,
    paste0("INSERT INTO links (",
           " db_uuid, ",
           " session, ",
           " bundle, ",
           " from_id, ",
           " to_id, ",
           " label",
           ") VALUES (?, ?, ?, ?, ?, NULL)"))
  
  DBI::dbBind(
    statement,
    list(
      rep(emuDBhandle$UUID, nrow(links)),
      links$session,
      links$bundle,
      links$from_id,
      links$to_id
    )
  ) 
  
  DBI::dbClearResult(statement)
  
  if (rewriteAllAnnots) {
    rewrite_annots(emuDBhandle, 
                   verbose = verbose)
  }
  
  invisible(NULL)
}


#######################
# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-annotations_crud.R')
# test_file('tests/testthat/test_zzz_cleanUp.R')
