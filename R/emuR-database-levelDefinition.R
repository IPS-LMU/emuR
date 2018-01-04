##' Functions used for manipulating level defintions of an emuDB
##' 
##' These functions allow the user to set add, list and remove level definitions for
##' an emuDB. 
##' 
##' A level is a more general term for what is often referred to as a "tier". 
##' It is more general in the sense that people usually 
##' expect tiers to contain time information. Levels 
##' can either contain time information if they are of the 
##' type "EVENT" or of the type "SEGMENT" but are timeless 
##' if they are of the type "ITEM". For more information 
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' 
##' A level cannot be removed if it contains instances of annotation items
##' or if it is linked to another level. Renaming a level definition 
##' can be done using \code{\link{rename_attributeDefinition}}.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param name The name of the level definition that should be added or removed to the emuDB.
##' @param type type of level definition ("SEGMENT","EVENT" or "ITEM")
##' @param rewriteAllAnnotations Should changes be written to file system? This operation will create a "_annot.json" file for each bundle and may take som time. This functionality is intended primarilly for expert use cases.
##' @param force Delete all items including links pointing to those items from the levels.
##' @param verbose Show progress bars and further information.
##' @keywords utilities
##' @name levelDefinition
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # Create the demo databases and load 'ae' 
##' # (see ?load_emuDB for more information)
##' create_emuRdemoData()
##' ae <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
##' # add level called "Phonetic2" to the ae emuDB
##' # that could for example contain the transcriptions of a second annotator
##' add_levelDefinition(emuDBhandle = ae,
##'                     name = "Phonetic2",
##'                     type = "SEGMENT")
##'                     
##' # list level definition of ae emuDB
##' list_levelDefinitions(emuDBhandle = ae)
##' 
##' # remove newly added level definition
##' remove_levelDefinitions(emuDBhandle = ae,
##'                         name = "Phonetic2")
##' }
##'
NULL 

##' @rdname levelDefinition
##' @export
add_levelDefinition <- function(emuDBhandle, name, type, rewriteAllAnnotations = FALSE, verbose = TRUE){
  allowedTypes = c('ITEM', 'SEGMENT', 'EVENT')
  # precheck type 
  if(!(type %in% allowedTypes)){
    stop('Bad type given! Type has to be either ', paste(allowedTypes, collapse = ' | ') )
  }
  levelDefinition=list(name = name, type = type, 
                       attributeDefinitions = list(list(name = name, type = 'STRING')))
  dbConfig = load_DBconfig(emuDBhandle)
  # check if level definition (name) already exists 
  for(ld in dbConfig$levelDefinitions){
    if(ld$name == levelDefinition$name){
      stop("Level definition:", levelDefinition$name," already exists in database ", emuDBhandle$dbName)
    }
  }
  # add
  dbConfig$levelDefinitions[[length(dbConfig$levelDefinitions) + 1]] = levelDefinition
  
  store_DBconfig(emuDBhandle, dbConfig)
  
  if(rewriteAllAnnotations){
    rewrite_allAnnots(emuDBhandle, verbose = verbose)
  }
  invisible(NULL)
}


##' @rdname levelDefinition
##' @export
list_levelDefinitions <- function(emuDBhandle){
  dbConfig = load_DBconfig(emuDBhandle)
  df <- data.frame(name = character(),
                   type = character(), 
                   nrOfAttrDefs = numeric(), 
                   stringsAsFactors = FALSE) 
  
  for(ld in dbConfig$levelDefinitions){
    df <- rbind(df, data.frame(name = ld$name, 
                               type = ld$type, 
                               nrOfAttrDefs = length(ld$attributeDefinitions),
                               attrDefNames = paste0(sapply(ld$attributeDefinitions, function(ad) paste0(ad$name, ";")), collapse = " "),
                               stringsAsFactors = FALSE))
  }
  # NULL out 
  if(nrow(df) == 0){
    df = NULL
  }
  return(df)
}


##' @rdname levelDefinition
##' @export
remove_levelDefinition<-function(emuDBhandle, name, rewriteAllAnnotations = FALSE, force = FALSE, verbose = TRUE){
  
  dbConfig = load_DBconfig(emuDBhandle)
  # check if level definition (name)exists 
  if(!any(sapply(dbConfig$levelDefinitions, function(ld) ld[['name']] == name))){
    stop("Level definition:", name, " does not exist in database ", dbConfig$name)
  }
  # check if level is referenced by link defintion
  for(lkd in dbConfig$linkDefinitions){
    if(lkd[['superlevelName']] == name | lkd[['sublevelName']] == name){
      lkdStr = toString(lkd)
      stop("Cannot remove level definition ", name, ". It is referenced by link definition: ", lkdStr)
    }
  }
  
  if(!force){
    # check if level is empty
    itemsDf = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items i WHERE \
                                                             i.db_uuid='", emuDBhandle$UUID, "' AND i.level='", name, "'"))
    itemsCnt = nrow(itemsDf)
    if(itemsCnt > 0){
      stop("Level is not empty. Remove items first to delete level ", name)
    }
  }else{
    
    if(verbose){
      answ <- readline(prompt="Are you sure you wish to remove all annotational items that are associated with this levelDefinition (y/n): ")
      
      if(!answ %in% c("y", "Y")){
        stop("removal of linkDefinition incl. associated links aborted")
      }
    }
    
    # delete all labels
    DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM labels ",
                                                  "WHERE EXISTS( ",
                                                  "SELECT * FROM items i ",
                                                  "WHERE i.db_uuid='", emuDBhandle$UUID, "' ",
                                                  "AND i.session = labels.session AND i.bundle = labels.bundle AND i.item_id = labels.item_id ",
                                                  "AND i.level='", name, "' ",
                                                  ")"
    ))
    
    # delete all items
    DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM items ",
                                                  "WHERE items.db_uuid='", emuDBhandle$UUID, "' AND items.level='", name, "'"))
  }
  
  # do removal
  newLvlDefs = list()
  for(lvlDef in dbConfig$levelDefinitions){
    if(lvlDef[['name']] != name){
      newLvlDefs[[length(newLvlDefs) + 1]] = lvlDef
    }
  }
  dbConfig$levelDefinitions = newLvlDefs
  
  # remove from levelCanvasOrder of EMUwebAppConfig if present
  for(i in 1:length(dbConfig$EMUwebAppConfig$perspectives)){
    if(any(dbConfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order == name)){
      # print(dbConfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order)
      found = dbConfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order == name
      # print(found)
      dbConfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order = dbConfig$EMUwebAppConfig$perspectives[[i]]$levelCanvases$order[!found]
    }
  }
  
  store_DBconfig(emuDBhandle, dbConfig)
  
  if(rewriteAllAnnotations){
    rewrite_allAnnots(emuDBhandle, verbose = verbose)
  }
  
  return(invisible(NULL))
}