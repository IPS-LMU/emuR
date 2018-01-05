##' Functions for manipulating the definitions of links between levels in an EmuDB
##' 
##'  A link definition specifies the relationship between two levels, the
##' super-level and the sub-level. Together, link definitions of a EmuDB provides the 
##' hierarchical structure of the database. For more information
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' 
##' There are three types of links:
##' \describe{
##' \item{\code{ONE_TO_MANY}}{A single ITEM of the super-level can be linked to multiple ITEMs of the sub-level. This is a frequently occuring link type, and may for instance be used for  }
##' \item{\code{MANY_TO_MANY}}{Multiple ITEMs of the super-level can be linked to multiple ITEMs of the sub-level.}
##' \item{\code{ONE_TO_ONE}}{A single ITEM of the super-level can be linked to a single ITEM of the sub-level.}
##' }
##' 
##' The \code{"ONE_TO_MANY"} relationship between levels are frequently used in EmuDB databases,
##' and may be used for instance in the case where an ITEM on an "Utterance" level
##' dominates a number of ITEMs on a sublevel "Word". \code{MANY_TO_MANY} links are less common
##' but may be required for encoding for instance the relationships between acoustic 
##' phonetic subcomponents of a segment and a theoretically defined ITEM on a super-level. 
##' See the case of \code{Phonetic} and \code{Phoneme} levels in the \code{ae} demo database 
##' for an example of this link type. If labels on the \code{Phonetic} level had enocoded
##' a full IPA specification of a segment in the \code{ae} demo database, then perhaps a
##' \code{ONE_TO_ONE} link relationship between \code{Phonetic} and \code{Phoneme} would have
##' been more appropriate.
##' 
##' Regardless of link type, no links are allowed to cross any other links. Further, a 
##' linkDefinition can not be removed, if there are links present in the EmuDB.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param type type of linkDefinition (either \code{"ONE_TO_MANY"}, \code{"MANY_TO_MANY"} or \code{"ONE_TO_ONE"})
##' @param superlevelName name of super-level of linkDefinition
##' @param sublevelName name of sub-level of linkDefinition
##' @param force delete all links belonging to the linkDefinition (\strong{USE WITH CAUTION! VERY INVASIVE AKTION!})
##' @param verbose be verbose. Ask to delete links if \code{force} is \code{TRUE}.
##' @name linkDefinitions
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded emuDB that was converted
##' # using the convert_TextGridCollection function called myTGcolDB
##' # (see ?load_emuDB and ?convert_TextGridCollection for more information)
##' 
##' # add link definition from super-level "Phoneme"
##' # to sub-level "Phonetic" of type "ONE_TO_MANY"
##' # for myTGcolDB emuDB
##' add_linkDefinition(emuDBhandle = myTGcolDB,
##'                    type = "ONE_TO_MANY",
##'                    superlevelName = "Phoneme",
##'                    sublevelName = "Phonetic")
##' 
##' # list link definitions for myTGcolDB emuDB
##' list_linkDefinitions(emuDBhandle = myTGcolDB)
##' 
##' # remove newly added link definition
##' remove_linkDefinition(emuDBhandle = myTGcolDB,
##'                       superlevelName = "Phoneme",
##'                       sublevelName = "Phonetic")
##' 
##' 
##' }
NULL

##' @rdname linkDefinitions
##' @export
add_linkDefinition <- function(emuDBhandle, 
                               type,
                               superlevelName,
                               sublevelName){
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  allowedTypes = c("ONE_TO_MANY", "MANY_TO_MANY", "ONE_TO_ONE")
  
  if(!type %in% allowedTypes){
    stop("Only the following types permitted: ", paste(allowedTypes, collapse = '; '))
  }
  
  curLds = list_linkDefinitions(emuDBhandle)
  
  # check if level is defined
  curLevs = list_levelDefinitions(emuDBhandle)
  if(!any(curLevs$name == superlevelName) | !any(curLevs$name == sublevelName)){
    stop("Either superlevelName or sublevelName are not defined")
  }
  
  
  # check if link between levels already exists
  if(any(curLds$superlevelName == superlevelName & curLds$sublevelName == sublevelName)){
    stop("linkDefinition already exists for superlevelName: '", 
         superlevelName, "' and sublevelName: '", sublevelName, "'")
  }
  
  # check that super level isn't of type EVENT -> validates "Events can never be 'parents' in a domination relationship" constraint
  superLevDev = curLevs %>% dplyr::filter_(~(name==superlevelName))
  if(superLevDev$type == "EVENT"){
    stop("levels of type 'EVENT' are not allowed to be super levels (== parents) in a domination relationship!")
  }
  
  l = length(dbConfig$linkDefinitions)
  dbConfig$linkDefinitions[[l + 1]] = list(type = type, 
                                           superlevelName = superlevelName,
                                           sublevelName = sublevelName)
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
  
}


##' @rdname linkDefinitions
##' @export
list_linkDefinitions <- function(emuDBhandle){
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  df = data.frame(type = character(),
                  superlevelName = character(),
                  sublevelName = character(),
                  stringsAsFactors = F)
  
  for(ld in dbConfig$linkDefinitions){
    df = rbind(df, data.frame(type = ld$type,
                              superlevelName = ld$superlevelName,
                              sublevelName = ld$sublevelName,
                              stringsAsFactors = F))
  }
  # NULL out df
  if(nrow(df) == 0){
    df = NULL
  }
  return(df)
  
}


##' @rdname linkDefinitions
##' @export
remove_linkDefinition <- function(emuDBhandle, 
                                  superlevelName,
                                  sublevelName,
                                  force = FALSE,
                                  verbose = TRUE){
  
  dbConfig = load_DBconfig(emuDBhandle)
  curLds = list_linkDefinitions(emuDBhandle)
  
  # check if linkDef exists
  if(!any(curLds$superlevelName == superlevelName & curLds$sublevelName == sublevelName)){
    stop("No linkDefinition found for superlevelName '", superlevelName, 
         "' and sublevelName '", sublevelName, "'")
  }
  if(!force){
    # check if links are present
    res = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM ",
                                                         "links ",
                                                         "INNER JOIN (SELECT * FROM items WHERE level = '", superlevelName, "' AND db_uuid = '", dbConfig$UUID, "') as superItems", 
                                                         "    ON links.from_id = superItems.item_id ",
                                                         "       AND links.db_uuid = superItems.db_uuid ",
                                                         "       AND links.session = superItems.session ",
                                                         "       AND links.bundle = superItems.bundle ",
                                                         "INNER JOIN (SELECT * FROM items WHERE level = '", sublevelName, "' AND db_uuid = '", dbConfig$UUID, "') as subItems", 
                                                         "    ON links.to_id = subItems.item_id ",
                                                         "       AND links.db_uuid = subItems.db_uuid ",
                                                         "       AND links.session = subItems.session ",
                                                         "       AND links.bundle = subItems.bundle ",
                                                         "WHERE links.db_uuid = '", emuDBhandle$UUID, "'"))
    
    if(nrow(res) != 0){
      stop("linkDefinition can not be remove as there are links present")
    }
  }else{
    
    if(verbose){
      answ <- readline(prompt="Are you sure you wish to remove all links that are associated with this linkDefinition (y/n): ")
      
      if(!answ %in% c("y", "Y")){
        stop("removal of linkDefinition incl. associated links aborted")
      }
    }
    # delete all links belonging to linkDef
    DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM links ",
                                                  "WHERE EXISTS( ",
                                                  "SELECT * FROM items i_from, items i_to ",
                                                  "WHERE i_from.db_uuid='", emuDBhandle$UUID, "' ",
                                                  "AND i_from.session = links.session AND i_from.bundle = links.bundle AND i_from.item_id = links.from_id ",
                                                  "AND i_from.level='", superlevelName, "' ",
                                                  "AND i_to.db_uuid='", emuDBhandle$UUID, "' ",
                                                  "AND i_to.session = links.session AND i_to.bundle = links.bundle AND i_to.item_id = links.to_id ",
                                                  "AND i_to.level='", sublevelName, "' ",
                                                  ")"
    ))
    
    
  }
  
  for(i in 1:length(dbConfig$linkDefinitions)){
    if(dbConfig$linkDefinitions[[i]]$superlevelName == superlevelName && dbConfig$linkDefinitions[[i]]$sublevelName == sublevelName){
      dbConfig$linkDefinitions[[i]] = NULL
      break
    }
  }
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
  if(force){
    rewrite_allAnnots(emuDBhandle, verbose = verbose)
  }
  
}


