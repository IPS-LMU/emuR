
#####################################################
# functions used to build various path combinations
# plus helper functions

get_levelNameForAttributeName <- function(emuDBhandle, attributeName){
  DBconfig = load_DBconfig(emuDBhandle)
  for(lvlD in DBconfig$levelDefinitions){
    aNames = character(0)
    for(ad in lvlD$attributeDefinitions){
      aNames = c(aNames, ad[['name']])
      if(attributeName %in% aNames){
        return(lvlD[['name']])
      }
    }
  }
  return(NULL)
}


get_allAttributeNames<-function(emuDBhandle){
  DBconfig = load_DBconfig(emuDBhandle)
  aNames=character(0)
  for(lvlD in DBconfig$levelDefinitions){
    for(ad in lvlD$attributeDefinitions){
      aNames=c(aNames,ad$name)
    }
    
  }
  return(aNames)
}


get_linkLevelChildrenNames<-function(schema, superlevelName){
  chNames = character(0)
  for(ld in schema[['linkDefinitions']]){
    if(ld[['superlevelName']] == superlevelName){
      chNames=c(chNames, ld[['sublevelName']])
    }
  }
  return(chNames)
}

expand_linkPath <- function(p){
  expPath = list()
  pLen = length(p)
  if(pLen == 1){
    return(list())
  }
  expPath[[length(expPath)+1L]] = p
  expPath = c(expPath, expand_linkPath(p[1:(pLen-1)]))
  return(expPath)
}

## build all hierarchy paths including partial paths
## @return list containing paths and subpaths
build_allHierarchyPaths <- function(schema){
  extLds = list()
  for(ld in schema[['levelDefinitions']]){
    lName = ld[['name']]
    pathes = build_sublevelPathes(schema, lName)
    for(p in pathes){
      extLds = c(extLds, expand_linkPath(p))
    }
  }
  return(unique(extLds))
}


build_sublevelPathes <- function(DBconfig, levelName){
  pathes = list()
  chNames = get_linkLevelChildrenNames(DBconfig, levelName)
  if(length(chNames) == 0){
    pathes[[length(pathes) + 1L]] = c(levelName)
  }else{
    for(chName in chNames){
      chPathes = build_sublevelPathes(DBconfig, chName)
      for(chPath in chPathes){
        pathes[[length(pathes)+1L]] = c(levelName,chPath)
      }
    }
  }
  return(pathes)
}


build_levelPathes <- function(emuDBhandle){
  DBconfig = load_DBconfig(emuDBhandle)
  pathes = list()
  chNames = character(0)
  for(l in DBconfig$levelDefinitions){
    lPathes = build_sublevelPathes(DBconfig, l[['name']])
    pathes = c(pathes, lPathes)
  }
  return(pathes)
}

# get all paths through hierarchy connecting two levels
get_hierPathsConnectingLevels <- function(emuDBhandle, levelName1, levelName2){
  
  allHierPaths = build_allHierarchyPaths(load_DBconfig(emuDBhandle))
  
  conHierPaths = list()
  
  
  for(p in allHierPaths){
    # assume levelName1 is above levelName2
    if(p[1] == levelName1 & p[length(p)] == levelName2){
      conHierPaths[[length(conHierPaths) + 1]] = p
    }
    # assume levelName2 is above levelName1
    if(p[1] == levelName2 & p[length(p)] == levelName1){
      conHierPaths[[length(conHierPaths) + 1]] = p
    }
    
  }
  
  return(conHierPaths)
}



# builds "extended" link definitions
# lists link definitionsfor every possible directed connection between levels
# returns list of character vectors 
# the first element of each character vector contains the super level name of the levelDefinition,
# the follwing elements contain all exetnded linked sub level names  
build_extLinkDefinitions <- function(emuDBhandle){
  lds = list()
  pathes = build_levelPathes(emuDBhandle)
  for(p in pathes){
    pLen = length(p)
    for(i in 1:pLen){
      ld = character(0)
      for(j in i:pLen){
        ld = c(ld,p[j])
      }
      lds[[length(lds)+1L]] = ld
    }
  }
  return(lds)
}


find_segmentLevels<-function(emuDBhandle, attrName){
  lvlNm = get_levelNameForAttributeName(emuDBhandle, attrName)
  extLnkDefs = build_extLinkDefinitions(emuDBhandle)
  segLvlList=character(0)
  for(extLnkDef in extLnkDefs){
    if(extLnkDef[1]==lvlNm){
      for(trgLvlNm in extLnkDef[2:length(extLnkDef)]){
        
        trgLd=get_levelDefinition(emuDBhandle, trgLvlNm)
        if(trgLd['type']=='SEGMENT'){
          segLvlList=unique(c(segLvlList,trgLvlNm))
        }
      }
    }
  }
  return(segLvlList)
}

get_levelDefinition <- function(emuDBhandle, name){
  DBconfig = load_DBconfig(emuDBhandle)
  res = NULL
  for(ld in DBconfig$levelDefinitions){
    if(ld$name == name){
      res = ld
      break
    }
  }
  return(res)
}

###########################################
# DBconfig file handeling functions

## load function for _DBconfig.json file of emuDB
load_DBconfig <- function(emuDBhandle){
  dbCfgPath = file.path(emuDBhandle$basePath, paste0(emuDBhandle$dbName, database.schema.suffix))
  if(file.exists(dbCfgPath)){
    DBconfig = jsonlite::fromJSON(dbCfgPath, simplifyVector=FALSE)
  }else{
    stop(dbCfgPath, " does not seem to exist. This could be due to a bad 'name' entry in the DBconfig file. This field has to be the same as the name of the emuDB (directory & _DBconfig.json)")
  }
  return(DBconfig)
}

# store function for dbConfig
store_DBconfig <- function(emuDBhandle, dbConfig, basePath = NULL){
  if(is.null(basePath)){
    basePath = emuDBhandle$basePath
  }
  dbCfgPath = file.path(basePath, paste0(emuDBhandle$dbName, database.schema.suffix))
  json = jsonlite::toJSON(dbConfig, auto_unbox = TRUE, force = TRUE, pretty = TRUE)
  writeLines(json, dbCfgPath, useBytes = TRUE)
}


################################################################
################# CRUD DBconfig functions ######################
################################################################



###########################################
# CRUD operation for levelDefinitions



###################################################
# CRUD operations for attributeDefinitions

##' Add / List / Rename / Remove attribute definition to / of / from emuDB
##' 
##' @description Add / List / Rename / Remove database operation functions for attribute
##' definition to / of / from an existing level definition of an emuDB.
##' Attribute definitions can be viewed as definitions of
##' parallel labels for the annotational units (ITEMs) of the emuDB. 
##' Each level definition is required to have at least one 
##' default attribute definition that has the same name as the level definition
##' (automatically created by \code{\link{add_levelDefinition}}). For more 
##' information on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' Note that as with level definitions, an attribute definition to a level cannot be removed,
##' if it contains labels in the emuDB.
##' 
##' As the only one of these operations, \code{rename_attributeDefinition} can
##' also be used to manipulate (i.e. rename) a level definition. It is therefore
##' not necessary to specify the name of the level that the attribute definition
##' belongs to. While renaming a level or attribute definition, emuR will
##' (1) rewrite the levelDefinitions in DBconfig, (2) rewrite the
##' linkDefinitions in DBconfig, (3) rewrite the perspectives in DBconfig,
##' (4) rewrite the anagestConfig in DBconfig, and (5) rewrite all _annot.json
##' files. (5) May take quite a while, depending on the number of bundles in the
##' database.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param levelName name of level
##' @param name name of attributeDefinition
##' @param type type of attributeDefinition (currently only "STRING")
##' @param origAttrDef name of level/attribute definition in emuDB that is to be changed
##' @param newAttrDef new name that shall be assigned to the level/attribute definition
##' @param rewriteAllAnnots should changes be written to file system (_annot.json files) (intended for expert use only)
##' @param force delete all attribute definitions in annotations (== label entries)
##' @param verbose if set to \code{TRUE}, more status messages are printed
##' @keywords emuDB database DBconfig Emu 
##' @name AddListRenameRemoveAttributeDefinitions
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # add additional attribute definition to the "Phonetic" level
##' # of the ae emuDB that will contain the UTF8 IPA
##' # symbols of the phonetic transcriptions
##' add_attributeDefinition(emuDBhandle = ae,
##'                         levelName = "Phonetic",
##'                         name = "IPA-UTF8")
##'                         
##' # list attribute definitions for level "Word"
##' # of the ae emuDB
##' list_attributeDefinitions(emuDBhandle = ae, 
##'                           levelName = "Word")
##' 
##' # remove newly added attributeDefinition
##' remove_attributeDefinition(emuDBhandle = ae,
##'                            levelName = "Phonetic",
##'                            name = "IPA-UTF8")
##' }
##' 
NULL

##' @rdname AddListRenameRemoveAttributeDefinitions
##' @export
add_attributeDefinition <- function(emuDBhandle, levelName, 
                                    name, type = "STRING", 
                                    rewriteAllAnnots = TRUE, verbose = TRUE){
  
  internal_add_attributeDefinition(emuDBhandle, levelName, 
                                   name, type = "STRING", 
                                   rewriteAllAnnots = TRUE, verbose = verbose)
  
  
}

internal_add_attributeDefinition <- function(emuDBhandle, levelName, 
                                             name, 
                                             type = "STRING", 
                                             rewriteAllAnnots = TRUE, 
                                             verbose = TRUE, 
                                             insertLabels = TRUE){
  if(type != "STRING"){
    stop("Currently only attributeDefinition of type 'STRING' allowed")
  }
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  df = list_attributeDefinitions(emuDBhandle, levelName)
  
  labelIdx = -1
  if(!(name %in% df$name)){
    for(i in 1:length(dbConfig$levelDefinitions)){
      if(dbConfig$levelDefinitions[[i]]$name == levelName){
        labelIdx = length(dbConfig$levelDefinitions[[i]]$attributeDefinitions) + 1
        dbConfig$levelDefinitions[[i]]$attributeDefinitions[[labelIdx]] = list(name = name, type = type)
        break
      }
    }
  }else{
    stop(paste0("attributeDefinition with name '", name, "' already present in level '", levelName, "'"))
  }
  
  # add to labels table
  if(insertLabels){
    DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO labels ",
                                                   "SELECT db_uuid, session , bundle, item_id, ", labelIdx, " AS label_idx, '", name, "' AS name, '' AS label ",
                                                   "FROM labels ",
                                                   "WHERE name = '", levelName, "' AND label_idx = 1 "))
  }
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
  
  if(rewriteAllAnnots){
    rewrite_allAnnots(emuDBhandle, verbose = verbose)
  }
  
}



##' @rdname AddListRenameRemoveAttributeDefinitions
##' @export
list_attributeDefinitions <- function(emuDBhandle, levelName){
  
  ld = get_levelDefinition(emuDBhandle, levelName)
  
  if(length(ld$attributeDefinitions) > 1){
    df = data.frame(name = character(), 
                    type = character(), 
                    hasLabelGroups = logical(), 
                    hasLegalLabels = logical(), 
                    stringsAsFactors = F)
    for(ad in ld$attributeDefinitions){
      df = rbind(df, df = data.frame(name = ad$name, 
                                     type = ad$type, 
                                     hasLabelGroups = !is.null(ad$labelGroups),
                                     hasLegalLabels = !is.null(ad$legalLabels),
                                     stringsAsFactors = F))
    }
  }else{
    df <- data.frame(name=ld$attributeDefinitions[[1]]$name, 
                     type=ld$attributeDefinitions[[1]]$type,
                     hasLabelGroups = !is.null(ld$attributeDefinitions[[1]]$labelGroups),
                     hasLegalLabels = !is.null(ld$attributeDefinitions[[1]]$legalLabels),
                     stringsAsFactors = F)
  }
  rownames(df) <- NULL
  return(df)
}


##' @rdname AddListRenameRemoveAttributeDefinitions
##' @export
rename_attributeDefinition <- function(emuDBhandle, origAttrDef, newAttrDef, verbose = TRUE) {
  
  #############################
  # check input parameters
  if(class(origAttrDef) != "character" | class(newAttrDef) != "character" | length(origAttrDef) != 1 | length(newAttrDef) != 1){
    stop("origAttrDef and newAttrDef have to be character vectors with only one item!")  
  }
  
  allAttrNames = get_allAttributeNames(emuDBhandle)
  if(!origAttrDef %in% allAttrNames){
    stop(paste0("Attribute definition: ", origAttrDef, " not found in emuDB! The available attribute definitions are: ", paste0(allAttrNames, collapse = "; ")))
  }
  
  if(newAttrDef %in% allAttrNames){
    stop(paste0("Attribute definition: ", newAttrDef, " is already defined in emuDB! You need to specify unique names!"))
  }
  
  #############################
  # adjust DBconfig
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  
  dbConfig$linkDefinitions = lapply (
    dbConfig$linkDefinitions,
    function (linkDef) {
      if (linkDef$superlevelName == origAttrDef) {
        linkDef$superlevelName = newAttrDef
      }
      if (linkDef$sublevelName == origAttrDef) {
        linkDef$sublevelName = newAttrDef
      }
      
      linkDef
    }
  )
  
  dbConfig$EMUwebAppConfig$perspectives = lapply (
    dbConfig$EMUwebAppConfig$perspectives,
    function (perspective) {
      perspective$levelCanvases$order = lapply(
        perspective$levelCanvases$order,
        function (canvas) {
          if (canvas == origAttrDef) {
            newAttrDef
          } else {
            canvas
          }
        }
      )
      
      perspective
    }
  )
  
  dbConfig$levelDefinitions = lapply (
    dbConfig$levelDefinitions,
    
    function (lvlDef) {
      # If lvlDef references the level to be renamed in its anagest config,
      # adjust that
      if(!is.null(lvlDef$anagestConfig)){
        if (lvlDef$anagestConfig$autoLinkLevelName == origAttrDef) {
          lvlDef$anagestConfig$autoLinkLevelName = newAttrDef
        }
      }
      # If lvlDef *is* the level to be renamed, adjust that
      if (lvlDef$name == origAttrDef) {
        lvlDef$name = newAttrDef
        lvlDef$attributeDefinitions[[1]]$name = newAttrDef
      } else {
        # If lvlDef is not the level to be renamed, search lvlDef's attribute
        # definitions. One of them may be the one to be renamed.
        lvlDef$attributeDefinitions = lapply(
          lvlDef$attributeDefinitions,
          function (attrDef) {
            if (attrDef$name == origAttrDef) {
              attrDef$name = newAttrDef
            }
            
            attrDef
          }
        )
      }
      
      # Return the (possibly modified) lvlDef so lapply knows the new value
      lvlDef
    }
  )
  
  
  #
  #############################
  if(verbose){
    cat("\n  INFO: creating temporary index...\n")
  }
  
  # create temp index
  DBI::dbExecute(emuDBhandle$connection, paste0("CREATE INDEX IF NOT EXISTS level_rename_tmp_idx ON items(db_uuid, level)"))
  
  
  if(verbose){
    cat("\n  INFO: renaming attribute definition\n")
  }
  
  # transaction start
  DBI::dbBegin(emuDBhandle$connection)
  
  DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE items SET level = '", newAttrDef, "' ",
                                                 "WHERE db_uuid='", emuDBhandle$UUID, "' ",
                                                 "AND level = '", origAttrDef, "'"))
  
  DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE labels SET name = '", newAttrDef, "' ",
                                                 "WHERE db_uuid='", emuDBhandle$UUID, "' ",
                                                 "AND name = '", origAttrDef, "'"))
  
  # transaction end
  DBI::dbCommit(emuDBhandle$connection)
  
  if(verbose){
    cat("\n  INFO: removing temporary index...\n")
  }  
  # remove temp index
  DBI::dbExecute(emuDBhandle$connection, paste0("DROP INDEX IF EXISTS level_rename_tmp_idx"))
  
  store_DBconfig(emuDBhandle, dbConfig)
  rewrite_allAnnots(emuDBhandle, verbose = verbose)
}


##' @rdname AddListRenameRemoveAttributeDefinitions
##' @export
remove_attributeDefinition <- function(emuDBhandle, 
                                       levelName, 
                                       name,
                                       force = FALSE,
                                       rewriteAllAnnots = TRUE,
                                       verbose = TRUE){
  
  if(levelName == name){
    stop("Can not remove primary attributeDefinition (attributeDefinition with same name as level)")
  }
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  ld = get_levelDefinition(emuDBhandle, levelName)
  
  if(!force){
    # check if instances are present
    qRes = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT * FROM items AS it, labels AS lb WHERE ",
                                                          "it.db_uuid = lb.db_uuid AND ", 
                                                          "it.session = lb.session AND ", 
                                                          "it.bundle = lb.bundle AND ",
                                                          "it.item_id = lb.item_id AND ",
                                                          "it.level = '", levelName, "' AND ",
                                                          "lb.name = '", name, "'"))
    if(nrow(qRes) > 0){
      stop("Can not remove attributeDefinition if there are labels present")
    }
  }else{
    if(verbose){
      answ <- readline(prompt = "Are you sure you wish to remove all labels that are associated with this attributeDefinition (y/n): ")
      
      if(!answ %in% c("y", "Y")){
        stop("removal of attributeDefinition aborted")
      }
    }
    # delete all labels
    DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM labels ",
                                                   "WHERE EXISTS( ",
                                                   "SELECT * FROM items i ",
                                                   "WHERE i.db_uuid='", emuDBhandle$UUID, "' ",
                                                   "AND i.session = labels.session AND i.bundle = labels.bundle AND i.item_id = labels.item_id ",
                                                   "AND i.level='", levelName, "' AND labels.name ='", name, "' ",
                                                   ")"
    ))
    
    
  }
  
  levDefIdx = NULL
  for(i in 1:length(dbConfig$levelDefinitions)){
    if(dbConfig$levelDefinitions[[i]]$name == levelName){
      levDefIdx = i
      break
    }
  }
  
  for(i in 1:length(dbConfig$levelDefinitions[[levDefIdx]]$attributeDefinitions)){
    if(dbConfig$levelDefinitions[[levDefIdx]]$attributeDefinitions[[i]]$name == name){
      dbConfig$levelDefinitions[[levDefIdx]]$attributeDefinitions[[i]] = NULL
      break
    }
  }
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
  if(rewriteAllAnnots){
    rewrite_allAnnots(emuDBhandle, verbose = verbose)
  }
}

###################################################
# CRUD operations for legalLabels

##' Set / Get / Remove legal labels of attributeDefinition of emuDB
##' 
##' Set / Get / Remove legal labels of a specific attributeDefinition of a emuDB. 
##' The legal labels are a character vector of strings
##' that specifies the labels that are legal (i.e. allowed / valid) for the given attribute. 
##' As the EMU-webApp won't allow the annotator to enter any labels that are not 
##' specified in this array, this is a simple way of assuring that a level 
##' has a consistent label set. For more information 
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' Note that defining legal labels for an attributeDefinition does not imply that the 
##' existing labels are checked for being 'legal' in the emuDB.
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param levelName name of level
##' @param attributeDefinitionName name of attributeDefinition (can be and often is the level name)
##' @param legalLabels character vector of labels
##' @keywords emuDB database schema Emu
##' @name SetGetRemoveLegalLabels
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' legalPhoneticLabels = c("V", "m", "N", "s", "t", "H", "@:", "f", "r", 
##'                         "E", "n", "z", "S", "i:", "w", "@", "k", "I", "d", 
##'                         "db", "j", "u:", "dH", "l", "ai", "O", "D", "o:", "v")
##' 
##' # set legal labels of the 
##' # default "Phonetic" attributeDefinition of
##' # the "Phonetic" level of ae emuDB
##' set_legalLabels(emuDBhandle = ae, 
##'                 levelName = "Phonetic",
##'                 attributeDefinitionName = "Phonetic",
##'                 legalLabels = legalPhoneticLabels)
##' 
##' # get legal labels of the 
##' # default "Phonetic" attributeDefinition of
##' # the "Phonetic" level of ae emuDB
##' get_legalLabels(emuDBhandle = ae, 
##'                 levelName = "Phonetic", 
##'                 attributeDefinitionName = "Phonetic")
##'                 
##' 
##' # remove legal labels of the 
##' # default "Phonetic" attributeDefinition of
##' # the "Phonetic" level of ae emuDB
##' remove_legalLabels(emuDBhandle = ae, 
##'                    levelName = "Phonetic", 
##'                    attributeDefinitionName = "Phonetic")
##'                 
##' }
##' 
NULL

##' @rdname SetGetRemoveLegalLabels
##' @export
set_legalLabels <- function(emuDBhandle,
                            levelName,
                            attributeDefinitionName,
                            legalLabels){
  
  if(!is.null(legalLabels) & class(legalLabels) != "character"){
    stop("legalLables must be of class 'character'")
  }
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  for(i in 1:length(dbConfig$levelDefinitions)){
    for(j in 1:length(dbConfig$levelDefinitions[[i]]$attributeDefinitions)){
      if(dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$name == attributeDefinitionName){
        dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$legalLabels = legalLabels
      }
    }
  }
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
  
}


##' @rdname SetGetRemoveLegalLabels
##' @export
get_legalLabels <- function(emuDBhandle,
                            levelName,
                            attributeDefinitionName){
  
  ld = get_levelDefinition(emuDBhandle, levelName)
  
  ll = NULL
  for(ad in ld$attributeDefinitions){
    if(ad$name == attributeDefinitionName){
      if(!is.null(ad$legalLabels)){
        ll = unlist(ad$legalLabels)
      }else{
        ll = NA
      }
    }
  }
  
  return(ll)
}


##' @rdname SetGetRemoveLegalLabels
##' @export
remove_legalLabels <- function(emuDBhandle,
                               levelName,
                               attributeDefinitionName){
  
  # remove by setting to NULL
  set_legalLabels(emuDBhandle,
                  levelName,
                  attributeDefinitionName,
                  legalLabels = NULL)
}

###################################################
# CRUD operations for attributeDefinition$labelGroups

##' Add / List / Remove labelGroup to / of / from attributeDefinition of emuDB
##' 
##' Add / List / Remove label group to / of / from a specific attribute definition. 
##' This label group can be used as a short hand  
##' to reference groups of labels specific
##' to an attribute definition (compared to global label groups that 
##' are added by \code{\link{add_labelGroup}}) in a 
##' \code{\link{query}}. A common example would be to
##' add a label group for something like the phonetic
##' category of nasals to be able reference them 
##' as "nasals" in a \code{\link{query}}. For more information 
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' 
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param levelName name of level
##' @param attributeDefinitionName name of attributeDefinition
##' @param labelGroupName name of label group
##' @param labelGroupValues character vector of labels
##' @keywords emuDB database schema Emu
##' @seealso add_labelGroup
##' @name AddListRemoveAttrDefLabelGroup
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' sampaNasals = c("m", "F", "n", "J", "N")
##' 
##' # add these values to the default Phonetic attribute
##' # definition of the Phonetic level of the ae emuDB
##' add_attrDefLabelGroup(emuDBhandle = ae,
##'                       levelName = "Phonetic",
##'                       attributeDefinitionName = "Phonetic",
##'                       labelGroupName = "sampaNasals",
##'                       labelGroupValues = sampaNasals)
##' 
##' # query the labelGroup
##' query(ae, "Phonetic=sampaNasals")
##' 
##' 
##' # list attribute definition label groups
##' # of attributeDefinition "Phonetic" of the level "Phonetic"
##' # of the ae emuDB
##' list_attrDefLabelGroups(emuDBhandle = ae, 
##'                         levelName = "Phonetic" , 
##'                         attributeDefinitionName = "Phonetic")
##' 
##' # remove the newly added attrDefLabelGroup
##' remove_attrDefLabelGroup(emuDBhandle = ae,
##'                          levelName = "Phonetic",
##'                          attributeDefinitionName = "Phonetic",
##'                          labelGroupName = "sampaNasals")
##' 
##' }
##' 
NULL

##' @rdname AddListRemoveAttrDefLabelGroup
##' @export
add_attrDefLabelGroup <- function(emuDBhandle,
                                  levelName,
                                  attributeDefinitionName, 
                                  labelGroupName,
                                  labelGroupValues){
  
  dbConfig = load_DBconfig(emuDBhandle)
  curLgs = list_attrDefLabelGroups(emuDBhandle, 
                                   levelName, 
                                   attributeDefinitionName)
  
  # wrap in list if array of length 1 -> so converted to json
  if(length(labelGroupValues) ==1 ){
    labelGroupValues = list(labelGroupValues)
  }
  
  if(labelGroupName %in% curLgs$name){
    stop("labelGroupName '", labelGroupName ,"' already exists!")
  }
  for(i in 1:length(dbConfig$levelDefinitions)){
    for(j in 1:length(dbConfig$levelDefinitions[[i]]$attributeDefinitions)){
      if(dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$name == attributeDefinitionName){
        l = length(dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups)
        dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups[[l + 1]] = list(name = labelGroupName, 
                                                                                             values = labelGroupValues)
      }
    }
  }
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
}

##' @rdname AddListRemoveAttrDefLabelGroup
##' @export
list_attrDefLabelGroups <- function(emuDBhandle,
                                    levelName,
                                    attributeDefinitionName){
  
  ld = get_levelDefinition(emuDBhandle, levelName)
  
  df = data.frame(name = character(), 
                  values = character(),
                  stringsAsFactors = F)
  for(ad in ld$attributeDefinitions){
    if(ad$name == attributeDefinitionName){
      if(!is.null(ad$labelGroups)){
        for(lg in ad$labelGroups){
          df = rbind(df, data.frame(name = lg$name,
                                    values = paste0(lg$values, collapse = "; "), 
                                    stringsAsFactors = F ))
        }
      }
    }
  }
  
  return(df)
}


##' @rdname AddListRemoveAttrDefLabelGroup
##' @export
remove_attrDefLabelGroup <- function(emuDBhandle,
                                     levelName,
                                     attributeDefinitionName, 
                                     labelGroupName){
  
  dbConfig = load_DBconfig(emuDBhandle)
  curLgs = list_attrDefLabelGroups(emuDBhandle, 
                                   levelName, 
                                   attributeDefinitionName)
  
  if(!labelGroupName %in% curLgs$name){
    stop("labelGroupName '", labelGroupName ,"' does not exists!")
  }
  
  for(i in 1:length(dbConfig$levelDefinitions)){
    for(j in 1:length(dbConfig$levelDefinitions[[i]]$attributeDefinitions)){
      if(dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$name == attributeDefinitionName){
        l = length(dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups)
        dbConfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups[[l]] = NULL
      }
    }
  }
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
  
}



# FOR DEVELOPMENT 
# library('testthat') 
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-database.DBconfig.R')