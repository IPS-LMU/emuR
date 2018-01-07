##' Functions used for manipulating legal labels for attributed of levels of an EmuDB
##' 
##' 
##' Legal labels are strings that are legal (allowed or valid) labels for the given attribute 
##' of a level. As the EMU-webApp won't allow the annotator to enter any labels that are not 
##' specified in this array, this is a simple way of assuring that a level 
##' has a consistent label set. For more information 
##' on the structural elements of an EmuDB see \code{vignette(emuDB)}.
##' Note that defining legal labels for an attributeDefinition does not imply that the 
##' existing labels are checked in an EmuDB.
##' 
##' @param emuDBhandle An EmuDB handle as returned by \code{\link{load_emuDB}}
##' @param levelName The level name.
##' @param attributeDefinitionName The name of the attributeDefinition for which there should be a limited set of legal labels. This can be, and often is, the same as the name of the level.
##' @param legalLabels A vector of labels.
##' @keywords utilities
##' @name legalLabels
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # Create the demo databases and load 'ae' 
##' # (see ?load_emuDB for more information)
##' create_emuRdemoData()
##' ae <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
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

##' @rdname legalLabels
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


##' @rdname legalLabels
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


##' @rdname legalLabels
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


