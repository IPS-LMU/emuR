
##' Functions used for maipulation of label groups that are specific for a attribute of a level.
##' 
##' Is is possible to add label groups that are global to a EmuDB database using 
##' \code{\link{add_labelGroup}}). This label group can be used as a short hand  
##' to reference groups of labels in a call to \code{\link{query}}.
##' 
##' Sometimes, however, label groups are more effective if 
##' they are local to a specific attribute of a level. For instance, a label group for nasal
##' consonants would have to be different in a SAMPA transcription (a SAMPA attribute) and in an
##' UTF-8 IPA transcription (an IPA attribute).
##' 
##' For more informationon the structural elements of an emuDB, see \code{vignette(emuDB)}.
##' 
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param levelName The name of the level having the attribute defined wher you want to make a label group.
##' @param attributeDefinitionName The name of the attribute definition of the level \code{levelName} where the labelGroup should be applied.
##' @param labelGroupName The name of the label group.
##' @param labelGroupValues A vector of labels that should be made members of the label group.
##' @keywords utilities
##' @seealso labelGroup, levelDefinition
##' @name attrDefLabelGroup
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # Create the demo databases and load 'ae' 
##' # (see ?load_emuDB for more information)
##' create_emuRdemoData()
##' ae <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
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

##' @rdname attrDefLabelGroup
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

##' @rdname attrDefLabelGroup
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


##' @rdname attrDefLabelGroup
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
