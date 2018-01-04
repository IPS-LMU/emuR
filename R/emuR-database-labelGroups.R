##' Functions used for manipulating global label groups in an emuDB
##' 
##' Global label groups are aliases that may be used instead of labels in queries
##' database regardless of levels.
##' 
##' A common use case for global label groups are defined phonetic
##' categories, such as nasal consonants, so that they may be easilly referered to
##' and found (or excluded) when using \code{\link{query}}. 
##' 
##' For users transitioning from the legacy EMU system: Do not confuse a 
##' labelGroup with legal labels: a labelGroup 
##' had the unfortunate name 'legal labels' in the legacy EMU system.  
##' For more information on the structural elements of an emuDB 
##' see \code{vignette(emuDB)}.
##' 
##' @param emuDBhandle An emuDB handle as returned by \code{\link{load_emuDB}}
##' @param name The name of the label group
##' @param labels A character vector of labels
##' @keywords utilities
##' @seealso attrDefLabelGroup
##' @name labelGroup
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # Create the demo databases and loed 'ae' 
##' # (see ?load_emuDB for more information)
##' create_emuRdemoData()
##' ae <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
##' 
##' sampaNasals = c("m", "F", "n", "J", "N")
##' 
##' # add these labels to the ae emuDB
##' # as a globally available labelGroup
##' add_labelGroup(emuDBhandle = ae,
##'                name = "sampaNasals",
##'                labels = sampaNasals)
##' 
##' # query the labelGroup in the "Phonetic" level
##' query(emuDBhandle = ae, 
##'       query = "Phonetic == sampaNasals")
##' 
##' # query the labelGroup in the "Phoneme" level
##' query(emuDBhandle = ae, 
##'       query = "Phoneme == sampaNasals")
##'       
##' #Add another group. There are more continuants in the database, 
##' # but this is only for the purpose of illustration.
##' 
##' add_labelGroup(ae,"Continuants",c(sampaNasals,"s","z"))
##' 
##' query(emuDBhandle = ae, query = "Phonetic == Continuants & Phonetic != sampaNasals ")
##' 
##' # list global label groups of ae emuDB
##' list_labelGroups(emuDBhandle = ae)
##' 
##' # remove the newly added labelGroups
##' remove_labelGroup(emuDBhandle = ae,
##'                   name = "sampaNasals")
##' remove_labelGroup(emuDBhandle = ae,
##'                   name = "Continuants")
##' }
##' 
NULL

##' @rdname labelGroup
##' @export
add_labelGroup <- function(emuDBhandle,
                           name,
                           labels){
  
  dbConfig = load_DBconfig(emuDBhandle)
  curLgs = list_labelGroups(emuDBhandle)
  
  if(name %in% curLgs$name){
    stop("A labelGroup with name '", name ,"' already exists!")
  }
  
  # add labelGroup
  dbConfig$labelGroups[[length(dbConfig$labelGroups) + 1]] = list(name = name, 
                                                                  values = labels)
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
}


##' @rdname labelGroup
##' @export
list_labelGroups <- function(emuDBhandle){
  
  dbConfig = load_DBconfig(emuDBhandle)
  df = data.frame(name = character(),
                  labels = character(),
                  stringsAsFactors = FALSE)
  
  for(lg in dbConfig$labelGroups){
    df = rbind(df, data.frame(name = lg$name,
                              labels = paste0(lg$labels, collapse = "; "),
                              stringsAsFactors = FALSE))
  }
  
  return(df)
  
}


##' @rdname labelGroup
##' @export
remove_labelGroup <- function(emuDBhandle,
                              name){
  
  dbConfig = load_DBconfig(emuDBhandle)
  curLgs = list_labelGroups(emuDBhandle)
  
  if(!name %in% curLgs$name){
    stop("No labelGroup with name '", name ,"' found!")
  }
  
  for(i in 1:length(dbConfig$labelGroups)){
    if(dbConfig$labelGroups[[i]]$name == name){
      dbConfig$labelGroups[[i]] = NULL
    }
  }
  
  # store changes
  store_DBconfig(emuDBhandle, dbConfig)
}
