##' read bundleList
##' @description read bundleList JSON file in emuDB
##' @details Read bundleList JSON file in emuDB that is stored in
##' the databases root dir sub-dir \code{bundleLists/}
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param name name of bundleList (excluding the _bundleList.json suffix)
##' @return tibble with the columns \code{session}, \code{name}, 
##' \code{comment}, \code{finishedEditing}
##' @export
read_bundleList <- function(emuDBhandle, 
                            name){
  
  bl_dir_path = file.path(emuDBhandle$basePath, "bundleLists")
  # check if folder exists
  if(!dir.exists(bl_dir_path)){
    stop("no bundleList dir found in emuDB in dir: ", bl_dir_path)
  }
  
  bl_path = file.path(bl_dir_path, 
                      paste0(name, "_bundleList.json"))
  
  return(tibble::as_tibble(jsonlite::read_json(bl_path, simplifyVector = T)))
  
}

##' write bundleList
##' @description write bundleList JSON file to emuDB
##' @details Write bundleList JSON file to emuDB sub-dir \code{bundleLists/}
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param name name of bundleList (excluding the _bundleList.json suffix)
##' @param bundleList tibble/data.frame with the columns \code{session}, \code{name}, 
##' \code{comment} (optional), \code{finishedEditing} (optional). Use \link{list_bundles}
##' @export
write_bundleList <- function(emuDBhandle, 
                             name, 
                             bundleList){
  
  bundleList %>% 
    dplyr::select(.data$session, 
                  .data$name, 
                  dplyr::contains("comment"), 
                  dplyr::contains("finishedEditing")) -> bundleList
  
  # add if not available
  if(!"comment" %in% names(bundleList)){
    bundleList$comment = ""
  }
  if(!"finishedEditing" %in% names(bundleList)){
    bundleList$finishedEditing = FALSE
  }
  
  
  
  bl_dir_path = file.path(emuDBhandle$basePath, "bundleLists")
  
  if(!dir.exists(bl_dir_path)){
    print(paste0("INFO: No bundleList dir found in emuDB (path: ", bl_dir_path, ")! Creating directory..."))
    dir.create(bl_dir_path)
  }
  
  bl_path = file.path(bl_dir_path, paste0(name, "_bundleList.json"))
  
  jsonlite::write_json(bundleList, bl_path, pretty = T)
  
}
