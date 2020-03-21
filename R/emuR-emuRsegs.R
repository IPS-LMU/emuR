## compatibility to emusegs
## methods:
## read.emusegs OK (type cast to emusegs) Override?
## make. OK different constructor
## is.seglist OK
## modify.seglist Problematic. No S3 method we cannot overload. Warning?
## emusegs.database OK
## emusegs.type OK (question: are mixed (EVENT and SEGMENT) 
## seglist possible in legacy Emu ?)
## emusegs.query
## print.emusegs OK But shows too many columns: TODO select a 
## good set of columns for an S3 override method
## [.emusegs OK But not clear what emusegs really does (and 
## code includes version$major switch (on S versions?))
## summary.emusegs OK
## label.emusegs OK
## as.matrix.emusegs OK
## write.emusegs OK (typecast to emusegs) TODO Print warning about data loss!
## start.emusegs OK
## end.emusegs OK
## utt.emusegs OK
## dur.emusegs OK

##' Make emuDB segment list
##' @param dbName name of emuDB
##' @param seglist segment list data.frame
##' @param query query string
##' @param type type of list elements
##' @export make.emuRsegs
make.emuRsegs <- function(dbName, seglist, query, type)
{
  
  class(seglist) <- c("emuRsegs",
                      "emusegs", 
                      "data.frame")
  
  attr(seglist, "query") <- query
  attr(seglist, "type") <- type
  attr(seglist, "database") <- dbName
  
  seglist
}


##' Print emuRsegs segment list
##' @param x object to print
##' @param ... additional params
##' @export
"print.emuRsegs" <-  function(x, ...) 
{
  cat(attributes(x)$type, 
      " list from database: ", 
      attributes(x)$database, 
      "\n")
  cat("query was: ", 
      attributes(x)$query, 
      "\n" )
  printX = '[.data.frame'(x, c('labels',
                               'start',
                               'end',
                               'session',
                               'bundle',
                               'level',
                               'type'))
  
  print.data.frame(printX, ...)
  
}

##' Sort emuRsegs segment list by session, bundle and sample_start 
##' @param x object to print
##' @param decreasing NOT IMPLEMENTED!
##' @param ... additional params
##' @export
"sort.emuRsegs" <-  function(x, decreasing, ...) 
{
  
  old_atts = attributes(x)
  
  sl_df_sorted = dplyr::arrange_(x, "session", "bundle", "sample_start")
  
  attributes(sl_df_sorted) = old_atts
  
  return(sl_df_sorted)
}


# S3 method definition
as.emusegs <- function(x, ...){
  UseMethod("as.emusegs", x)
}

as.emusegs.emuRsegs <- function(x, ...){
  emusegs = make.seglist(x$labels, 
                         x$start, 
                         x$end, 
                         x$utts, 
                         attr(x, "query"), 
                         type = attr(x, "type"), 
                         database = attr(x, "database"))
  return(emusegs)
} 


##' Exports a segment list to txt collection
##' 
##' Extract the media file (usually .wav file) snippets that correspond to 
##' the segments of a segment list (see result of a \code{\link{query}}) and 
##' save them to separate files and write the corresponding labels into a .txt file. Further,
##' the segmentlist is also stored to the target directory (as a .csv file).
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param seglist \code{tibble}, \code{\link{emuRsegs}} or 
##' \code{\link{emusegs}} object obtained by \code{\link{query}}ing a loaded emuDB 
##' @param targetDir target directory to store
##' @export
export_seglistToTxtCollection <- function(emuDBhandle, 
                                          seglist, 
                                          targetDir){
  
  if(!dir.exists(targetDir)){
    stop("targetDir does not exist!")
  }
  
  targetDir_full = file.path(targetDir, 
                             paste0(emuDBhandle$dbName, 
                                    "_txt_col_from_seglist"))
  dir.create(targetDir_full)
  
  for(i in 1:nrow(seglist)){
    ado = wrassp::read.AsspDataObj(file.path(emuDBhandle$basePath, 
                                             paste0(seglist[i,]$session, "_ses"),
                                             paste0(seglist[i,]$bundle, "_bndl"),
                                             paste0(seglist[i,]$bundle, ".wav")),
                                   begin = seglist[i,]$start / 1000,
                                   end = seglist[i,]$end / 1000) # hardcoded mediaFileExt!
    
    i_padded = stringr::str_pad(i, 
                                width = stringr::str_length(nrow(seglist)),
                                side = "left", 
                                pad = "0")
    
    wrassp::write.AsspDataObj(ado, 
                              file = file.path(targetDir_full, 
                                               paste0("sl_rowIdx_", i_padded, ".wav")))
    
    readr::write_file(seglist[i,]$labels, 
                      path = file.path(targetDir_full, 
                                       paste0("sl_rowIdx_", i_padded, ".txt")))
  }
  
  readr::write_csv(seglist, 
                   path = file.path(targetDir_full, 
                                    paste0("seglist.csv")))
  
}