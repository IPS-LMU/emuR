
## Some constants
metadata.suffix = "_meta.json"

## I will need this function until 0.9.0 of dplyr is released,
#possibly fixing the issue with all NA columns supplied to coalesce
# The implementation comes from https://stackoverflow.com/a/19254510
coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}


#' Functions for gathering metadata specified for recordings in an emuR database.
#'
#' Metadata of a recording is stored in '_meta.json' files. Metadata may be set at the database, session and bundle level.
#' The functions goes through the database metadata file, session metadata files and metadata files associated with
#' each bundle, parses the JSON data and returns \code{\link[dplyr]{tibble}} with one row per bundle in the database.
#' Database default values are supressed by information set in a session metadata file, and session level data are in
#' turn surpressed by data given at the bundle level.
#' The structure of the metadata does not have to be consistent across meta_json files.
#' New columns are added to the as new fields are detected.
#'
#' The function \code{export_metadata} outputs the metadata as an Excel file instead, with bundle, session and database
#' tabs. The "bundle" tab gives the complete set of all the metadata that are active for each bundle, regardness where
#' it was set (for the bundle directly, or as a session / database default value).
#'
#' The user is expected to use the functions \code{export_metadata} and \code{import_metadata} to fix
#' accedental inconsistencies in the metadata of a database across bundles by exporting all
#' information to an Excel file using \code{export_metadata}, edit columns and values (including moving inconsistently
#' spelled metadata fields into a single column with the intended name) using Excel or another editor that complies with
#' the OOXML Workbook ISO/IEC 29500:2008 standard. The user is also expected to keep the indented structure of the Excel
#' file (one row per bundle or session, and each column except for those indicating session and bundle names containing
#' metadata), otherwise it is possible that the file may not be read in again by \code{\link{import_metadata}} to set
#' updated values.
#'
#'
#' @param emuDBhandle The database handle of an emuR database.
#' @param Excelfile The full path and file name of the Excel file that the metadata should be written to. The function will not overwrite this file, unless \code{overwrite} is set to \code{TRUE}.
#' @param overwrite The default behaviour is that an Excel file should not be
#' overwritten if it exists already. If this parameter is \code{TRUE} then the file will be overwritten.
#' @param manditory A vector of column names which the function will ensure are present in both the bundle and session level metadata.
#'
#' @rdname export_metadata
#' @export
#'
#' @return A data frame containing inforamtion about the '_meta.json' files found
#' \describe{
#'   \item{session}{The name of the session.}
#'   \item{bundle}{The bundle name}
#' }
#' In addition, the [dplyr::tibble] will contain one column for every type of information given in any of the 'meta_json' files.
#'
#'
#' @examples
#' \dontrun{
#' create_emuRdemoData()
#' ae_test <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
#'
#' # Database-wide default information
#' add_metadata(ae_test,list("Accent"="Northern","Elicitation"="Scripted"))
#' #Bundle specific information
#' add_metadata(ae_test,list("Speaker.Sex"="Male","Date"="2020-03-04"),session="0000",bundle="msajc003")
#' get_metadata(ae_test) -> res
#' print(res)
#' }
#'

get_metadata <- function(emuDBhandle,sessionPattern=".*", bundlePattern=".*",manditory=c(),clearCache=FALSE, verbose=TRUE){
  if(clearCache){
    if(verbose){
      cat("INFO: Clearing metadata cache\n")
    }
    memoise::forget(metadata_cache)
  } 	 
  if(exists("metadata_cache")){
    if(!memoise::has_cache(metadata_cache)(emuDBhandle,manditory)){
      if(verbose){
        res <- metadata_cache(emuDBhandle,manditory,progressBar=TRUE)
      } else {
        res <- metadata_cache(emuDBhandle,manditory,progressBar=FALSE)
      }
    } else {
      res <- metadata_cache(emuDBhandle,manditory)
    }
  } else {
    metadata_cache <- memoise::memoise(export_metadata)
    res <- metadata_cache(emuDBhandle,manditory)
  }
  return(res%>% dplyr::filter(grepl(sessionPattern,session)) %>% dplyr::filter(grepl(bundlePattern,bundle)))
}


#'
#' @rdname export_metadata
#' @export
#'

export_metadata <- function(emuDBhandle,xlsxFile=NULL,overwrite=FALSE, manditory=c(), progressBar=FALSE){
  #Start with checking consistency regarding output file
  if(overwrite && !is.null(xlsxFile) && file.access(xlsxFile,2)){
    stop("Can not write output to file ",xlsxFile,": File exists but does not have permission to be overwriten.")
  }
  if(!overwrite && !is.null(xlsxFile) && file.access(xlsxFile,0)){
    stop("Can not write output to file ",xlsxFile,": Set variable overwrite=TRUE if you want to overwrite.")
  }
  if (!is.null(xlsxFile) && !requireNamespace("openxlsx", quietly = TRUE)){
    stop("'openxlsx' package required to run export metadata to an xlsx file.")
  }
  if (!is.null(xlsxFile) && !progressBar){
    #Exporting to xlsx without progress bar is not recommended
    progressBar = TRUE
  }
  check_emuDBhandle(emuDBhandle)
  
  bundles <- list_bundles(emuDBhandle) %>%
    dplyr::rename(bundle=name)
  if(progressBar){
    cat(paste0("INFO: Reading _meta.json files for ", nrow(bundles), " bundles\n"))
    pb <- txtProgressBar(min = 0, max = nrow(bundles), style = 3)
  }
  metafiles <- list_metafiles(emuDBhandle)
  
  #Use the bundle list as a scaffold for a dataframe to hold the content of all metadata files
  #  metacontent <- metafiles[c("bundle","absolute_file_path")]
  i <- 0
  for(currFile in na.omit(metafiles$absolute_file_path)){
    jsonmeta <- jsonlite::read_json(currFile,simplifyVector = TRUE)
    i <- i+1
    if(progressBar){
      setTxtProgressBar(pb, i)
    }
    # Now start inserting data from the metafiles
    for(col in names(jsonmeta)){
      #Assignment of NULL is problematic, so we need to make sure we never do that
      metafiles[metafiles$absolute_file_path == currFile,col] <- ifelse(is.null(jsonmeta[[col]]),NA,jsonmeta[[col]])
    }
  }
  if(progressBar){
    close(pb)
  }
  # Now make sure that all bundles have a row
  metafiles <- bundles %>%
    dplyr::left_join(metafiles,by=c("session","bundle")) %>%
    dplyr::select(-file,-absolute_file_path)
  
  #Add missing manditory columns
  missingManditory <- setdiff(manditory, names(metafiles))
  metafiles[, missingManditory] <- NA
  
  #For sessions it makes less sense to use list_files as a base for finding the metadata files
  #so we pre-generate the expected names and check which ones actually exist.
  
  sessions <- list_sessions(emuDBhandle) %>%
    dplyr::rename(session=name)
  
  sessions$session_metadata_file <- file.path(emuDBhandle$basePath,
                                              paste0(sessions$session,session.suffix),
                                              paste0(sessions$session,metadata.suffix))
  
  sessJSONFilesDF <- sessions[file.exists(sessions$session_metadata_file),]
  
  for(row in seq_len(nrow(sessJSONFilesDF))){
    
    jsonmeta <- jsonlite::read_json(sessJSONFilesDF[row,"session_metadata_file"],simplifyVector = TRUE)
    
    # Now start inserting data from the session metadata file
    for(col in names(jsonmeta)){
      #Assignment of NULL to a data.frame value is problematic, so we make sure we never do
      sessJSONFilesDF[row,col] <- ifelse(is.null(jsonmeta[[col]]),NA,jsonmeta[[col]])
    }
  }
  #Make sure that all sessions have a row in the output
  sessJSONFilesDF <- sessions %>%
    dplyr::select(-session_metadata_file) %>%
    dplyr::left_join(sessJSONFilesDF,by="session") %>%
    dplyr::select(-session_metadata_file)
  
  #Add missing manditory columns
  missingManditory <- setdiff(manditory, names(sessJSONFilesDF))
  sessJSONFilesDF[, missingManditory] <- NA
  
  # Make the merger with bundle files to make the final output tibble
  metafiles %>%
    dplyr::left_join(sessJSONFilesDF,by="session",suffix=c("","_sessionmetadatafile")) -> metafiles
  
  # Now check and load metadata set at the database level
  
  database_metadata_file <- file.path(emuDBhandle$basePath,
                                      paste0(emuDBhandle$dbName,metadata.suffix))
  
  if(!file.exists(database_metadata_file)){
    dbMeta <- data.frame()
  }else{
    jsonmeta <- jsonlite::read_json(database_metadata_file,simplifyVector = TRUE)
    
    dbDefaults <- as.data.frame(jsonmeta,stringsAsFactors=FALSE)
    if(length(dbDefaults) > 0){
      #This means that the field is not just empty
      # Repeat the rows so that the columns may be merged
      dbMeta <- as.data.frame(c(metafiles["bundle"],dbDefaults))  %>%
        dplyr::mutate_if(is.factor,as.character)
      
      metafiles <- metafiles %>%
        dplyr::mutate_if(is.factor,as.character)%>%
        dplyr::left_join(dbMeta,by="bundle",suffix=c("","_databasemetadatafile")) %>%
        dplyr::distinct() ## This is needed since duplicate rows are introduced by the join by dbMeta
      
      
    }
  }
  
  #Now, there may be a metadata X column set at the bundle level, an X_sessionmetadatafile
  # column set at the session level, and an X_database column for the whole database.
  # These need to be reconsiled
  
  
  cols <- names(metafiles)
  duplicates <- grep("_(databasemetadatafile|sessionmetadatafile)$",cols,value=TRUE)
  duplicated <- unique(gsub("_(databasemetadatafile|sessionmetadatafile)$","",cols))
  
  
  for(bundleoriginal in duplicated){
    
    sessColName <- paste0(bundleoriginal,"_sessionmetadatafile")
    sessVec <- ifelse(exists(sessColName,metafiles),metafiles[,sessColName],NA)
    dbColName <- paste0(bundleoriginal,"_databasemetadatafile")
    dbVec <- ifelse(exists(dbColName,metafiles),metafiles[,dbColName],NA)
    ## This seems odd, but it makes sure that NAs are repeated for the length of vectors.
    tempDF <- data.frame(metafiles[[bundleoriginal]],
                         sessVec,
                         dbVec,stringsAsFactors = FALSE) %>%
      
      dplyr::mutate_if(is.factor,as.character)
    
    names(tempDF) <- c("bundle","session","database")
    # Here the result is the first non-NA value for each row (or NA if the row in tempDF contains only NAs)
    metafiles[bundleoriginal] <- with(tempDF,coalesce(bundle,session,database))
    # This works since you can always remove a column without an error message (even non-existing ones)
    metafiles[sessColName] <- NULL
    metafiles[dbColName] <- NULL
  }
  
  #Prepare an OpenXML Excel workbook, if one should be written
  if(!is.null(xlsxFile)){
    wb <- openxlsx::createWorkbook(paste(emuDBhandle$dbName,"bundle"))
    openxlsx::addWorksheet(wb,"bundles")
    openxlsx::writeDataTable(wb,"bundles",x=metafiles,keepNA = FALSE,withFilter=FALSE)
    openxlsx::freezePane(wb,"bundles",firstActiveCol = 5)
    openxlsx::setColWidths(wb,"bundles",cols=5:30,widths = 18)
    
    # session information
    
    openxlsx::addWorksheet(wb,"sessions")
    openxlsx::writeDataTable(wb,"sessions",x=sessJSONFilesDF,keepNA = FALSE,withFilter=FALSE)
    openxlsx::freezePane(wb,"sessions",firstActiveCol = 3)
    openxlsx::setColWidths(wb,"sessions",cols=3:30,widths = 18)
    #database defaults
    openxlsx::addWorksheet(wb,"database")
    if(ncol(dbMeta)> 0){
      openxlsx::writeDataTable(wb,"database",x=dbMeta,keepNA = FALSE,withFilter=FALSE)
    }else{
      openxlsx::writeComment(wb,"database",col=1,row=1,
                             openxlsx::createComment("The database default keys go in the header (top row) of a column",author="EmuR"))
      openxlsx::writeComment(wb,"database",col=1,row=2,
                             openxlsx::createComment("The database default values go below in the second row of a column, and the value underneath",author="EmuR"))
      
    }
    # We do not need to check overwriting here as that is handled by saveWorkbook
    openxlsx::saveWorkbook(wb,file=xlsxFile,overwrite=overwrite)
  }
  
  return(metafiles)
  
}


#' Functions to import or add metadata information to database bundles.
#'
#' The function takes an appropriately structured Excel file and uses the
#' information to set metadata for bundles.
#'
#' The first sheet ("bundles") in the Excel file should begin with the folowing two columns:
#' \itemize{
#' \item session
#' \item bundle
#' }
#' and then go on to have some columns which contains the metadata. Each row in the
#' data contains the information and metadata for a bundle (in the specific session).
#' The simples way to get an appropriately structured Excel file is to create one from a database using the
#' \code{\link{export_metadata}} function on an existing database and given an output file.
#'
#' Please be aware that bundles that are speficied in the Excel file will have
#' their metadata files (ending with '_meta.json') overwritten when using the
#' \code{import_metadata}. So, please make sure to remove the rows of bundles that should
#' not be altered from the Excel file before importing the metadata from it using this function.
#'
#' Date and time fields are assumed to follow the ISO8601 specification, and an attempt to convert them to the
#' approprite JSON representation will be made. The user should be aware that this conversion is made however, and
#' watch out unexpected results in advanced cases.
#'
#' @param emuDBhandle The emuR database handle of the database.
#' @param xlsxFile The path to a properly formated Excel (.xlsx) file.
#'
#' @return A vector of _meta.json files updated by the call. The path for each file is given relative to the base of the EmuR database.
#' @export
#'
import_metadata <- function(emuDBhandle,xlsxFile, deleteExisting=FALSE, verbose=TRUE){
  if (!requireNamespace("openxlsx", quietly = TRUE)){
    stop("'openxlsx' package required to run import_metadata()")
  }
  if(!file.exists(xlsxFile)){
    stop("Unable to open the metadata Excel file.\nThe file ",xlsxFile," does not exist!")
  }
  
  #Open xlsx file and read sheets to dataframes
  
  openxlsx::read.xlsx(xlsxFile,sheet="bundles",detectDates=TRUE) -> meta
  openxlsx::read.xlsx(xlsxFile,sheet="sessions") -> sessionMeta
  openxlsx::read.xlsx(xlsxFile,sheet="database") -> dbMeta
  
  if(nrow(dbMeta) > 1){
    stop("Sheet 'database' in ",xlsxFile," contains more than the allowable one row (+ header)")
  }
  if(verbose){
    cat(paste0("INFO: Reading ", xlsxFile, " and removing inherited values\n"))
    pb <- txtProgressBar(min = 0, max = 1 + nrow(sessionMeta), style = 3)
  }
  
  #Remove duplicate inheritance values from session and bundles
  for(col in names(dbMeta)){
    if(!is.na(dbMeta[[col]])){
      if(!is.null(sessionMeta[[col]])){
        sessionMeta[[col]] <- ifelse(sessionMeta[[col]] == dbMeta[[col]], NA, sessionMeta[[col]])
      }
      if(!is.null(meta[[col]])){
        meta[[col]] <- ifelse(meta[[col]] == dbMeta[[col]], NA, meta[[col]])
      }
    }
  }
  if(verbose){
    setTxtProgressBar(pb, 1)
  }
  for(row in 1:nrow(sessionMeta)){
    for(col in names(sessionMeta)){
      if(col != "session" && !is.na(sessionMeta[[col]][row])){
        if(!is.null(meta[[col]])){
          meta[[col]] <- ifelse(meta[["session"]] == sessionMeta[["session"]][row] & meta[[col]] == sessionMeta[[col]][row], NA, meta[[col]])
        }
      }
    }
    if(verbose){
      setTxtProgressBar(pb, 1 + row)
    }
  }
  if(verbose){
    close(pb)
  }
  
  #Write any remaining (non-NA) rows to _meta.json files
  if(verbose){
    cat(paste0("INFO: Writing _meta.json for ", nrow(meta), " bundles, ", nrow(sessionMeta), " sessions and ", nrow(dbMeta), " database\n"))
    pb <- txtProgressBar(min = 0, max = nrow(sessionMeta) + nrow(meta), style = 3)
  }
  
  #Give the bundle dataframe an output file column with full path 
  meta <- meta %>%
    dplyr::mutate(metadatafile=file.path(emuDBhandle$basePath,
                                         paste0(session,session.suffix),
                                         paste0(bundle,bundle.dir.suffix),
                                         paste0(bundle,metadata.suffix)),
                  json=rep(NA, nrow(meta)))
  #Create json string from non-NA columns
  colsOnly <- meta %>% dplyr::select(-session,-bundle,-metadatafile,-json)
  for(row in 1:nrow(colsOnly)){
    if(verbose){
      setTxtProgressBar(pb, row)
    }
    colsOnly[row,] %>%
      dplyr::select(where(function(x) !is.na(x))) -> jsondat
    
    if(length(jsondat) > 0){	
      meta$json[row] <-jsonlite::toJSON(jsondat,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")
      #Write the bundle metadata files
      fileConn <- file(meta$metadatafile[row])
      writeLines(as.character(meta$json[row]), fileConn)
      close(fileConn)
    } else if(file.exists(meta$metadatafile[row]) && deleteExisting){
      file.remove(meta$metadatafile[row])
    }
  }
  
  bFiles <- meta %>% dplyr::filter(!is.na(json)) %>%
    dplyr::select(metadatafile) 
  
  ## Now process session metadata files
  
  sessionMeta <- sessionMeta %>%
    dplyr::mutate(metadatafile=file.path(emuDBhandle$basePath,
                                         paste0(session,session.suffix),
                                         paste0(session,metadata.suffix)),
                  json=rep(NA, nrow(sessionMeta)))
  #Create json string from non-NA columns
  colsOnly <- sessionMeta %>% dplyr::select(-session,-metadatafile,-json)
  pbOffset = nrow(meta)
  for(row in 1:nrow(colsOnly)){
    if(verbose){
      setTxtProgressBar(pb, row + pbOffset)
    }
    colsOnly[row,] %>%
      dplyr::select(where(function(x) !is.na(x))) -> jsondat
    
    if(length(jsondat) > 0){	
      sessionMeta$json[row] <-jsonlite::toJSON(jsondat,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")
      #Write the session metadata files
      fileConn <- file(sessionMeta$metadatafile[row])
      writeLines(as.character(sessionMeta$json[row]), fileConn)
      close(fileConn)
    } else if(file.exists(sessionMeta$metadatafile[row]) && deleteExisting){
      file.remove(sessionMeta$metadatafile[row])
    }
  }
  
  sFiles <- sessionMeta %>% dplyr::filter(!is.na(json)) %>%
    dplyr::select(metadatafile) 
  
  if(verbose){
    close(pb)
  }
  
  # Now inject database wide metadata
  database_metadata_file <- file.path(emuDBhandle$basePath,
                                      paste0(emuDBhandle$dbName,metadata.suffix))
  
  if(nrow(dbMeta) == 1){
    dbMetaJSON <-jsonlite::toJSON(dbMeta,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")
    
    fileConn <- file(database_metadata_file)
    writeLines(as.character(dbMetaJSON), fileConn)
    close(fileConn)
    
    return(c(database_metadata_file,sFiles,bFiles))
  } else {
    if(file.exists(database_metadata_file) && deleteExisting){
      file.remove(database_metadata_file)
    }
    return(c(sFiles,bFiles))
  }
}


#' A utility function used for programatically setting metadata for a bundle or
#' session or default values for an entire database.
#'
#' The function takes a list and a specification of where the metadata should be
#' set. The default behaviour is to keep already set metadata, and overwrite
#' only the values that are named in the list. The user may change this
#' behaviour by setting \code{reset.before.add=TRUE}, in which case all previous
#' bundle, session or database level metadata will be replaced with the contents
#' of the list.
#'
#' If a bundle name and a \code{session} name is provided, the metadata will be
#' inserted only for that fully specified \code{bundle}. If only a \code{bundle}
#' name is provided, the function will add the metadata for the bundle only if
#' there is just one session in the database. If there are multiple
#' \code{session}s, the function will given an error.
#'
#' If no \code{session} or \code{bundle} names are provided, the metadata will
#' be inserted as default values for the entire database. Please note that
#' database wide ingested metadata defaults currently interferes with the Emu
#' web app and should not be put into general use before this issue has been
#' fixed in the Emu web app.
#'
#' @param emuDBhandle An Emu database handle
#' @param metadataList A list specifying the metadata to be set. If set to an
#'   empty list (\code{list()}) the function will clear all metadata, if the
#'   argument \code{reset.before.add=TRUE} is given by the user. The user may
#'   also clear (remove from the set of defined metadata) by setting the
#'   property to NULL.
#' @param bundle An optional name of a bundle
#' @param session An optional name of a session
#' @param reset.before.add If set to TRUE, the function will ignore previously
#'   set metadata and simply add the metadata supplied in the list.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' create_emuRdemoData()
#' ae_test <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
#'
#' # Database-wide default information
#' add_metadata(ae_test,list("Accent"="Northern","Elicitation"="Scripted"))
#' #Bundle specific information
#' add_metadata(ae_test,list("Speaker.Sex"="Male","Date"="2020-03-04"),session="0000",bundle="msajc003")
#' get_metadata(ae_test) -> res
#' print(res)
#' }
#'
add_metadata <- function(emuDBhandle,metadataList,bundle=NULL,session=NULL, reset.before.add=FALSE){
  # Here we store metadata in either database wide, session wide or bundle specific metadata files
  # Since these files use the same structure, the only difference is to set the correct metadatafile filename.
  
  if(is.null(bundle) && is.null(session)){
    
    #Database level metadata
    
    metadatafile <- file.path(emuDBhandle$basePath,
                              paste0(emuDBhandle$dbName,metadata.suffix))
    
  } else if(! is.null(session) & is.null(bundle)){
    #Session level metadata
    
    metadatafile <- file.path(emuDBhandle$basePath,
                              paste0(session,session.suffix),
                              paste0(session,metadata.suffix))
  } else if(! is.null(bundle)){
    #Bundle metadata
    if(is.null(session)){
      ses <- list_sessions(emuDBhandle)
      if(nrow(ses) == 1){
        #use the name of the only available session
        session <- ses[[1]]
      }else{
        stop("If you provide a bundle name you need to provide a session name if there are more than one sessions in the database.")
      }
      
    }
    
    metadatafile <- file.path(emuDBhandle$basePath,
                              paste0(session,session.suffix),
                              paste0(bundle,bundle.dir.suffix),
                              paste0(bundle,metadata.suffix))
    
    
  }
  
  if(reset.before.add | ! file.exists(metadatafile) ){
    #Start fresh / overwrite previous values
    jsonmetaList <- list()
  } else{
    
    #Read in previous values
    jsonmetaList <- as.list(jsonlite::read_json(metadatafile,simplifyVector = TRUE))
    
  }
  #set / overwrite metadata from list
  #jsonmetaList[names(metadataList)] <- metadataList
  jsonmetaList <- utils::modifyList(jsonmetaList,metadataList,keep.null = FALSE)
  jsonlite::write_json(jsonmetaList,metadatafile,auto_unbox=TRUE)
  #Reset the metadata cache now that we have writen new data to it
  memoise::forget(metadata_cache)
  
}


#' Add identifying information based on the content of the wave file to the metadata information for the bundle.
#'
#' This function will extract information (lenght of recording and a checksum) from the wav file associated with a bundle, and add it to the set of metadata
#' for the bundle. This information can later be used to verify that the file has not been altered later on, or to deidentify
#' wav files in a reversable manner for use outside of the emuR framework. De-identified files are sometimes useful for blinded randomized
#' perceptual testing, and the ability to reverse the procedure is then essential to link the results of the evaluation back to the original
#' recording extracted from the emuR data base. The user may create checksums by multiple algorithms by running the function again with different \code{algorithm} arguments.
#'
#' @param emuDBhandle The handle for the emuR database.
#' @param sessionPattern A regexp pattern that allows the user to limit which sessions should be affected by the manipulation.
#' @param bundlePattern A regexp pattern that allows the user to limit which bundles to include.
#' @param algorithm The name of the hashing algorithm, according to the \code{\link[digest]{digest}} function.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_emuRdemoData()
#' ae_test <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
#'
#' #Add a md5 digest to the metadata of all bundles
#' add_digests(ae_test,algorithm = "md5")
#'
#' #Add a "sha1" checksum (the default) to some bundles
#' add_digests(ae_test,bundlePattern = "msajc0.*")
#' get_metadata(ae_test) -> res
#' print(res)
#' }
#'
add_digests <- function(emuDBhandle,sessionPattern=".*",bundlePattern=".*",algorithm="sha1", verbose=TRUE){
  wavs <- list_files(emuDBhandle,fileExtension = "*.wav",sessionPattern=sessionPattern,bundlePattern=bundlePattern)
  if(verbose){
    cat(paste0("INFO: Writing ",algorithm, " checksums for ", nrow(wavs), " bundles\n"))
    pb <- txtProgressBar(min = 0, max = nrow(wavs), style = 3)
  }
  for(f in 1:nrow(wavs)){
    inFile <- unlist(wavs[f,"absolute_file_path"],use.names = FALSE)
    session <- unlist(wavs[f,"session"],use.names = FALSE)
    bundle <-  unlist(wavs[f,"bundle"],use.names = FALSE)
    
    wrassp::read.AsspDataObj(inFile) -> w
    options(digits=15)
    attr(w,"sampleRate") -> sr
    attr(w,"endRecord") - attr(w,"startRecord") +1 -> samples
    samples / sr *1000 -> duration
    rm(w)
    digest::digest(inFile,file=TRUE,algo=algorithm) -> checksum
    metadata <- list("Bundle.Duration.ms"=duration)
    metadata[paste0("Bundle.",algorithm,"_checksum")] <- checksum
    
    add_metadata(emuDBhandle,metadata,session=session,bundle=bundle)
    if(verbose) {
      setTxtProgressBar(pb, f)
    }
  }
  if(verbose) {
    close(pb)
  }
}


#' Create a biography of the labels in a list of segments in a tidy manner
#'
#' @param segs_tbl The \code{\link[dplyr]{tibble}} that is the result \code{\link[emuR]{query}} call.
#' @param emuDBhandle A \code{\link{emuR}} database handle.
#' @param compute_digests Should information that describes the recorded sound files be computed so that is is definitelly part of the
#' added metadata information.
#' @param algorithm The checksum algorithm that should be used when computing sound file information.
#'
#' @return A \code{\link[dplyr]{tibble}}
#' @export
#'
#' @examples
#' \dontrun{
#' create_emuRdemoData()
#' ae_test <- load_emuDB(file.path(tempdir(),"emuR_demoData","ae_emuDB"))
#'
#' # Database-wide default information
#' add_metadata(ae_test,list("Accent"="Northern","Elicitation"="Scripted"))
#' #Bundle specific information
#' add_metadata(ae_test,list("Speaker.Sex"="Male","Date"="2020-03-04"),session="0000",bundle="msajc003")
#'
#' # Get all the 'n' segments in the database
#' query(ae_test,"Phonetic = n",resultType = "tibble") -> ae_nt
#' # Add information related to the nature the recording sessions
#' # e.g. the speaker ID, the date of the recording
#' ae_nt %>% biographize(ae_test) %>% glimpse()
#' # This code does the same as the above, but it will also compute new
#' # information that is strictly  aimed at identifying the recording
#' # (length of recording (in ms) and a sha1 digest of the wav file).
#' ae_nt %>%
#'    biographize(ae_test,compute_digests=TRUE,algorithm="sha1") %>%
#'    glimpse()
#' rm(ae_test)
#'
#' }
#'
biographize <- function(segs_tbl,emuDBhandle,compute_digests=FALSE,algorithm="sha1") {
  #make sure that the first argument is a segment list, and that
  # it contains "session" and "bundle" columns.
  if(! is.data.frame(segs_tbl) || isFALSE(c("session", "bundle") %in% names(segs_tbl))){
    out <- paste("The input to the",match.call()[[1]], "has to be an emuDB seglist 'tibble' or a 'data.frame'.")
    stop(out)
  }
  if(compute_digests==TRUE){
    add_digests(emuDBhandle,algorithm = algorithm)
  }
  #Here we use the special mode of export_medatata to get a data structure rather than an Excel file.
  mdata <- get_metadata(emuDBhandle)
  
  out <- segs_tbl %>%
    dplyr::left_join(mdata,by = c("session", "bundle"))
  
  return(out)
}


##' List metadata files of emuDB
##' 
##' List metadata files (only those within bundles) belonging to emuDB.
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param fileExtension file extension of files (default .json)
##' @param sessionPattern A (RegEx) pattern matching sessions to be searched from the database
##' @param bundlePattern A (RegEx) pattern matching bundles to be searched from the database
##' @return file paths as character vector
##' @export
##' @importFrom rlang .data
##' @keywords emuDB database schema Emu 
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB
##' # (see ?load_emuDB for more information)
##' 
##' # list all files of ae emuDB
##' list_metafiles(emuDBhandle = ae)
##'
##' # list all files of ae emuDB in bundles ending with '3'
##' list_metafiles(emuDBhandle = ae, bundlePattern=".*3$") 
##' 
##' }
##' 
list_metafiles <- function(emuDBhandle,
                           fileExtension = ".json",
                           sessionPattern = ".*",
                           bundlePattern = ".*"){
  
  check_emuDBhandle(emuDBhandle)
  
  fileList = list.files(path = file.path(emuDBhandle$basePath),
                        recursive = T,
                        pattern = paste0("*_meta", fileExtension, "$")) %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = .data$value,
                    into = c("session", "bundle", "file"),
                    sep = .Platform$file.sep,
                    extra = "drop",
                    fill = "right")  %>%
    dplyr::filter(!is.na(.data$session)) %>%
    dplyr::filter(!is.na(.data$bundle)) %>%
    dplyr::filter(!is.na(.data$file)) %>%
    
    dplyr::filter (endsWith(.data$session, "_ses")) %>%
    dplyr::filter (endsWith(.data$bundle, "_bndl")) %>%
    
    dplyr::mutate(session = stringr::str_remove(.data$session, "_ses$")) %>%
    dplyr::mutate(bundle = stringr::str_remove(.data$bundle, "_bndl$")) %>%
    
    dplyr::filter (stringr::str_detect(.data$session, sessionPattern)) %>%
    dplyr::filter (stringr::str_detect(.data$bundle, bundlePattern)) %>%
    
    dplyr::mutate (absolute_file_path = file.path(emuDBhandle$basePath,
                                                  paste0(.data$session, "_ses"),
                                                  paste0(.data$bundle, "_bndl"),
                                                  file))
  
  return (fileList)
}

###Initialise metadata cache
metadata_cache <- memoise::memoise(export_metadata, omit_args = c("progressBar"))

### INTERACTIVE testing

#unlink_emuRDemoDir()
#create_ae_db() -> emuDBhandle
#add_metadata(emuDBhandle,session = "0000",bundle = "msajc003",metadataList = list(Gender=NA,Age=10))
#rstudioapi::navigateToFile(file.path(emuDBhandle$basePath,"ae_DBconfig.json"))
#add_metadata(emuDBhandle,metadataList = list(Gender=NA,Age=10))

#add_metadata(emuDBhandle,session = "0000",bundle = "msajc003",metadataList = list(Gender=NULL,Age=20))

# dir.create(file.path(emuDBhandle$basePath,"temp"))
# for(i in 1:11){
#   file.copy(file.path(emuDBhandle$basePath,"0000_ses"),file.path(emuDBhandle$basePath,"temp"),recursive = TRUE)
#   newName <- paste0(i,i,i,i,"_ses")
#   file.rename(file.path(emuDBhandle$basePath,"temp","0000_ses"),file.path(emuDBhandle$basePath,"temp",newName))
#   file.copy(file.path(emuDBhandle$basePath,"temp",newName),emuDBhandle$basePath,recursive = TRUE)
#   unlink(file.path(emuDBhandle$basePath,"temp",newName),recursive = TRUE)
# }



#export_metadata(ae,Excelfile = "~/Desktop/out.xlsx",overwrite = TRUE)
#rstudioapi::navigateToFile(list_files(emuDBhandle,"meta_json")$absolute_file_path)
#export_metadata(ae,Excelfile = "~/Desktop/out.xlsx")