
## Some constants
metadata.extension = "meta_json"

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


#' Functions for gathering metata specified for recordings in an emuR database.
#'
#' Metadata of a recording is stored in 'meta_json' files. Metadata may be set at the database, session and bundle level.
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
#'
#' @rdname export_metadata
#' @export
#'
#' @return A data frame containing inforamtion about the 'meta_json' files found
#' \describe{
#'   \item{session}{The name of the session.}
#'   \item{bundle}{The bundle name}
#' }
#' In addition, the \code{\link[dplyr]{tibble}} will contain one column for every type of information given in any of the 'meta_json' files.
#'
#'
#' @examples
#' \dontrun{
#' demodir <- file.path(tempdir(),"emuR_demoData")
#
#' create_emuRdemoData()
#'
#' ae <- load_emuDB(file.path(demodir,"ae_emuDB"))
#' articulator:::make_dummy_metafiles(ae)
#' get_metadata(ae)
#' }
#'

get_metadata <- function(emuDBhandle,overwrite=FALSE,session=".*"){
  res <- export_metadata(emuDBhandle=emuDBhandle,overwrite=overwrite)
  return(res)
}


#'
#' @rdname export_metadata
#' @export
#'

export_metadata <- function(emuDBhandle,Excelfile=NULL,overwrite=FALSE){
  #Start with checking consistency regarding output file
  if(! overwrite && !is.null(Excelfile) && file.exists(Excelfile)){
    stop("Could not write output file ",Excelfile,": File exists but should not be overwritten.")
  }

  check_emuDBhandle(emuDBhandle)

  bundles <- list_bundles(emuDBhandle) %>%
    rename(bundle=name)
  metafiles <- list_files(emuDBhandle,fileExtension = metadata.extension)
  #Use the bundle list as a scaffold for a data fram to hold the content of all metadata files
  #  metacontent <- metafiles[c("bundle","absolute_file_path")]
  for(currFile in na.omit(metafiles$absolute_file_path)){
    jsonmeta <- jsonlite::read_json(currFile,simplifyVector = TRUE)

    # Now start inserting data from the metafiles
    for(col in names(jsonmeta)){
      metafiles[metafiles$absolute_file_path == currFile,col] <- jsonmeta[[col]]
    }
  }
  # Now make sure that all bundles have a row
  metafiles <- bundles %>%
    dplyr::left_join(metafiles,by=c("session","bundle")) %>%
    dplyr::select(-file,-absolute_file_path)


  # Include the possibility of having default meta data for a sessions (in a _ses folder)
  sessJSONFiles <- list.files(file.path(emuDBhandle$basePath),pattern=paste0(".*.",metadata.extension),recursive = TRUE,full.names = FALSE)

  # Remove meta files associated with bundles

  sessJSONFiles <- sessJSONFiles[! grepl(bundle.dir.suffix,sessJSONFiles) & grepl(session.suffix,sessJSONFiles)]

  sessions <- list_sessions(emuDBhandle) %>%
    rename(session=name)

  # Run only if there are session metadata files
  if(length(sessJSONFiles) > 0){
    sessJSONFilesDF <- data.frame(stringr::str_split(sessJSONFiles,pattern = .Platform$file.sep,simplify = TRUE),stringsAsFactors=FALSE)
    names(sessJSONFilesDF) <- c("session","session_metadata_file")
    # The session needs to be without suffix so that metadata may be joinded by session later

    sessJSONFilesDF$session <- gsub(paste0(session.suffix,"$"),"",sessJSONFilesDF$session)

    sessJSONFilesDF <- na.omit(sessJSONFilesDF)


    for(row in 1:nrow(sessJSONFilesDF)){
      currFile <- as.vector(sessJSONFilesDF[[row,"session_metadata_file"]])
      currSession <- as.vector(sessJSONFilesDF[[row,"session"]])

      currSessionDir <- paste0(currSession,session.suffix)


      jsonmeta <- jsonlite::read_json(file.path(emuDBhandle$basePath,currSessionDir,currFile),simplifyVector = TRUE)

      # Now start inserting data from the session metadata file
      for(col in names(jsonmeta)){
        sessJSONFilesDF[sessJSONFilesDF$session == currSession,col] <- jsonmeta[[col]]
      }
    }

    #Add session meta data to the workbook,
    #or just empty sessions speficiations if there are no session metadata files

    sessJSONFilesDF <- sessions %>%
      dplyr::left_join(sessJSONFilesDF,by="session")

    # Make the merger with bundle files to make the final output tibble
    metafiles %>%
      dplyr::left_join(sessJSONFilesDF,by="session",suffix=c("","_sessionmetadatafile")) %>%
      dplyr::select(-session_metadata_file) -> metafiles

  }

  # Now check and load metadata set at the database level

  load_DBconfig(emuDBhandle) -> dbCfg

  if(is.null(dbCfg$metadataDefaults)){
    dbDefaults <- data.frame()
  }else{
    dbDefaults <- as.data.frame(dbCfg$metadataDefaults,stringsAsFactors=FALSE)
    if(length(dbDefaults) > 0){
      #This means that the field is not just empty
      # Repeat the rows so that the columns may be merged
      dbMeta <- as.data.frame(c(metafiles["bundle"],dbDefaults))  %>%

        dplyr::mutate_if(is.factor,as.character)
      metafiles <- metafiles %>%
        dplyr::mutate_if(is.factor,as.character)%>%
        dplyr::left_join(dbMeta,by="bundle",suffix=c("","_database")) %>%
        dplyr::distinct() ## This is needed since duplicate rows are introduced by the join by dbMeta


    }
  }

  #Now, there may be a metadata X column set at the bundle level, an X_sessionmetadatafile
  # column set at the session level, and an X_database column for the whole database.
  # These need to be reconsiled


  cols <- names(metafiles)
  duplicates <- grep("_(database|sessionmetadatafile)$",cols,value=TRUE)
  duplicated <- unique(gsub("_(database|sessionmetadatafile)$","",cols))


  for(bundleoriginal in duplicated){

    sessColName <- paste0(bundleoriginal,"_sessionmetadatafile")
    sessVec <- ifelse(exists(sessColName,metafiles),metafiles[,sessColName],NA)
    dbColName <- paste0(bundleoriginal,"_database")
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

  #Prepare an Excel workbook, if one should be written
  if(!is.null(Excelfile)){
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
    openxlsx::writeDataTable(wb,"database",x=dbDefaults,keepNA = FALSE,withFilter=FALSE)
    # We do not need to check owrwriting here as that is handled by saveWorkbook
    openxlsx::saveWorkbook(wb,file=Excelfile,overwrite=overwrite)
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
#' The simples way to get an appropriately structed Excel file is to create one from a database using the
#' \code{\link{export_metadata}} function on an existing database and given an output file.
#'
#' Please be aware that bundles that are speficied in the Excel file will have
#' their metadata files (ending with '.meta_json') overwritten when using the
#' \code{import_metadata}. So, please make sure to remove the rows of bundles that should
#' not be altered from the Excel file before importing the metadata from it using this function.
#'
#' Date and time fields are assumed to follow the ISO8601 specification, and an attempt to convert them to the
#' approprite JSON representation will be made. The user should be aware that this conversion is made however, and
#' watch out unexpected results in advanced cases.
#'
#' @param emuDBhandle The emuR database handle of the database.
#' @param Excelfile The path to a properly formated Excel (.xlsx) file.
#'
#' @return A vector of .meta_json files updated by the call. The path for each file is given relative to the base of the EmuR database.
#' @export
#'
import_metadata <- function(emuDBhandle,Excelfile){
  if(!file.exists(Excelfile)){
    stop("Unable to open the metadata Excel file.\nThe file ",Excelfile," does not exist!")
  }
  openxlsx::read.xlsx(Excelfile,sheet="bundles",detectDates=TRUE) -> meta

  #Make sure we have an output file with full path
  meta <- meta %>%

    dplyr::mutate(metadatafile=file.path(emuDBhandle$basePath,
                                  paste0(session,session.suffix),
                                  paste0(bundle,bundle.dir.suffix),
                                  paste0(bundle,".",metadata.extension))
           )
  #Now to the main business of the function

  json <- c()
  for(r in 1:nrow(meta)){
    meta %>%

      dplyr::slice(r) %>%
      dplyr::select(-session,-bundle,-metadatafile) %>%
      dplyr::select_if(function(x) !is.na(x)) -> jsondat
      currJSON <- ifelse(length(jsondat) > 0,
                         jsonlite::toJSON(jsondat,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows"),
                         "[{}]" #Just an empty JSON vector
                         )
      json <- c(json, currJSON)
  }
  json <- data.frame("json"=json)
  towrite <- meta %>%

    dplyr::bind_cols(json) %>%
    dplyr::mutate(json=as.character(json)) %>%
    dplyr::select(json,metadatafile)

  #Write the bundle metadata files
  for(r in 1:nrow(towrite)){
    fileConn <- file(towrite[r,"metadatafile"])
    writeLines(towrite[r,"json"], fileConn)
    close(fileConn)
  }
  bFiles <- gsub(paste0(emuDBhandle$basePath,"/"),"",towrite[["metadatafile"]])

  ## Now process session metadata files

  openxlsx::read.xlsx(Excelfile,sheet="sessions") -> sessionMeta
  sessjsondat <- sessionMeta %>%
    dplyr::select(-session)
  json <- sessjsondat %>%
    dplyr::rowwise() %>%
    dplyr::do(json=jsonlite::toJSON(.,raw="base64",na="null",complex="string",factor="string",POSIXt="ISO8601",Date="ISO8601",null="null",dataframe = "rows")) %>%
    unlist()
  json <- data.frame("json"=as.vector(json))

  towriteSess <- sessionMeta %>%
    dplyr::mutate(session_metadata_file=paste0(session,".",metadata.extension)) %>%
    dplyr::bind_cols(json) %>%
    dplyr::select(session,session_metadata_file,json)
  ## Här finns inte session_metadata_file
  #Write the bundle metadata files
  for(r in 1:nrow(towriteSess)){
    outFile <- file.path(emuDBhandle$basePath,
                         paste0(towriteSess[r,"session"],session.suffix)
                         towriteSess[r,"session_metadata_file"])
    fileConn <- file(outFile)
    writeLines(as.character(towriteSess[r,"json"]), fileConn)
    close(fileConn)
    outFile <- gsub(paste0(emuDBhandle$basePath,"/"),"",outFile)
    sFiles <- ifelse(exists("sFiles"),c(sFiles,outFile),c(outFile))
  }

  # Now inject database wide metadata

  load_DBconfig(emuDBhandle) -> dbCfg
  openxlsx::read.xlsx(Excelfile,sheet="database") -> dbMeta
  dbCfg$metadataDefaults <- as.list(dbMeta)
  store_DBconfig(emuDBhandle,dbCfg)

  return(c(sFiles,bFiles))
}



#' A utility function used for programatically setting metadata for a bundle, or default values for a session or an entire database.
#'
#' The function takes a list and a specification of where the metadata should be set. The default behaviour is to
#' keep already set metadata, and overwrite only the values that are named in the list. The user may change this
#' behaviour by setting \code{reset.before.add=TRUE}, in which case all previous  bundle, session
#' or database level metadata will be replaced with the contents of the list.
#'
#' If a bundle name and a \code{session} name is provided, the metadata will be inserted only for that fully speficied \code{bundle}.
#' If only a \code{bundle} name is provided, the function will add the metadata for the bundle only if there is just
#' one session in the database. If there are multiple \code{session}s, the function will given an error.
#'
#' If no \code{session} or \code{bundle} names are provided, the metadata will be inserted as default values for the entire database.
#'
#' @param emuDBhandle An Emu database handle
#' @param metadataList A list specifying the metadata to be set. If set to an empty list (\code{list()}) the function will clear all metadata, if the argument \code{reset.before.add=TRUE} is given by the user.
#' @param bundle An optional name of a bundle
#' @param session An optional name of a session
#' @param reset.before.add If set to TRUE, the function will ignore previously set metadata and simply add the metadata supplied in the list.
#'
#' @return
#' @export
#'
#'
add_metadata <- function(emuDBhandle,metadataList,bundle=NULL,session=NULL, reset.before.add=FALSE){

  if(is.null(bundle) & is.null(session)){
    #Database wide injection

    load_DBconfig(emuDBhandle) -> dbCfg
    
    if(reset.before.add){
      dbCfg$metadataDefaults <- as.list(metadataList)
    } else {
      #Append data
      prev <- dbCfg$metadataDefaults
      prev[names(metadataList)] <- metadataList
      dbCfg$metadataDefaults <- prev
    }


    store_DBconfig(emuDBhandle,dbCfg)

  } else {
    # Here we store metadata in either session wide or bundle speficit metadata files
    # Since these files use the same structure, the business here is to set the correct metadatafile filename.

    if(! is.null(session) & is.null(bundle)){
      #Session level metadata

      metadatafile <- file.path(emuDBhandle$basePath,
                           paste0(session,session.suffix),
                           paste0(session,".",metadata.extension))
    }


    if(! is.null(bundle)){
      #Bundle  metadata
      if(is.null(session)){
        ses <- list_sessions(ae_test)
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
                                paste0(bundle,".",metadata.extension))


    }
    if(reset.before.add | ! file.exists(metadatafile) ){
      #Start fresh / overwrite previous values
      jsonmetaList <- list()
    }else{

      #Read in previous values
      jsonmetaList <- as.list(jsonlite::read_json(metadatafile,simplifyVector = TRUE))

    }
    #set / overwrite metadata from list
    jsonmetaList[names(metadataList)] <- metadataList

    jsonlite::write_json(jsonmetaList,metadatafile)
  }
}

#' Add identifying information based on the content of the wave file to the metadata information for the bundle.
#'
#' This function will extract information (lenght of recording and a checksum) from the wav file associated with a bundle, and add it to the set of metadata
#' for the bundle. This information can later be used to verify that the file has not been altered later on, or to deidentify
#' wav files in a reversable manner for use outside of the emuR framework. Deidentified files are sometimes useful for blinded randomized
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
#' create_ae_db() -> ae
#' add_digests(ae)
#' export_metadata(ae,Excelfile = NULL) -> res
#' print(res)
#' unlink_emuRDemoDir()
#' }
#'
add_digests <- function(emuDBhandle,sessionPattern=".*",bundlePattern=".*",algorithm="sha1"){
  wavs <- list_files(emuDBhandle,fileExtension = "*.wav",sessionPattern=sessionPattern,bundlePattern=bundlePattern)
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
#' ## This code just sets up a new emuR database and inserts some
#' ## fake metadata into it.
#' create_ae_db() -> ae_test
#' make_dummy_metafiles(ae_test)
#' # Get all the 'n' segments in the database
#' query(ae_test,"Phonetic = n",resultType = "tibble") -> ae_nt
#' # Add information related to the nature the recording sessions
#' # e.g. the speaker ID, the date of the recording
#' ae_nt %>% biographize(ae_test)
#' # This code does the same as the above, but it will also compute new
#' # information that is strictly  aimed at identifying the recording
#' # (length of recording (in ms) and a sha1 digest of the wav file).
#' ae_nt %>% biographize(ae_test,compute_digests=TRUE,algorithm="sha1")
#' }
#'
biographize <- function(segs_tbl,emuDBhandle,compute_digests=FALSE,algorithm="sha1") {
  #make sure that the first argument is a segment list, and that
  # it contains "session" and "bundle" columns.
  if(! is.data.frame(segs_tbl) || !c("session", "bundle") %in% names(segs_tbl)){
    out <- paste("The input to the",match.call()[[1]], "has to be a 'tibble' or a 'data.frame'.")
    stop(out)
  }
  if(compute_digests==TRUE){
    add_digests(emuDBhandle,algorithm = algorithm)
  }
  #Here we use the special mode of export_medatata to get a data structure rather than an Excel file.
  mdata <- get_metadata(emuDBhandle,session = ".*")

  out <- segs_tbl %>% 
    dplyr::left_join(mdata,by = c("session", "bundle"))

  return(out)
}
