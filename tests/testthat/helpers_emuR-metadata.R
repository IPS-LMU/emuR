library(emuR)
library(dplyr)

create_ae_db <- function(){
  demodir <- file.path(tempdir(),"emuR_demoData")

  if(!dir.exists(demodir)){
    create_emuRdemoData()
  }
  db <- load_emuDB(file.path(demodir,"ae_emuDB"))

  # We need two copies of the session, which in R requires som manual intervention
  dirs <- list.dirs(file.path(db$basePath,"0000_ses"))
  dirs <- gsub("0000_ses","0001_ses",dirs)
  for(currDir in dirs){
    dir.create(currDir,showWarnings = FALSE,recursive = TRUE)
  }

  inFiles <- list.files(file.path(db$basePath,"0000_ses"),full.names = TRUE,recursive = TRUE,include.dirs = FALSE)
  outFiles <- gsub("0000_ses","0001_ses",inFiles)
  file.copy(from=inFiles,to=outFiles,recursive = FALSE)
  return(db)
}

unlink_emuRDemoDir <- function(){
  demodir <- file.path(tempdir(),"emuR_demoData")
  res <- unlink(demodir,recursive = TRUE)
  binRes <- c(TRUE,FALSE)[res+1]
  return(binRes)
}


make_dummy_metafiles <- function(db){


  sess1 <- file.copy(from=file.path("..","metadata_extras","session.meta"),to=file.path(db$basePath,"0000_ses","0000.meta_json"))

  sess2 <- file.copy(from=file.path("..","metadata_extras","session_0001.meta"),to=file.path(db$basePath,"0001_ses","0001.meta_json"))
  outMetaFiles <- list_files(db,"wav") %>%
    dplyr::select(absolute_file_path) %>%
    dplyr::mutate(absolute_file_path=gsub("wav$",metadata.extension,absolute_file_path)) %>%
    dplyr::arrange(absolute_file_path) %>%
    dplyr::slice(-1,-11) ## One file per session should be missing so that we may test bundle and session defaults
  # "ae_emuDB/0000_ses/msajc003_bndl/msajc003.meta_json"
  # "ae_emuDB/0001_ses/msajc015_bndl/msajc015.meta_json"
  # should be missing.

  res <- file.copy(from=file.path("..","metadata_extras","bundle.meta"),to=outMetaFiles[[1]])

  res <- c(sess1,sess2,res)
  # store database wide default values
  emuR:::load_DBconfig(db) -> dbCfg
  dbCfg$metadataDefaults <- jsonlite::read_json(file.path("..","metadata_extras","db.meta"),simplifyVector = FALSE) ## Keep the list strucure since this is going directly to json
  emuR:::store_DBconfig(db,dbConfig = dbCfg)

}

