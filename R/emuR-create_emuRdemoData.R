##' Create demo data for the emuR package
##' 
##' Create a folder within the folder specified
##' by the dir argument called emuR_demoData.
##' This folder contains the folders:
##' \itemize{
##' \item{ae_emuDB: Containing an emuDB that adheres to the new format specification 
##' (as expected by the \code{\link{load_emuDB}} function). See \code{vignette(emuDB)} 
##' for more information on this database format.}
##' \item{BPF_collection: Containing a BAS Partitur Format (BPF) file collection (as
##' expected by the \code{\link{convert_BPFCollection}} function)}
##' \item{legacy_ae: Containing a legacyEmuDB (as expected by the 
##' \code{\link{convert_legacyEmuDB}} function)}
##' \item{TextGrid_collection: Containing a TextGrid file collection 
##' (as expected from the \code{\link{convert_TextGridCollection}} function)}
##' }
##' @param dir directory to create demo data in (default= tempdir())
##' @param precache creates an on-file-system cache for the ae emuDB to allow fast loading
##' (see \code{load_emuDB} for details about the emuDB file cache)
##' @export
##' @examples 
##' \dontrun{
##' 
##' # create demo data directory in directory
##' # provided by the tempdir function
##' create_emuRdemoData(dir = tempdir())
##' }
create_emuRdemoData <- function(dir = tempdir(), precache = FALSE){
  
  ddPath = file.path(dir,"emuR_demoData")
  
  path2data = system.file("extdata", package = "emuR")
  
  if(file.exists(ddPath)){
    stop("Path '", ddPath,"' already exists!")
  }
  
  created = dir.create(ddPath)
  if(!created){
    stop("Couldn't created ", ddPath)
  }
  
  #################################
  # create scripts
  matlabSourcePaths = list.files(file.path(path2data, "rawDemoData", "scriptFiles"),
                                 pattern = ".m$",
                                 full.names = TRUE)
  
  scriptsDestinationPath = file.path(ddPath, "add_signal_scripts")
  matlabDestinationPath  = file.path(scriptsDestinationPath, "matlab")
  
  created = dir.create(scriptsDestinationPath)
  if(!created){
    stop("Couldn't create ", scriptsDestinationPath)
  }
  created = dir.create(matlabDestinationPath)
  if(!created){
    stop("Couldn't create ", matlabDestinationPath)
  }
  
  file.copy(matlabSourcePaths, matlabDestinationPath)

  
  #################################
  # create ae
  configPath = list.files(path2data, 
                          pattern = "DBconfig.json$", 
                          recursive = TRUE,
                          full.names = TRUE)
  wavPaths = list.files(path2data, 
                        pattern = ".wav$", 
                        recursive = TRUE,
                        full.names = TRUE)
  annotPaths = list.files(path2data, 
                          pattern = "_annot.json$", 
                          recursive = TRUE,
                          full.names = TRUE)
  aePath = file.path(ddPath, 
                     paste0("ae", emuDB.suffix))
  
  created = dir.create(aePath)
  if(!created){
    stop("Couldn't create ", aePath)
  }
  
  file.copy(configPath, aePath)
  
  sesPath = file.path(aePath, "0000_ses")
  created = dir.create(sesPath)
  if(!created){
    stop("Couldn't create ", sesPath)
  }
  for(p in wavPaths){
    bndlName = gsub(".wav$", "", basename(p))
    bndlPath = file.path(sesPath, paste0(bndlName, "_bndl"))
    dir.create(bndlPath)
    
    file.copy(p, bndlPath)
    idx = grep(paste0(bndlName, "_annot.json$"), annotPaths)
    file.copy(annotPaths[idx], bndlPath)
    
  }
  
  # calc dft and fms files
  wps = list.files(sesPath, 
                   pattern = ".wav$", 
                   recursive = TRUE,
                   full.names = TRUE)
  wrassp::dftSpectrum(wps, verbose = FALSE)
  wrassp::forest(wps, verbose = FALSE)
  
  
  # generate cache of ae emuDB
  if(precache){
    dbHandle = load_emuDB(aePath, inMemoryCache = FALSE, verbose = FALSE)
    DBI::dbDisconnect(dbHandle$connection)
  }
  
  ####################################
  # create TextGrid_collection, BPF_collection and txt_collection
  fpltgc = create_filePairList(path2data, 
                               path2data, 
                               "wav", 
                               "TextGrid")
  fplbpf_original = create_filePairList(path2data, 
                                        path2data, 
                                        "wav", 
                                        "par")
  fplbpf_manipulated = create_filePairList(path2data, 
                                           path2data, 
                                           "wav", 
                                           "parmanipulated")
  fpltxt = create_filePairList(path2data, 
                               path2data, 
                               "wav", 
                               "txt")
  tgcPath = file.path(ddPath, 
                      "TextGrid_collection")
  bpfPath_original = file.path(ddPath, 
                               "BPF_collection")
  txtcPath = file.path(ddPath, 
                       "txt_collection")
  
  created = dir.create(tgcPath)
  if(!created){
    stop("Couldn't create ", tgcPath)
  }
  
  created = dir.create(bpfPath_original)
  if(!created){
    stop("Couldn't create ", bpfPath_original)
  }
  
  created = dir.create(txtcPath)
  if(!created){
    stop("Couldn't create ", txtcPath)
  }
  
  
  file.copy(fpltgc[,1], tgcPath)
  file.copy(fpltgc[,2], tgcPath)
  file.copy(fplbpf_original[,1], bpfPath_original)
  file.copy(fplbpf_original[,2], bpfPath_original)
  file.copy(fpltxt[,1], txtcPath)
  file.copy(fpltxt[,2], txtcPath)
  
  #################################
  # create legacyEmuDB
  tplPath = list.files(path2data, 
                       pattern = ".tpl$", 
                       recursive = TRUE,
                       full.names = TRUE)
  wavPaths = list.files(path2data, 
                        pattern = ".wav$", 
                        recursive = TRUE,
                        full.names = TRUE)
  hlbPaths = list.files(path2data, 
                        pattern = "hlb$", 
                        recursive = TRUE,
                        full.names = TRUE)
  labPaths = list.files(path2data, 
                        pattern = "lab$", 
                        recursive = TRUE,
                        full.names = TRUE)
  tonePaths = list.files(path2data, 
                         pattern = "tone$", 
                         recursive = TRUE,
                         full.names = TRUE)
  
  legacyAePath = file.path(ddPath, "legacy_ae")
  created = dir.create(legacyAePath)
  if(!created){
    stop("Couldn't create ", legacyAePath)
  }
  
  labelsPath = file.path(legacyAePath, "labels")
  created = dir.create(labelsPath)
  if(!created){
    stop("Couldn't create ", legacyAePath)
  }
  
  signalsPath = file.path(legacyAePath, "signals")
  created = dir.create(signalsPath)
  if(!created){
    stop("Couldn't create ", legacyAePath)
  }
  
  # copy files
  file.copy(tplPath, legacyAePath)
  file.copy(wavPaths, signalsPath)
  file.copy(hlbPaths, labelsPath)
  file.copy(labPaths, labelsPath)
  file.copy(tonePaths, labelsPath)
  
  # calc dft and fms files
  wps = list.files(signalsPath, pattern = ".wav$", recursive = TRUE, full.names = TRUE)
  wrassp::dftSpectrum(wps, verbose = FALSE)
  wrassp::forest(wps, verbose = FALSE)
  
  return(invisible())
}

## create manipulated BPF_collection
##
## @param dir directory in that the BPF_collection is created
create_BPFcollectionManipulated = function(dir){
  
  path2data = system.file("extdata", package = "emuR")
  
  bpfPath_manipulated = file.path(dir, "BPF_collection_manipulated")
  
  if(file.exists(bpfPath_manipulated)){
    stop("Path '", bpfPath_manipulated,"' already exists!")
  }
  
  created = dir.create(bpfPath_manipulated)
  if(!created){
    stop("Couldn't create ", bpfPath_manipulated)
  }
  
  
  fplbpf_manipulated = create_filePairList(path2data, 
                                           path2data, 
                                           "wav", 
                                           "parmanipulated")
  
  created = dir.create(file.path(bpfPath_manipulated, "0000"))
  if(!created){
    stop("Couldn't create ", file.path(bpfPath_manipulated, "0000"))
  }
  file.copy(fplbpf_manipulated[,1], 
            file.path(bpfPath_manipulated, "0000"))
  file.copy(fplbpf_manipulated[,2], 
            file.path(bpfPath_manipulated, "0000"))
  
}

########################
# FOR DEVELOPMENT 
# unlink(file.path(tempdir(),"emuR_demoData"), recursive = TRUE)
# create_emuRdemoData(precache = TRUE)
