## Update cache of emuDB
## 
## Updates sqlite cache of loaded emuDB. This can be used
## to update changes to precached/loaded DBs as it only updates the deltas 
## in the cache which is considerably faster than reloading and therefore 
## recacheing the entire DB. This function is now called by load_emuDB if 
## load_emuDB finds a preexisting cache.
## @param emuDBhandle 
## @param verbose display infos
update_cache <- function(emuDBhandle, verbose = TRUE){
  
  DBconfig = load_DBconfig(emuDBhandle)
  
  # add missing index to cache
  DBI::dbExecute(emuDBhandle$connection, paste0("CREATE INDEX IF NOT EXISTS ",
                                                " items_level_seq_idx ",
                                                "ON items(db_uuid, session, bundle, level, seq_idx)"))
  DBI::dbExecute(emuDBhandle$connection, paste0("CREATE INDEX IF NOT EXISTS ",
                                                " label_nameLabel_idx ",
                                                "ON labels(name, label)"))
  
  # list sessions & bundles
  sessions = list_sessions(emuDBhandle)
  bundles = list_bundles(emuDBhandle)
  notUpdatedSessionDBI = list_sessionsDBI(emuDBhandle)
  notUpdatedBundlesDBI = list_bundlesDBI(emuDBhandle)
  
  # add column to sessions to track if already stored
  if(nrow(sessions) ==0 || nrow(bundles) == 0){
    return()
  }
  
  if(verbose){
    cat("INFO: Checking if cache needs update for", nrow(sessions), "sessions ")
  }
  
  
  # handle session
  sesDelta_new = dplyr::anti_join(sessions, notUpdatedSessionDBI, by = "name")
  sesDelta_toDelete = dplyr::anti_join(notUpdatedSessionDBI, sessions, by = "name")
  
  # add new
  if(nrow(sesDelta_new) > 0){
    for(i in 1:nrow(sesDelta_new)){
      add_sessionDBI(emuDBhandle, sesDelta_new[i,])
    }
  }
  # delete
  if(nrow(sesDelta_toDelete) > 0){
    for(i in 1:nrow(sesDelta_toDelete)){
      remove_sessionDBI(emuDBhandle, sesDelta_toDelete[i,])
    }
  }
  
  progress = 0
  
  if(verbose){
    cat("and", nrow(bundles), "bundles ...\n")
  }
  
  # delete all entries in all tables that don't have the uuid of the emuDB (happens by on delete cascade)
  numberOfRowsAffected = DBI::dbExecute(emuDBhandle$connection, 
                                        paste0("DELETE FROM emu_db WHERE uuid != '", emuDBhandle$UUID, "'"))
  if(numberOfRowsAffected != 0){
    cat(paste0("INFO: Found and deleted ",
               numberOfRowsAffected, 
               " elements in emuDBcache of emuDB with other UUID \n"))
  }
  
  # calculate all md5sums
  allAnnotFps = file.path(emuDBhandle$basePath, 
                          paste0(bundles$session, session.suffix), 
                          paste0(bundles$name, bundle.dir.suffix), 
                          paste0(bundles$name, bundle.annotation.suffix, ".json"))
  # remove all paths that don't contain _ses & _bndl just in case
  allAnnotFps_onlyAnnots = stringr::str_match(allAnnotFps, 
                                              pattern = ".*_ses.*_bndl.*_annot.json")
  
  if(verbose){
    cat("INFO: Performing precheck and calculating checksums (== MD5 sums) for _annot.json files ...\n")
  }
  
  file_md5sums = tools::md5sum(allAnnotFps_onlyAnnots)
  
  files_sesBndlMd5DF = data.frame(session = bundles$session, 
                                  name = bundles$name, 
                                  md5_annot_json = file_md5sums, 
                                  row.names = NULL, 
                                  stringsAsFactors = F)
  cache_sesBndlMd5DF = DBI::dbGetQuery(emuDBhandle$connection, 
                                       paste0("SELECT session, name, md5_annot_json FROM bundle"))
  
  bndlsDelta_new = dplyr::anti_join(files_sesBndlMd5DF, 
                                    cache_sesBndlMd5DF, 
                                    by = c("session", "name"))
  bndlsDelta_toDelete = dplyr::anti_join(cache_sesBndlMd5DF, 
                                         files_sesBndlMd5DF, 
                                         by = c("session", "name"))
  
  bndlsDelta_updated = dplyr::anti_join(files_sesBndlMd5DF, 
                                        cache_sesBndlMd5DF, 
                                        by = c("session", "name", "md5_annot_json"))
  bndlsDelta_updated = dplyr::anti_join(bndlsDelta_updated, 
                                        bndlsDelta_new, 
                                        by = c("session", "name", "md5_annot_json"))  # remove new
  bndlsDelta_updated = dplyr::anti_join(bndlsDelta_updated, 
                                        bndlsDelta_toDelete, 
                                        by = c("session", "name", "md5_annot_json"))  # remove toDelete
  
  # return if data.frames are the same
  if(nrow(bndlsDelta_new) == 0 & nrow(bndlsDelta_toDelete) == 0 & nrow(bndlsDelta_updated) == 0){
    if(verbose){
      cat("INFO: Nothing to update!\n")
    }
    return()
  }
  
  bndlsDelta_load = dplyr::bind_rows(bndlsDelta_new, bndlsDelta_updated)
  
  ##########################
  # as of here we loop manualy...
  if(verbose){
    cat("INFO: (Re)loading / deleting", nrow(bndlsDelta_load), "bundle(s) to / from emuDBcache ...\n")
    pb = utils::txtProgressBar(min = 0, max = nrow(bndlsDelta_load) + nrow(bndlsDelta_toDelete), initial = progress, style=3)
    utils::setTxtProgressBar(pb, progress)
  }
  # add
  if(nrow(bndlsDelta_load) > 0){
    DBI::dbBegin(emuDBhandle$connection)
    for(bndlIdx in 1:nrow(bndlsDelta_load)){
      
      bndl = bndlsDelta_load[bndlIdx,]
      
      # construct path to annotJSON
      annotFilePath = normalizePath(file.path(emuDBhandle$basePath, 
                                              paste0(bndl$session, session.suffix), 
                                              paste0(bndl$name, bundle.dir.suffix), 
                                              paste0(bndl$name, bundle.annotation.suffix, '.json')))
      
      # extract MD5 sum of bundle annotJSON
      newMD5annotJSON = files_sesBndlMd5DF[files_sesBndlMd5DF$session == bndl$session 
                                           & files_sesBndlMd5DF$name == bndl$name,]$md5_annot_json
      # read annotJSON as charac 
      #annotJSONchar = enc2utf8(readChar(annotFilePath, file.info(annotFilePath)$size)) # wrapped in enc2utf8 as readChar respects the system default (windows iso 88591)
      annotJSONchar = readr::read_file(annotFilePath)
      # convert to bundleAnnotDFs
      bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(annotJSONchar)
      # removing old bundle entry
      remove_bundleDBI(emuDBhandle, bndl$session, bndl$name)
      # and adding to bundle table
      add_bundleDBI(emuDBhandle, 
                    bndl$session, 
                    bndl$name, 
                    bundleAnnotDFs$annotates, 
                    bundleAnnotDFs$sampleRate, 
                    newMD5annotJSON)
      # and remove bundleAnnotDBI
      remove_bundleAnnotDBI(emuDBhandle, 
                            bndl$session, 
                            bndl$name)
      # add to items, links, labels tables
      store_bundleAnnotDFsDBI(emuDBhandle, 
                              bundleAnnotDFs, 
                              bndl$session, 
                              bndl$name)
      
      # increase progress bar  
      progress=progress+1L
      if(verbose){
        utils::setTxtProgressBar(pb,progress)
      }
    }
    DBI::dbCommit(emuDBhandle$connection)
  }
  
  # delete
  if(nrow(bndlsDelta_toDelete) > 0 ){
    for(i in 1:nrow(bndlsDelta_toDelete)){
      remove_bundleDBI(emuDBhandle, 
                       bndlsDelta_toDelete[i,]$session, 
                       bndlsDelta_toDelete[i,]$name)
      progress=progress+1L
      if(verbose){
        utils::setTxtProgressBar(pb,progress)
      }
    }
  }
}

# FOR DEVELOPMENT 
# library('testthat')
# test_file('tests/testthat/test_aaa_initData.R')
# test_file('tests/testthat/test_emuR-database.caching.R')
