##' Export annotations of emuDB to TextGrid collection
##' 
##' Exports the annotations of an emuDB to a TextGrid collection (.TextGrid and .wav file pairs).
##' To avoid naming conflicts and not to loose the session information, the session structure of 
##' the database is kept in place (i.e. the TextGrid collection will have sub-folders that are named
##' as the sessions were). Due to the more complex annotation structure modeling capabilities of 
##' the EMU-SDMS system, this export routine has to make several compromises on export which 
##' can lead to information loss. So use with caution and at own risk as reimporting the exported
##' data will mean that not all information can be recreated!
##' The main compromises are:
##' \itemize{
##'   \item If a MANY_TO_MANY relationship between two levels is present and 
##'   two items from the parent level are linked to a single item on the child level, the 
##'   concatenated using the '->' symbol. An example would be: the annotation items containing the labels 'd' and 'b' of the 
##'   parent items are merged into a single annotation item and their labels are 
##'   Phoneme level are linked to 'db' on the Phonetic level. The generated Phoneme tier then has a segment with the 
##'   start and end times of the 'db' item and contains the labels 'db' (see for example the 
##'   bundle 0000_ses/msajc010_bndl of the ae_emuDB).
##'   \item As annotations can contain gaps (e.g. incomplete hierarchies or orphaned items) and do not have to start at
##'   time 0 and be the length of the audio file this export routine pads these gaps with empty segments.
##' }
##' 
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param targetDir directory where the TextGrid collection should be saved
##' @param sessionPattern A regular expression pattern matching session names to be exported from the database
##' @param bundlePattern A regular expression pattern matching bundle names to be exported from the database
##' @param attributeDefinitionNames list of names of attributeDefinitions that are to be 
##' exported as tiers. If set to NULL (the default) all attribute definitions will be exported as separate tiers.
##' @param timeRefSegmentLevel parameter passed into \link{query} function. (set time segment level from which to derive time 
##' information. It is only necessary to set this parameter if more than one child 
##' level contains time information and the queried parent level is of type ITEM.)
##' @param verbose Show progress bars and further information
##' @export
##' @seealso \code{\link{load_emuDB}}
##' @keywords emuDB database query Emu EQL 
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded ae emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' ## Export all levels
##' export_TextGridCollection(ae, "/path/2/targetDir")
##' 
##' }
##' 
export_TextGridCollection <- function(emuDBhandle, 
                                      targetDir, 
                                      sessionPattern = '.*', 
                                      bundlePattern = '.*', 
                                      attributeDefinitionNames = NULL, 
                                      timeRefSegmentLevel = NULL,
                                      verbose = TRUE) {
  
  check_emuDBhandle(emuDBhandle)
  
  dbConfig = load_DBconfig(emuDBhandle)
  
  allAttrNames = get_allAttributeNames(emuDBhandle)
  
  if(!is.null(attributeDefinitionNames)){
    
    if(!all(attributeDefinitionNames %in% allAttrNames)){
      stop(paste0("Bad attributeDefinitionNames given! Valid ",
                  "attributeDefinitionNames of the emuDB are: ", 
                  paste0(allAttrNames, collapse = "; ")))
    }
    allAttrNames = attributeDefinitionNames
  }
  
  # create target dir
  if(!dir.exists(targetDir)){
    if(verbose){
      cat("targetDir DOESN'T exist! Creating new dir...\n")
    }
    dir.create(targetDir)
  }else{
    if(verbose){
      cat("targetDir exists! Using specified dir...\n")
    }
  }
  
  # extract all items as giant seglist
  if(verbose){
    cat("Querying all annotation items... (this may take a while!)\n")
  }
  
  slAll = NULL
  for(i in 1:length(allAttrNames)){
    sl = query(emuDBhandle, 
               paste0(allAttrNames[i], "=~ .*"), 
               resultType = "emuRsegs", # still uses old emuRsegs obj
               timeRefSegmentLevel = timeRefSegmentLevel,
               verbose = FALSE)
    slAll = dplyr::bind_rows(slAll, sl)
  }
  
  # convert times to seconds
  slAll$start = slAll$start / 1000
  slAll$end = slAll$end / 1000
  
  # extract rel.  bundles
  bndls = list_bundles(emuDBhandle)
  bndls = bndls[grepl(sessionPattern, bndls$session) & grepl(bundlePattern, bndls$name),]
  
  if(verbose){
    cat('\n  INFO: exporting', nrow(bndls), 'bundles\n')
    pb <- utils::txtProgressBar(min = 0, max = nrow(bndls), style = 3)
  }
  
  # loop through bundles and write to TextGrids & copy wav
  for(i in 1:nrow(bndls)){
    curSes = bndls[i, ]$session
    curBndl = bndls[i, ]$name
    # check if session folder exists
    sesDir = file.path(targetDir, bndls[i, ]$session)
    if(!dir.exists(sesDir)){
      dir.create(sesDir)
    }
    
    # copy wav file
    wavPath = file.path(emuDBhandle$basePath, 
                        paste0(curSes, session.suffix), 
                        paste0(curBndl, bundle.dir.suffix), 
                        paste0(curBndl, ".", dbConfig$mediafileExtension))
    file.copy(wavPath, sesDir)
    # extract bundle sl
    slBndl = slAll[grepl(paste0("^", curSes, "$"), 
                         slAll$session) 
                   & grepl(paste0("^", curBndl, "$"), 
                           slAll$bundle), ]
    tgPath = file.path(sesDir, paste0(curBndl, ".TextGrid"))
    
    wavDur = wrassp::dur.AsspDataObj(wrassp::read.AsspDataObj(wavPath))
    
    # tg header
    tgHeader = c("File type = \"ooTextFile\"",
                 "Object class = \"TextGrid\"", 
                 "",
                 "xmin = 0 ",
                 paste0("xmax = ", wavDur, " "),
                 "tiers? <exists> ",
                 paste0("size = ", length(allAttrNames), " "),
                 "item []: ")
    
    write(tgHeader, tgPath)
    
    for(attrNameIdx in 1:length(allAttrNames)){
      
      slTier = slBndl[slBndl$level == allAttrNames[attrNameIdx],]
      
      emptyRow = data.frame(labels = "", 
                            start = -1, 
                            end = -1, 
                            utts = "", 
                            db_uuid = "", 
                            session = "", 
                            bundle = "", 
                            start_item_id = "", 
                            end_item_id = "", 
                            level = "", 
                            start_item_seq_idx = "", 
                            end_item_seq_idx = "",
                            type = "", 
                            sample_start = "", 
                            sample_end = "", 
                            sample_rate = "", 
                            stringsAsFactors = F)
      # tier header
      if(all(slTier$end == 0)){
        tierType = "TextTier"
      }else{
        tierType = "IntervalTier"
        if(min(slTier$start) > 0){
          # add empty segment to left (== pad left)
          emptyRow$start = 0
          emptyRow$end = min(slTier$start)
          slTier = rbind(emptyRow, slTier)
        }
        
        if(max(slTier$end) < wavDur){
          # add empty segment to right (== pad right)
          emptyRow$start = max(slTier$end)
          emptyRow$end = wavDur
          slTier = rbind(slTier, emptyRow)
        }
        
        # check for empty and overlapping segments (caused by orphaned children in hierarchy)
        problemSegs = slTier[-nrow(slTier),]$end - slTier[-1,]$start != 0
        # check for overlapping segs
        overlSegs = slTier[-nrow(slTier),]$end - slTier[-1,]$start > 0
        # check for duplicate segs (caused by many_to_many -> elisions)
        dupliSegs = slTier[-nrow(slTier),]$start == slTier[-1,]$start & slTier[-nrow(slTier),]$end == slTier[-1,]$end
        
        if(any(problemSegs) | any(overlSegs) | any(dupliSegs)){
          slTierTmpNrow = nrow(slTier) + length(which(problemSegs)) - length(which(overlSegs)) - length(which(dupliSegs)) # remove overlSegs + dupliSegs from problemSegs (reason for minus)
          # preallocate data.frame
          slTierTmp = data.frame(labels = character(slTierTmpNrow), 
                                 start = integer(slTierTmpNrow), 
                                 end = integer(slTierTmpNrow), 
                                 utts = character(slTierTmpNrow), 
                                 db_uuid = character(slTierTmpNrow), 
                                 session = character(slTierTmpNrow), 
                                 bundle = character(slTierTmpNrow), 
                                 start_item_id = character(slTierTmpNrow), 
                                 end_item_id = character(slTierTmpNrow), 
                                 level = character(slTierTmpNrow), 
                                 start_item_seq_idx = integer(slTierTmpNrow), 
                                 end_item_seq_idx = integer(slTierTmpNrow),
                                 type = character(slTierTmpNrow), 
                                 sampleStart = integer(slTierTmpNrow), 
                                 sample_end = integer(slTierTmpNrow), 
                                 sample_rate = integer(slTierTmpNrow), 
                                 stringsAsFactors = F)
          
          slTierTmp[1,] = slTier[1,]
          curRowIdx = 2
          dupliSegsRowIdx = NULL
          for(slTierRowIdx in 2:nrow(slTier)){
            if(slTier[slTierRowIdx - 1,]$end < slTier[slTierRowIdx,]$start){
              # add empty segment
              emptyRow$start = slTier[slTierRowIdx - 1,]$end
              emptyRow$end = slTier[slTierRowIdx,]$start
              slTierTmp[curRowIdx,] = emptyRow
              curRowIdx = curRowIdx + 1
              slTierTmp[curRowIdx,] = slTier[slTierRowIdx,]
              curRowIdx = curRowIdx + 1
              
            }else{
              if(slTier[slTierRowIdx - 1,]$end > slTier[slTierRowIdx,]$start 
                 | slTier[slTierRowIdx - 1,]$start == slTier[slTierRowIdx,]$start 
                 & slTier[slTierRowIdx - 1,]$end == slTier[slTierRowIdx,]$end){ 
                # overlapping or duplicate
                slTierTmp[curRowIdx,] = slTier[slTierRowIdx,]
                slTierTmp[curRowIdx,]$labels = paste0(slTier[slTierRowIdx - 1,]$labels, 
                                                      "->", 
                                                      slTier[slTierRowIdx,]$labels)
                slTierTmp[curRowIdx,]$start = slTier[slTierRowIdx - 1,]$start
                slTierTmp[curRowIdx,]$end = slTier[slTierRowIdx - 1,]$end
                dupliSegsRowIdx = c(dupliSegsRowIdx, curRowIdx - 1)
                curRowIdx = curRowIdx + 1
              }else{
                slTierTmp[curRowIdx,] = slTier[slTierRowIdx,]
                curRowIdx = curRowIdx + 1
              }
            }
          }
          
          
          if(is.null(dupliSegsRowIdx)){
            slTier = slTierTmp 
          }else{
            slTier = slTierTmp[-1 * dupliSegsRowIdx,]
          }
        }
      }
      tierHeader = c(paste0("    item [", attrNameIdx, "]:"), 
                     paste0("        class = \"", tierType, "\" "),
                     paste0("        name = \"", allAttrNames[attrNameIdx], "\" "),
                     "        xmin = 0 ",
                     paste0("        xmax = ", wavDur, " "))
      
      write(tierHeader, tgPath, append=TRUE)
      
      # tier items
      if(tierType == "IntervalTier"){
        tierItems = c(paste0("        intervals: size = ", nrow(slTier), " "), 
                      c(rbind(paste0("        intervals [",1:nrow(slTier), "]:"), 
                              paste0("            xmin = ", slTier$start, " "),
                              paste0("            xmax = ", slTier$end, " "),
                              paste0("            text = \"", slTier$labels, "\" "))))
        
      }else{
        tierItems = c(paste0("        points: size = ", nrow(slTier), " "), 
                      c(rbind(paste0("        points [",1:nrow(slTier), "]:"), 
                              paste0("            number = ", slTier$start, " "),
                              paste0("            mark = \"", slTier$labels, "\" "))))
        
      }
      write(tierItems, tgPath, append=TRUE)
    }
    
    # increase pb
    if(verbose){
      utils::setTxtProgressBar(pb, i)
    }
    
  }
  
  # close progress bar if open
  if(exists('pb')){
    close(pb)
  }
  
  
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_emuR-export_TextGridCollection.R')

