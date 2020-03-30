# context("Bundle (session and database) metadata")
# 

dbName = "ae"
path2demoData = file.path(tempdir(),
                          "emuR_demoData")
path2orig = file.path(tempdir(), 
                      "emuR_demoData", 
                      paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), 
                          "emuR_testthat")
path2db = file.path(path2testData, 
                    paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", 
                   envir = .emuR_pkgEnv)

# 
test_that("Metadata is collected correctly by get_metadata",{
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromLegacy"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae_test = load_emuDB(path2db, 
                  verbose = FALSE)
  
  ## Create a second session
  dirs <- list.dirs(file.path(ae_test$basePath,"0000_ses"))
  dirs <- gsub("0000_ses","0001_ses",dirs)
  for(currDir in dirs){
    dir.create(currDir,showWarnings = FALSE,recursive = TRUE)
  }
  
  inFiles <- list.files(file.path(ae_test$basePath,"0000_ses"),full.names = TRUE,recursive = TRUE,include.dirs = FALSE)
  outFiles <- gsub("0000_ses","0001_ses",inFiles)
  file.copy(from=inFiles,to=outFiles,recursive = FALSE)
  

  make_dummy_metafiles(ae_test)

  res <- get_metadata(ae_test)
  resnames <- names(as.data.frame(res))
  namesShouldBe <- c("session", "bundle", "Session.Date", "Session.Time", "Participant.ID", "Researcher", "Gender", "Condition", "Setup")
  outputShouldBe <- openxlsx::read.xlsx(file.path("..","metadata_extras","expected_meta.xlsx"),sheet="bundles")


  expect_named(res,namesShouldBe)
  expect_equal(res,outputShouldBe)

}

)

test_that("Import of metadata from an Excel file produces an exected result",{
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromLegacy"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae_test = load_emuDB(path2db, 
                       verbose = FALSE)
  
  ## Create a second session
  dirs <- list.dirs(file.path(ae_test$basePath,"0000_ses"))
  dirs <- gsub("0000_ses","0001_ses",dirs)
  for(currDir in dirs){
    dir.create(currDir,showWarnings = FALSE,recursive = TRUE)
  }
  
  inFiles <- list.files(file.path(ae_test$basePath,"0000_ses"),full.names = TRUE,recursive = TRUE,include.dirs = FALSE)
  outFiles <- gsub("0000_ses","0001_ses",inFiles)
  file.copy(from=inFiles,to=outFiles,recursive = FALSE)
  
  make_dummy_metafiles(ae_test)
  dummyRes <- get_metadata(ae_test)

  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromLegacy"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae_test = load_emuDB(path2db, 
                       verbose = FALSE)
  
  ## Create the second session again
  dirs <- list.dirs(file.path(ae_test$basePath,"0000_ses"))
  dirs <- gsub("0000_ses","0001_ses",dirs)
  for(currDir in dirs){
    dir.create(currDir,showWarnings = FALSE,recursive = TRUE)
  }
  
  inFiles <- list.files(file.path(ae_test$basePath,"0000_ses"),full.names = TRUE,recursive = TRUE,include.dirs = FALSE)
  outFiles <- gsub("0000_ses","0001_ses",inFiles)
  file.copy(from=inFiles,to=outFiles,recursive = FALSE)

  import_metadata(ae_test,file.path("..","metadata_extras","expected_meta.xlsx"))
  importRes <- get_metadata(ae_test)
  importRes <- importRes %>%
    dplyr::select(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup) %>%
    dplyr::arrange(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup)

  dummyRes <- dummyRes %>%
    dplyr::select(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup) %>%
    dplyr::arrange(session,bundle,Session.Date,Session.Time,Participant.ID,Researcher,Gender,Condition,Setup)


  expect_identical(na.omit(dummyRes), na.omit(importRes))
  expect_equal(as.list(table(is.na(dummyRes))),list(`FALSE`=122,`TRUE`=4))
  expect_equal(as.list(table(is.na(importRes))),list(`FALSE`=122,`TRUE`=4))
}
)

test_that("Test of the digest adding function",{
  # delete, copy and load
  unlink(path2db, recursive = TRUE)
  unlink(file.path(path2testData, "fromLegacy"), 
         recursive = TRUE)
  file.copy(path2orig, 
            path2testData, 
            recursive = TRUE)
  ae_test = load_emuDB(path2db, 
                       verbose = FALSE)
  
  add_digests(ae_test,algorithm="md5")
  add_digests(ae_test)
  add_digests(ae_test,algorithm="sha512")

  md <- get_metadata(ae_test)

  expect_false(all(is.na(md[c("Bundle.Duration.ms","Bundle.md5_checksum","Bundle.sha512_checksum")])))
}
)


