##' testthat tests for autoproc_annots
##'
context("testing autoproc_annots")

dbName = "ae"
path2orig = file.path(tempdir(), "emuR_demoData", paste0(dbName, emuDB.suffix))
path2testData = file.path(tempdir(), "emuR_testthat")
path2db = file.path(path2testData, paste0(dbName, emuDB.suffix))

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

test_that("replace_itemLabels works correctly", {

  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)

  test_that("replace_itemLabels throws correct errors", {

    expect_error(replace_itemLabels(ae, attributeDefinitionName = "badName", origLabels = "a", newLabels = "a"), regexp = "No attributeDefinitionName: badName", ignore.case = T)
    expect_error(replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = "a", newLabels = c("a","b")), regexp = "origLabels and newLabels have to be", ignore.case = T)
    expect_error(replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = 1, newLabels = "a"), regexp = "origLabels and newLabels have to be", ignore.case = T)
    expect_error(replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = "a", newLabels = 1), regexp = "origLabels and newLabels have to be", ignore.case = T)
  })


  test_that("replace_itemLabels replaces correct labels", {

    replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = "n", newLabels = "n_rep", verbose = F)

    sl = query(ae, "Phonetic == n")
    expect_equal(nrow(sl), 0)
    sl = query(ae, "Phonetic == n_rep")
    expect_equal(nrow(sl), 12)

    replace_itemLabels(ae, attributeDefinitionName = "Phonetic", origLabels = c("I", "p"), newLabels = c("I_rep", "p_rep"), verbose = F)

    sl = query(ae, "Phonetic == I_rep")
    expect_equal(nrow(sl), 14)
    sl = query(ae, "Phonetic == p_rep")
    expect_equal(nrow(sl), 2)
  })

  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL

})


test_that("duplicate_level works correctly", {

  # delete, copy and load
  unlink(path2db, recursive = T)
  file.copy(path2orig, path2testData, recursive = T)
  ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)

  test_that("duplicate_level throws correct errors", {

    expect_error(duplicate_level(ae, 
                                 levelName = "badName", 
                                 duplicateLevelName = "bla"), 
                 regexp = "not a valid level name", 
                 ignore.case = T)
    expect_error(duplicate_level(ae, 
                                 levelName = "Phonetic", 
                                 duplicateLevelName = "bla", 
                                 duplicateLinks = T, 
                                 linkDuplicates = T), 
                 regexp = "duplicateLinks & linkDuplicates", 
                 ignore.case = T)
    
    # unique  multiple attribute definitions
    # expect_error(duplicate_level(ae, 
    #                 levelName = "Word", 
    #                 duplicateLevelName = "Word2", 
    #                 verbose = F), 
    #              regexp = "attributeDefinition with name")

  })

  test_that("duplicate_level works correctly", {
    duplicate_level(ae, 
                    levelName = "Phonetic", 
                    duplicateLevelName = "Phonetic2", 
                    verbose = F)
    dbConfig = load_DBconfig(ae)
    expect_true(length(dbConfig$levelDefinitions) == 10)
    expect_equal(dbConfig$levelDefinitions[[10]]$name, "Phonetic2")
    # check items
    oldIts = DBI::dbGetQuery(ae$connection, "SELECT * FROM items WHERE level = 'Phonetic'")
    newIts = DBI::dbGetQuery(ae$connection, "SELECT * FROM items WHERE level = 'Phonetic2'")
    expect_equal(nrow(oldIts), nrow(newIts))
    # check labels
    oldLabs = DBI::dbGetQuery(ae$connection, "SELECT * FROM labels WHERE name = 'Phonetic'")
    newLabs = DBI::dbGetQuery(ae$connection, "SELECT * FROM labels WHERE name = 'Phonetic2'")
    expect_equal(nrow(oldLabs), nrow(newLabs))
    # check labelGroups
    oldLG = list_attrDefLabelGroups(ae, "Phonetic", "Phonetic")
    newLG = list_attrDefLabelGroups(ae, "Phonetic2", "Phonetic2")
    expect_equal(nrow(oldLG), nrow(newLG))

  })

  test_that("duplicateLinks = F works correctly", {
    duplicate_level(ae, levelName = "Phonetic", duplicateLevelName = "Phonetic3", duplicateLinks = F, verbose = F)
    linkDefs = list_linkDefinitions(ae)
    # no linkdefs are added
    expect_false("Phonetic3" %in% linkDefs$superlevelName)
    expect_false("Phonetic3" %in% linkDefs$sublevelName)
  })


  test_that("linkDuplicates works correctly", {
    duplicate_level(ae, levelName = "Phonetic", duplicateLevelName = "Phonetic4", duplicateLinks = F, linkDuplicates = T, verbose = F)
    linkDefs = list_linkDefinitions(ae)
    # linkdefs are added
    expect_true("Phonetic4" %in% linkDefs$sublevelName)

    sl1 = query(ae, "[Phonetic == n ^ #Word =~.*]", timeRefSegmentLevel = "Phonetic")
    sl2 = query(ae, "[Phonetic4 == n ^ #Word =~.*]", timeRefSegmentLevel = "Phonetic4")

    expect_true(all(sl1 == sl2))
  })

  # clean up
  DBI::dbDisconnect(ae$connection)
  ae = NULL
  unlink(path2db, recursive = T)
})

# test_that("resample annots works correctly", {
#   
#   # delete, copy and load
#   unlink(path2db, recursive = T)
#   file.copy(path2orig, path2testData, recursive = T)
#   ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
#   
#   # test_that("duplicate_level throws correct errors", {
#   #   
#   #   expect_error(duplicate_level(ae, levelName = "badName", duplicateLevelName = "bla"), regexp = "not a valid level name", ignore.case = T)
#   #   expect_error(duplicate_level(ae, levelName = "Phonetic", duplicateLevelName = "bla", duplicateLinks = T, linkDuplicates = T), regexp = "duplicateLinks & linkDuplicates", ignore.case = T)
#   #   
#   # })
# 
#   test_that("correct updates are made to cache", {
#     
#     # resample_annots(ae, oldSampleRate = 20000, newSampleRate = 44100, verbose = TRUE)
#     
#   })
# 
#   test_that("correct updates are made to annotation files", {
#     
#   })
#   
#     
#   # clean up
#   DBI::dbDisconnect(ae$connection)
#   ae = NULL
#   unlink(path2db, recursive = T)
# })

# test_that("add_itemsToEmptyLevel works correctly", {
#   
#   # delete, copy and load
#   unlink(path2db, recursive = T)
#   file.copy(path2orig, path2testData, recursive = T)
#   ae = load_emuDB(path2db, inMemoryCache = internalVars$testingVars$inMemoryCache, verbose = F)
#   
#   test_that("add_itemsToEmptyLevel throws correct errors", {
#     sl = query(ae, "Phonetic == n")
#     #expect_error(add_itemsToEmptyLevel(ae, levelName = "badName", sl), regexp = "Specified level does not exist", ignore.case = T)
#     #expect_error(add_itemsToEmptyLevel(ae, levelName = "Tone", sl), regexp = "Specified level is of type EVENT", ignore.case = T)
#     #expect_error(add_itemsToEmptyLevel(ae, levelName = "Phonetic", labels = sl$labels), regexp = "Specified level is of type SEGMENT", ignore.case = T)
#     #expect_error(add_itemsToEmptyLevel(ae, levelName = "Phonetic", labels = sl$labels, sampleStart = sl$sample_start, sampleEnd = sl$sample_end), regexp = "Specified level is not empty", ignore.case = T)
# 
#   })
#   
#   test_that("correct segment level is created", {
#     sl = query(ae, "Phonetic == n")
#     # add_levelDefinition(ae, name = "Phonetic_n", type = "SEGMENT", verbose = F)
#     # add_itemsToEmptyLevel(ae, levelName = "Phonetic_n", sl)
#     # resample_annots(ae, oldSampleRate = 20000, newSampleRate = 44100, verbose = TRUE)
#     
#   })
#   
#   test_that("correct updates are made to annotation files", {
#     
#   })
#   
#   
#   # clean up
#   DBI::dbDisconnect(ae$connection)
#   ae = NULL
#   unlink(path2db, recursive = T)
# })