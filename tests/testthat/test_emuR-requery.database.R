context("testing requeries")

aeSampleRate = 20000

test_emu_ae_db = NULL

test_emu_ae_db_uuid = "0fc618dc-8980-414d-8c7a-144a649ce199"
test_emu_ae_db_dir = NULL

path2demoData = file.path(tempdir(),"emuR_demoData")
path2testhatFolder = file.path(tempdir(),"emuR_testthat")

# extract internalVars from environment .emuR_pkgEnv
internalVars = get("internalVars", envir = .emuR_pkgEnv)

legacyDbEmuAeTpl <- file.path(path2demoData, "legacy_ae", "ae.tpl")
test_emu_ae_db_dir <- file.path(path2testhatFolder, 'test_emu_ae')
unlink(test_emu_ae_db_dir, recursive = T)

# copy 4 faster tests
dir.create(test_emu_ae_db_dir)
file.copy(file.path(path2demoData, paste0('ae', emuDB.suffix)), test_emu_ae_db_dir, recursive = T)

ae = load_emuDB(file.path(test_emu_ae_db_dir, 
                          paste0('ae', emuDB.suffix)), 
                inMemoryCache = internalVars$testingVars$inMemoryCache, 
                verbose=FALSE)

test_that("Requery sequential",{
  
  # Phoneme sequences n->t
  sl1 = query(ae, "[Phoneme == n -> Phoneme == t]")
  # requery two elemnts before and one after sequence
  rsl1 = requery_seq(ae, sl1, offset = -2, length = 5)
  rsl2 = requery_seq(ae, sl1, offset = -3, length = 5, offsetRef = 'END')
  
  # equivalent requery results should be equal
  expect_equal(rsl1, rsl2)
  
  expect_that(class(rsl1), is_identical_to(c('tbl_df', 'tbl', 'data.frame')))
  expect_that(nrow(sl1), equals(2))
  expect_that(nrow(rsl1), equals(2))
  expect_that('[.data.frame'(rsl1, 1, 'labels'), is_equivalent_to('l->@->n->t->l'))
  expect_that('[.data.frame'(rsl1, 1, 'start_item_id'), equals(144))
  expect_that('[.data.frame'(rsl1, 1, 'end_item_id'), equals(148))
  
  expect_that('[.data.frame'(rsl1, 2, 'labels'), is_equivalent_to('s->@->n->t->ei'))
  expect_that('[.data.frame'(rsl1,2,'start_item_id'), equals(101))
  expect_that('[.data.frame'(rsl1,2,'end_item_id'), equals(105))
  
  # Bug ID 42
  sl1 = query(ae, "[[Phonetic == k -> Phonetic =~ .*] -> Phonetic =~ .*]")
  sl1w = suppressWarnings(requery_hier(ae, sl1, level = 'Word', verbose = F)) # this will insert an NA row because sl1 has 8 rows and sl1w has 7 msajc023 k->H->s not dominated by single C
  # sl1w has sequence length 1
  sl1w2 = requery_seq(ae, sl1w[1,])
  # Bug startItemID != endItemID, and label is not a sequence !!
  expect_that('[.data.frame'(sl1w2, 1, 'start_item_id'), equals(61))
  expect_that('[.data.frame'(sl1w2, 1, 'end_item_id'), equals(61))
  
})

test_that("Requery sequential produces correct NA rows",{
  
  # first -> move one left
  sl = query(ae, "Phonetic == V")
  expect_warning(requery_seq(ae, sl, offset = -1, ignoreOutOfBounds = T))
  sl_rq = suppressWarnings(requery_seq(ae, sl, offset = -1, ignoreOutOfBounds = T))
  expect_true(is.na(sl_rq[1,1]))
  
  # last -> move one right
  sl = query(ae, "Phonetic == l", resultType = "tibble")
  expect_warning(requery_seq(ae, sl, offset = 1, ignoreOutOfBounds = T))
  sl_rq = suppressWarnings(requery_seq(ae, sl, offset = 1, ignoreOutOfBounds = T))
  expect_true(is.na(sl_rq[1,1]))
  
  # last -> move one right + end as ref
  expect_warning(requery_seq(ae, sl, offset = 1, ignoreOutOfBounds = T, offsetRef = "END"))
  sl_rq = suppressWarnings(requery_seq(ae, sl, offset = 1, ignoreOutOfBounds = T, offsetRef = "END"))
  expect_true(is.na(sl_rq[1,1]))
  
  # last -> move one left + length way too long
  sl_rq = suppressWarnings(requery_seq(ae, sl, offset = -1, length = 15, ignoreOutOfBounds = T))
  expect_true(is.na(sl_rq[1,1]))
})

test_that("Requery hierarchical",{
  
  # Text beginning with 'a'
  sl1 = query(ae, "Text =~ 'a[mn].*'")
  # requery to level Phoneme
  rsl1 = suppressWarnings(requery_hier(ae, sl1, level = 'Phoneme'))
  expect_that(class(rsl1), is_identical_to(c('tbl_df', 'tbl', 'data.frame')))
  expect_that(nrow(sl1),equals(3))
  expect_that(nrow(rsl1),equals(3))
  expect_that('[.data.frame'(rsl1, 1, 'labels'), is_equivalent_to('V->m->V->N->s->t'))
  expect_that('[.data.frame'(rsl1, 1, 'start_item_id'), equals(114))
  expect_that('[.data.frame'(rsl1, 1, 'end_item_id'), equals(119))
  
  expect_that('[.data.frame'(rsl1, 2, 'labels'), is_equivalent_to('E->n->i:'))
  expect_that('[.data.frame'(rsl1, 2, 'start_item_id'), equals(135))
  expect_that('[.data.frame'(rsl1, 2, 'end_item_id'), equals(137))
  
  expect_that('[.data.frame'(rsl1, 3, 'labels'), is_equivalent_to('@->n'))
  expect_that('[.data.frame'(rsl1, 3, 'start_item_id'), equals(102))
  expect_that('[.data.frame'(rsl1, 3, 'end_item_id'), equals(103))
  
})

test_that("Requery hierarchical with collapse works",{
  
  # Text beginning with 'a'
  sl1 = query(ae, "Text =~ 'a[mn].*'")
  # requery to level Phoneme
  rsl1 = suppressWarnings(requery_hier(ae, sl1, level = 'Phonetic', collapse = F, verbose = F))
  expect_equal(nrow(rsl1), 12) # should have 12 elements
  allLabels = paste0(rsl1$labels, collapse = "->")
  expect_equal(allLabels, "V->m->V->N->s->t->H->E->n->i:->@->n")
})

test_that("hierarchical requery on same attrDef without times calculates missing times",{
  
  slTimes = query(ae, "Word=~.*", calcTimes = T)
  slNoTime = query(ae, "Word=~.*", calcTimes = F)
  
  # requery to same attrDef
  slRq = requery_hier(ae, slNoTime, level='Word')
  
  # overwrite attr
  attr(slTimes, "query") = ""
  attr(slRq, "query") = ""
  
  cres = compare::compare(slTimes, slRq, allowAll = T)
  expect_true(cres$result)
})

test_that("hierarchical requery on parallel attrDef works",{
  
  # Text beginning with 'a'
  sl1 = query(ae, "Text =~ 'a[mn].*'")
  
  # requery to same attrDef
  slRq = requery_hier(ae, sl1, level = 'Word')
  
  expect_equal(paste0(sl1$labels, collapse = "; "), "amongst; any; and")
  
  expect_equal(sl1$start, slRq$start)
  expect_equal(sl1$end, slRq$end)
  expect_equal(sl1$sample_start, slRq$sample_start)
  expect_equal(sl1$sample_end, slRq$sample_end)
  expect_equal(sl1$start_item_id, slRq$start_item_id)
  expect_equal(sl1$end_item_id, slRq$end_item_id)
  expect_equal(sl1$start_item_seq_idx, slRq$end_item_seq_idx)
  expect_equal(sl1$end_item_seq_idx, slRq$end_item_seq_idx)
  
})

test_that("hierarchical requery on non main attributes work",{
  
  # Text beginning with 'a'
  sl1 = query(ae, "Phonetic == n")
  
  # requery to Word:Text
  slRq = requery_hier(ae, sl1, level = 'Text')
  
  expect_equal(paste0(slRq$labels, collapse = "; "), 
               "friends; considered; any; resistance; wind; violently; concealing; weaknesses; and; no; new; than")
  
})


test_that("hierarchical throws warning if badly ordered/multiple levels",{
  # warning from various levels
  sl1 = query(ae, "Phonetic == n")
  sl2 = query(ae, "Syllable == S")
  
  sl = rbind(sl1, sl2)
  
  expect_warning(check_emuRsegsForRequery(sl))
  
  
  sl1 = query(ae, "Phonetic == n", resultType = "emuRsegs")
  sl2 = query(ae, "Phonetic == @", resultType = "emuRsegs")
  
  sl = rbind(sl1, sl2)
  
  expect_warning(check_emuRsegsForRequery(sl))
  
  sl = sort(sl)
  check_emuRsegsForRequery(sl)

  # check with new default tibble result type as well
  sl1 = query(ae, "Phonetic == n")
  sl2 = query(ae, "Phonetic == @")
  
  sl = rbind(sl1, sl2)
  
  expect_warning(check_emuRsegsForRequery(sl))
  
  sl = dplyr::arrange(sl, db_uuid, session, bundle, start_item_seq_idx)
  check_emuRsegsForRequery(sl)
  
    
})

test_that("requery_hier inserts NAs",{
  
  # delete link to check if NA is inserted
  DBI::dbExecute(ae$connection, "DELETE FROM links WHERE bundle = 'msajc003' AND from_id = 115 AND to_id = 148")
  DBI::dbExecute(ae$connection, "DELETE FROM links WHERE bundle = 'msajc012' AND from_id = 134 AND to_id = 169")
  DBI::dbExecute(ae$connection, "DELETE FROM links WHERE bundle = 'msajc023' AND from_id = 96 AND to_id = 120")
  rewrite_annots(ae, verbose = F)
  
  ########################
  # parent requery
  sl = query(ae, 
             "Phonetic == m", 
             resultType = "tibble")
  
  sl_req = suppressWarnings(requery_hier(ae, 
                                         sl, 
                                         level = "Phoneme", 
                                         resultType = "tibble"))
  
  expect_equal(nrow(sl), nrow(sl_req))
  expect_true(all(is.na(sl_req[1,])))
  expect_true(all(is.na(sl_req[2,])))
  expect_true(all(is.na(sl_req[5,])))
  # calcTimes = F
  sl_req = suppressWarnings(requery_hier(ae, 
                                         sl, 
                                         level = "Phoneme", 
                                         calcTimes = F,
                                         resultType = "tibble"))
  
  expect_equal(nrow(sl), nrow(sl_req))
  expect_true(all(is.na(sl_req[1,])))
  expect_true(all(is.na(sl_req[2,])))
  expect_true(all(is.na(sl_req[5,])))
  
  
  sl = query(ae, "Phonetic == db", resultType = "tibble")
  sl_req = requery_hier(ae, 
                        sl, 
                        level = "Phoneme", 
                        resultType = "tibble")
  
  expect_equal(sl_req$labels[1], "d->b") # check that collapsing of multiple parents works
  
  sl_req = suppressWarnings(requery_hier(ae, 
                                         sl, 
                                         level = "Phoneme", 
                                         collapse = F,
                                         resultType = "tibble"))
  
  expect_equal(sl_req$labels[1], "d")
  expect_equal(sl_req$labels[2], "b")
  
  
  ########################
  # child requery
  sl = query(ae, 
             "Phoneme == m", 
             resultType = "tibble", 
             calcTimes = F)
  
  sl_req = suppressWarnings(requery_hier(ae, 
                                         sl, 
                                         level = "Phonetic", 
                                         resultType = "tibble"))
  
  expect_equal(nrow(sl), nrow(sl_req))
  expect_true(all(is.na(sl_req[1,])))
  expect_true(all(is.na(sl_req[2,])))
  
  # calcTimes = F
  sl_req = suppressWarnings(requery_hier(ae, 
                                         sl, 
                                         level = "Phonetic", 
                                         calcTimes = F, 
                                         resultType = "tibble"))
  
  expect_equal(sl_req$labels[6], 'Om->m') # callapsing works
  
  
  # over multiple levels (parent requery)
  sl = query(ae, 
             "Phonetic == m", 
             resultType = "tibble")
  
  sl_req = suppressWarnings(requery_hier(ae, 
                                         sl, 
                                         level = "Text", 
                                         resultType = "tibble"))
  
  expect_equal(nrow(sl), nrow(sl_req))
  expect_true(all(is.na(sl_req[1,])))
  expect_true(all(is.na(sl_req[2,])))
  expect_true(all(is.na(sl_req[5,])))
  
  
  sl = query(ae, 
             "[[Phonetic == D -> Phonetic == @] -> Phonetic == m]", 
             resultType = "tibble", 
             calcTimes = F)
  
  # if only NAs in resulting seglist an empty object is returned
  sl_req = suppressWarnings(requery_hier(ae, 
                                         sl, 
                                         level = "Word", 
                                         resultType = "tibble"))
  
  expect_equal(nrow(sl_req), 0)
  
  # over multiple levels (child requery)
  sl = query(ae, 
             "Text == them", 
             resultType = "tibble", 
             calcTimes = F)
  
  sl_req = requery_hier(ae, 
                        sl, 
                        level = "Phonetic", 
                        resultType = "tibble")
  
  # only dominates D->@ not D->@->m as link to m is missing
  expect_equal(sl_req$labels[1], 'D->@')
  
})

# clean up (also disconnects)
DBI::dbDisconnect(ae$connection)
ae = NULL
unlink(test_emu_ae_db_dir, recursive = T)
