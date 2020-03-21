# emuR 2.0.4.9004

## new features / performance tweaks / improvements

- implemented `write_bundleList()` and `read_bundleList()` functions
- new `bundleListName` `serve()` parameter implemented
- added warning to `serve()` when either bundleComments or bundleFinishedEditing is set to true and no bundleListName was set (closes \#268)
- implemented `onTheFlyFunction` parameter for `get_trackdata()` which allows users to implement their own functions (input: path to wav file; output tibble/data.frame that has a column called `"frame_time"`)
- converting factors into characters in `get_trackdata()` and `normalize_length()` (closes \#224 and \#223) 
- `add_perspective()` now sets `"restrictions"` -> `"showPerspectivesSidebar"` to `true` to make the side bar visible

## bug fixes


# emuR 2.0.4

## new features / performance tweaks / improvements

- `export_seglistToTxtCollection()` now zero pads file names (should fix #219)
- implemented `write_bundleList()` and `read_bundleList()` functions
- new `bundleListName` `serve()` parameter implemented

## bug fixes

- using `rstudioapi::translateLocalUrl()` for ws connection url to permit RStudio to connect
- fixed bug in duplicate levels (bad level name insert into items table) that was causing the items array in the JSON file to stay empty
- `serve()` working outside of RStudio again (overlooked that `rstudioapi::translateLocalUrl()` needs RStudio)
- improved error formatting in `load_emuDB()` and `export_TextGridCollection`
- fixed `rename_bundles()` issue with sub-string matching of names (fixes \#220)
- `requery_hier()` works with non-main attributes again (always returned levels main attribute labels) 

# emuR 2.0.2

## new features / performance tweaks / improvements

* changed default `resultTypes` to `"tibble"` (`query()`, `requery_hier()`, `requery_seq()`, `get_trackdata()`)
* changed url in printed output of serve to https
* `serve()` doesn't block the R console any more
* suppressing requery differing length warnings in `runBASwebservice_*` functions
* `requery_hier()` now persists the input segment list length by inserting NA rows for missing segments
* depricated vignettes are now stubs only (== empty containing notice pointing to the EMU-SDMS manual)
* implemented `rename_bundles()` function
* better `times_norm` calculation for `normalize_length()`
* better error message when empty or non existing session is passed into `add_files()`
* `serve()` `useViewer` parameter now implemented and the default.
* `serve()` now uses a single server to host the EMU-webApp and provide the websocket server for the emuDB
* `serve()` now works within RStudio when it is run as a web application

## bug fixes

# emuR 1.1.2

## new features / performance tweaks / improvements

* rewrite of `list_files()` that leads to massive speed bump
* implemented `convert_wideToLong()` function to convert trackdata tibble objects to their long form representation (useful for spectral analysis)
* `query()` sub-function now sets `perl = TRUE` arguments in `regexp` call when using `sessionPattern` and `bundlePattern` (useful for e.g. for negative look-around `bundlePattern = "^((?!msajc003).)*$"`)
* implemented `export_seglistToTxtCollection()` 
* normalize length now also using `list()` with `do.call(rbind, res_list)`
* implemted new `runMINNI` boolean parameter in `runBASwebservice_all()`

## bug fixes

* https default on `serve()`
* fixed problems of `calcFreqs = T` in `convert_wideToLong()` (possibly caused by newer version of dplyr?)


# emuR 1.1.1

## new features / performance tweaks / improvements

## bug fixes

* fixed bad column init. in `normalize_length()` column
* removed detritus LaTex files (e.g. .log, .aux) from `vignette/pics` directory (as requested by CRAN maintainer)

# emuR 1.1.0

## new features / performance tweaks / improvements

* URL encoding of bundle and session names to allow for URL string reserved characters in bundle and session names
* error message of `get_trackdata()` now contains seglist row index if "Can not extract following"
* `normalize_length()` now allows for additional non-numeric columns
* changed `stop()` to `warning()` in `get_trackdata()` when samplerates are inconsistent (closes \#190)
* better error message when there is a naming mismatch of `_emuDB` dir `_DBconfig.json`
* using `sub()` instead of `tools::file_path_sans_ext()` to handle `_` in file extensions
* `query()`, `requery_seq()` and `requery_hier()` now supports the `resultType` `"tibble"` (and `get_trackdata()`, `requery_seq()`, `requery_hier()` and `serve()` support them as input)
* `serve()` function now uses `seglist$start` and  `seglist$end` instead of `seglist$sample_start` and `seglist$sample_end`
* implemented first version of `update_itemsInLevel()` (only label updates for now)
* now setting the `sample_start` and `sample_end` values in query results when EVENT levels are queried (previously only `start` was set)
* implemented first versions of `create_links()` (currently not checking for anything), `create_itemsInLevel()` (only EVENTs and ITEMs) and `update_itemsInLevel()` (only labels) and `delete_itemsInLevel()`
* `list_bundles()` outside of loop for performance bump in `get_trackdata()`
* better error message in `add_files()` if no files are found
* `consistentOutputType` of `get_trackdata()` is not set to `TRUE` and is reset to `T` if `resultType` is `"emuRtrackdata"` or `"tibble"` (fixes \#203)
* avoiding negative `times_rel` and `times_norm` values in `create_emuRtrackdata()` by setting them to 0 (caused by string to numeric conversion precision errors)
* `requery_seq()` now inserts NA values for the out of bounds rows instead of dropping them.
* implemented `list_sampleRates()` function
* `get_trackdata()` is now iteratively appending to a `list()` instead of into a SQLite temp table. This is a fairly large performance boost and also fixes \#206.
* added deprecation warnings to vignettes (added links to manual chapters)

## bug fixes

* propper fix for "now ordering by `items_idx` not by `start_start_seq_idx` which led to bad label sequences (fixes \#140)"
* fixed bad indexing in `normalize_length()` when sl_rowIdx values are not a `c(1, 2, 3, 4, ...)` sequence
* fixed `staticContours` SSFF tracks not being sent to EMU-webApp (fixes \#195)
* fixed bug with completely empty levels that caused a bad resort of levels in `_annot.json`s in `rewrite_allAnnots()`
* fixed bug in `add_files()` that was using the wrong variable (fixes \#196)
* added error message when querying levels without time-bearing sub-levels (closes \#150)
* fixed bug in `create_emuRtrackdata()` with handling trackdata object of class `spectral`
* correct recalculation of ITEM IDs of missing levels in .hlb files (== only present in ESPS files) in `convert_legacyEmuDB()`
* fixed handling of completely empty levels in .hlb files
* `normalize_length()` now handles various additional column types (not just `"numeric"`)

# emuR 1.0.0

## new features / performance tweaks / improvements

* implemented new `consistentOutputType` parameter for `get_trackdata()` to always return a `trackdata` or `emuRtrackdata` object independent of what the `cut` and `npoints` arguments are set to
* now removing `levelCanvasOrder` entry in `remove_levelDefinition()` (fixes \#156)
* `serve()` method now uses GET to deliver media files to the EMU-webApp. This avoids the base64 conversion overhead and is a quite significant load time improvement
* explicit error message in `convert_legacyEmuDB()` when invalid redundant links are found
* better error message in BPF parser
* `convert_legacyEmuDB()` automatically converts `.ssd` media files to `.wav` and normalizes the annotations to start at 0 (only if attr(ssd,'startTime') is not 0).
* added `sort()` S3 method for `emuRsegs` objects
* checking for badly sorted `emuRsegs` in `requery_hier()` and `requery_seq()` functions
* `create_emuRtrackdata()` returns a simple `data.frame` object not a `data.table` object
* `emuRtrackdata` object now contains a `times_norm` (normalized time values between 0 and 1 for each segment) column by default
* added note to `print.emuRsegs()` to give the user a hint about missing columns
* implemented `print.emuRtrackdata()` to avoid overly verbose output
* implemented `normalize_length()` function as S3 function to normalize the lenght of each segment in an `emuRtrackdata` object
* added `absolute_file_path` column to output of `list_files()`
* query engine does not rely on label index in label array any more (updated `convert_queryResultToEmuRsegs()` to use `resultAttrDef` instead of `labelIdx`). Closes \#164.
* added `browser` argument to `serve()` function which is passed on to `utils::browseURL()` function
* `requery_seq()` now uses `start_item_seq_idx` and `end_item_seq_idx` of seglist instead of `start_item_id` and `start_item_id` to simplify function
* implemented `check_emuDBhandle()` function that is used on every exported function that takes a `emuDBhandle` as an argument to check if the handle is still valid (closes \#176)
* implemented `"tibble"` as `resultType` option in `get_trackdata()`. This will probably replace the `"emuRtrackdata"` option in future (it contains exactly the same data/columns).
* prechecking if attribute definition is already defined (closes \#182)
* `get_trackdata()` now uses temporary SQL tables to store the intermediate results (massive performance gains!). Removed `nrOfAllocationRows` parameter as this is no longer needed as no matrix is used to store the intermediate results. (also closes \#125)
* `convert_TextGridCollectio()` using `dir.exists()` instead of `file.exists()` to check dirs 
* all read operations now use the readr package (avoids encoding problems like \#187)
* `list_attributeDefinitions()` now allows for a name vector to be passed in
* rewrite of `rewrite_allAnnots()` functions for faster rewrites of `_annot.json` files to disk
* improved cleanup in testing DBconfig functions
* now ordering by `items_idx` not by `start_start_seq_idx` which led to bad label sequences (fixes \#140)

## bug fixes

* fixed problem of updating cache. Didn't handle `data.frame` object that was thought to be a vector correctly!
* fixed a bug in the BPF export function, which meant that WAVE files were only copied into one session
* added missing `$` in pattern arguments in `list.files` call in `list_files` (fixes \#170)
* not adding ssffTrackDefinition to DBconfig if user input is no (closes \#171)
* fixed bad `seq_start_seq_idx` and `seq_seq_idx` returned by `requery_seq()` (fixes \#183) 
* fixed bad `seq_start_seq_idx` returned by internal `query_databaseHier()` function
* fixed bad sorting of `requery_seq()/requery_hier()` when `calcTimes = F` (still sorted by `start_sample` instead of the correct `seq_idx`)
* added `readr::parse_character()` to data received in `serve()` as this is recoded in windows (fixes \#188).

# emuR 0.2.3

## new features / performance tweaks / improvements

* tweaked `runBASwebservice_maus()`; improved performance for presegmented bundles
* performance bump for `fapply()` by preallocating result matrix
* performance bump for `trapply()` by preallocating result matrix
* performance bump for `mel.spectral()` by preallocating result matrix
* performance bump for `bark.spectral()` by preallocating result matrix
* updated DBI calls to comply with the latest best practices (using `DBI::dbExecute()` instead of `DBI::dbGetQuery()` for non-`SELECT` queries)
* BPF collection exporter documented and now public

## bug fixes

* `export_TextGridCollection()` now handles partial includes of bundle and session names correctly (issue \#147)
* added missing check if `anagestConfig` is defined to `rename_attributeDefinition()`
* setting useBytes to T to avoid reencoding under windows
* fixed bug in `add_ssffTrackDefinition()` that was trying to access `fp` which was renamed in a refactor to `filesDf`
* fixed export to autodetect S3 methods (cbind & rbind for trackdata)

# emuR 0.2.2

## new features / performance tweaks / improvements

* some changes to the parameter names in the BAS webservice functions
* convert_txtCollection and convert_BPFCollection now name topmost item "bundle"
* added functions to set and get level descriptions in DBconfig
* BAS webservice functions now perform a cache update prior to departure
* added multiple perspectives to ae demo database
* choosing explicit paths with intersecting hierarchies now possible
* remove levelDef & linkDef now implement force parameters
* new function convert_txtCollection converts plain text collections into single-node emuDB
* new functions runBASwebservice_* that call various BAS webservices from inside emuR
* NULLing out empty DFs on list_level/linkDefs for more consistent API
* `newLinkDefType` argument implemented in `autobuild_linkFromTimes()` to generate linkDefinition if so desired 
* automatically removing superlevel from `levelCanvasOrder` if `convertSuperlevel` is set to `TRUE` in `autobuild_linkFromTimes()`

## bug fixes

* wrapped `readChar`s in `enc2utf8` to avoid encodings issues on windows
* updating label table correctly on add_attributeDefinition() (#138)
* runBASwebservice_maus / minni / all now no longer ignore unlinked items (idx -1) but treat them as linkless segments
* commented out `cat()` in `train()` function be be less verbose
* BAS webservice calls now get their own temp directories (UUID based). This avoids race conditions when several scripts are running in parallel.
* convert_txtCollection now treats perspectives as array (as it should)

# emuR 0.2.1

## new features / performance tweaks / improvements

* added new `EMUwebAppConfig -> perspectives -> signalCanvases -> minMaxValLims` config option to emuDB vignette
* requery_hier + requery_seq now implement the same timeRefSegmentLevel parameter as query (#135)

## bug fixes

* fixed requery\_hier() bug of requery on same attribute definition
* fixed requery\_hier() bug of requery on same level but different attribute definition

# emuR 0.2.0

## new features / performance tweaks / improvements

* rewrite of query engine to not require links_ext table any more (== redundant links)
* calcTimes parameter added to query() / requery\_seq() / requery\_hier() to make calculating times optional (extreme performance boost if no times have to be calculated)
* rewrite of annotJSONcharToBundleAnnotDFs() for faster loads emuDBs containing large annotJSONs
* replaced tidyjson as annot.json parser with own solution at tidyjson didn't scale well on larger annotation files
* added verbose parameter to export\_TextGridCollection()
* improved pre-check of dir exists in export\_TextGridCollection()
* added new replace\_itemLabels function
* improved export\_TextGridCollection() doc
* improved replace_itemLables() speed
* implemented rename\_emuDB() (\#116)
* implemented duplicate\_level() (\#113)
* implemented linkDuplicates parameter in duplicate\_level()
* autobuild\_linkFromTimes() speed improvements
* FUNCQ queries (start(),end(), medial()) now additionally support TRUE & FALSE and T & F values (vs. 0 & 1) 
* added attrDefNames column to list\_levelDefinitions() output
* can now deal with read only emuDBs by copying the cache to tempdir() and making it writable for the user
* added start\_item\_seq\_idx and end\_item\_seq\_idx to emuRsegs object
* added start\_item\_seq\_idx and end\_item\_seq\_idx type values to all intermediate result tables
* added optional function to reduce hierarchical query results to left and right most children only (large performance gain on calcTimes = T)
* rewriting annot.json files now updates MD5 sums as well (avoids unnecessary reload on next load\_emuDB)
* rewriting annot.json files now writes all (including empty / missing) attributeDef. labels

## bug fixes

* fixed bad DBconfig gen. on add_perspective
* fixed list\_linkDefinitions() returning strings as factors
* fixed bad error message when passing in ITEM levels to autobuild\_linkFromTimes()
* fixed incorrect handling of DBconfig when writeToFS was set to FALSE (writeToFS is now called rewriteAllAnnots)

# emuR 0.1.9

## new features / performance tweaks / improvements

* also allowing "time = " in TextTiers
* "levels of type 'EVENT' are not allowed to be super levels (== parents) in a domination relationship" constraint enforced in add_linkDefinition
* added "MEDIAFILE\_SAMPLES" as constant name to access audio samples to get\_trackdata() function
* improved error message to include tgPath in create_DBconfigFromTextGrid function
* no integer return value returned by create_emuRdemoData() any more! It was implicitly returned from wrassp function call...
* improved the slow overlap checking function in the BPF parser (is now O(n) instead of O(n^2))
* fixed col naming problems for new (unreleased) RSQLite version
* added export_TextGridCollection() function
* improved doc for get_trackdata
* constant naming of EMU-SDMS vs EMU_SDMS in various files
* rewriting all annotation file on add\_levelDefinition, remove\_levelDefinition 

## bug fixes

* fixed problem in conversion to JSON with empty items array (object '{}' vs array '[]')
* fixed problem of keywords "number" | "time" | "xmin" | ... in labels causing TextGrid parser to fail
* fixed problem with to lax RegEx in TextGrid parser
* fixed validation problem with missing levels regarding types

# emuR 0.1.8

## new features / performance tweaks / improvements

* get_trackdata with onTheFly calculation now reuses AsspDataObj if the current utterance is the same as the previous (large performance gain especially on long audio files)
* checking if DBconfig exists for better error message if 'name' field is not set correctly in DBconfig
* setting PRAGMA temp_store = 2; for SQLite connections
* not extracting tables to R if no RegEx needed to create filtered_tmp tables (performance gain when querying large emuDBs)
* convert_BPFCollection can now assigns the same label to more than one item when unifying tiers
* newline at the end of load_emuDB if no redundant links are built
* queries using dominates operator '^' don't use linksExt table anymore -> large performances benefits 
* only using \_filtered\_tmp tables if RegEx patterns are used
* changed primary key on items table which leads to massive performance gains (deleting _emuDBcache.sqlite required)

## bug fixes

* fixed error handling of create_emuRtrackdata + added @export to roxygen doc
* invalid annotJSONs generated by import_mediaFiles fixed
* convert_TextGridCollection can now handle nested folders again
* invalid UUIDs in DBConfig produced by convert_BPFCollection. Also added additional unit test to detect this.
* list_bundles uses session argument again
* fixed "Expression tree is too large (maximum depth 1000)" error in get_trackdata with long emuRsegs lists

# emuR 0.1.7

* R depends version bump to 3.2.0 (as requested by CRAN maintainer)
* updated testthat::expect\_less\_than to expect\_lt calls (due to deprecated warnings)
* Using new .keep_all = T parameter of dplyr
* removed legacy version of EQL vignette (overlooked as inst/doc was in .gitignore)

# emuR 0.1.6

* skipping in-depth thorough tests on CRAN for query and autobuild SQL functions 

# emuR 0.1.5

* fixed problem of interm\_res\_tables already being present with queries that have multiple recursion depth on both sides
of either -> or ^ operand (e.g. query (ae ,  "[[[Phonetic = n -> Phonetic =z] -> Phonetic = S ] ^ [Text = friends -> Text = she]]")) 
* fixed bad URL in README.md
* added CITATION file

# emuR 0.1.3.9000

* renamed SQL tables & columns from camel case to underscore notation 
* variable SQL backend implementation

# emuR 0.1.2.9000

* multiple check fixes on various platforms

# emuR 0.1.1.9000

* `serve` problem with internalVars bug fixed
* file locking problem that caused vignettes to fail under windows problem fixed

# emuR 0.1.0.9000

* massive refactor of all functions that used to refer to an emuDB by 
  name and optionally by its UUID. They now use the new emuDBhandle object
  that is now returned by the `load_emuDB()` function.
* `convert_XXX_to_emuDB()` functions renamed to `convert_XXX()`
