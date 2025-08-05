###########################################################################
# create table / index definitions for DBI that are used at query time

# tabels that store "filtered" items and labels (when session/bundlePatterns are used)
database.DDL.emuDB_itemsFilteredTmp = gsub("CREATE TABLE items", 
                                           "CREATE TEMP TABLE items_filtered_tmp", 
                                           database.DDL.emuDB_items)
database.DDL.emuDB_itemsFilteredTmp = gsub(",...FOREIGN.*CASCADE", 
                                           "", 
                                           database.DDL.emuDB_itemsFilteredTmp) # remove FOREIGN KEY

database.DDL.emuDB_labelsFilteredTmp = gsub("CREATE TABLE labels", 
                                            "CREATE TEMP TABLE labels_filtered_tmp", 
                                            database.DDL.emuDB_labels)
database.DDL.emuDB_labelsFilteredTmp = gsub(",...FOREIGN.*CASCADE", 
                                            "", 
                                            database.DDL.emuDB_labelsFilteredTmp) # remove FOREIGN KEY

database.DDL.emuDB_linksFilteredTmp = gsub("CREATE TABLE links", 
                                           "CREATE TEMP TABLE links_filtered_tmp", 
                                           database.DDL.emuDB_links)
database.DDL.emuDB_linksFilteredTmp = gsub(",...FOREIGN.*CASCADE", 
                                           "", 
                                           database.DDL.emuDB_linksFilteredTmp) # remove FOREIGN KEY

#####################################
create_tmpFilteredQueryTablesDBI <- function(emuDBhandle){
  
  DBI::dbExecute(emuDBhandle$connection, database.DDL.emuDB_itemsFilteredTmp)
  DBI::dbExecute(emuDBhandle$connection, database.DDL.emuDB_labelsFilteredTmp)
  DBI::dbExecute(emuDBhandle$connection, database.DDL.emuDB_linksFilteredTmp)
  
  # tabels that store subsets of filtered tables
  database.DDL.emuDB_itemsFilteredSubsetTmp = gsub("CREATE TABLE items", 
                                                   "CREATE TEMP TABLE items_filtered_subset_tmp", 
                                                   database.DDL.emuDB_items)
  database.DDL.emuDB_itemsFilteredSubsetTmp = gsub(",...FOREIGN.*CASCADE", 
                                                   "", 
                                                   database.DDL.emuDB_itemsFilteredSubsetTmp) # remove FOREIGN KEY
  
  database.DDL.emuDB_labelsFilteredSubsetTmp = gsub("CREATE TABLE labels", 
                                                    "CREATE TEMP TABLE labels_filtered_subset_tmp", 
                                                    database.DDL.emuDB_labels)
  database.DDL.emuDB_labelsFilteredSubsetTmp = gsub(",...FOREIGN.*CASCADE", 
                                                    "", 
                                                    database.DDL.emuDB_labelsFilteredSubsetTmp) # remove FOREIGN KEY
  
  DBI::dbExecute(emuDBhandle$connection, 
                 database.DDL.emuDB_itemsFilteredSubsetTmp)
  DBI::dbExecute(emuDBhandle$connection, 
                 database.DDL.emuDB_labelsFilteredSubsetTmp)
  
  database.DDL.emuDB_lrExpResTmp = paste0("CREATE TEMP TABLE lr_exp_res_tmp (",
                                          " db_uuid VARCHAR(36),",
                                          " session TEXT,",
                                          " bundle TEXT,",
                                          " l_seq_start_id INTEGER,",
                                          " l_seq_end_id INTEGER,",
                                          " l_seq_len INTEGER,",
                                          " l_level TEXT,",
                                          " l_attribute TEXT,",
                                          " l_seq_start_seq_idx INTEGER,",
                                          " l_seq_end_seq_idx INTEGER,",
                                          " r_seq_start_id INTEGER,",
                                          " r_seq_end_id INTEGER,",
                                          " r_seq_len INTEGER,",
                                          " r_level TEXT,",
                                          " r_attribute TEXT,",
                                          " r_seq_start_seq_idx INTEGER,",
                                          " r_seq_end_seq_idx INTEGER",
                                          ");")
  
  
  DBI::dbExecute(emuDBhandle$connection, 
                 database.DDL.emuDB_lrExpResTmp)
  
}

#####################################
create_intermResTmpQueryTablesDBI <- function(emuDBhandle, 
                                              suffix = "root"){
  
  database.DDL.emuDB_intermRes_itemsTmp = paste0("CREATE TEMP TABLE interm_res_items_tmp_", suffix, " (",
                                                 " db_uuid VARCHAR(36),",
                                                 " session TEXT,",
                                                 " bundle TEXT,",
                                                 " seq_start_id INTEGER,",
                                                 " seq_end_id INTEGER,",
                                                 " seq_len INTEGER,",
                                                 " level TEXT,",
                                                 " attribute TEXT,",
                                                 " seq_start_seq_idx INTEGER,",
                                                 " seq_end_seq_idx INTEGER",
                                                 #"PRIMARY KEY (db_uuid, session, bundle, seq_start_id, seq_end_id)",
                                                 ");")
  
  database.DDL.emuDB_intermRes_itemsTmp_idx1 = paste0("CREATE INDEX interm_res_items_tmp_", suffix, "_idx1 ",
                                                      "ON interm_res_items_tmp_", suffix, "(",
                                                      " db_uuid, ",
                                                      " session, ",
                                                      " bundle, ",
                                                      " seq_start_id, ",
                                                      " seq_end_id",
                                                      ")")
  database.DDL.emuDB_intermRes_itemsTmp_idx2 = paste0("CREATE INDEX interm_res_items_tmp_", suffix, "_idx2 ",
                                                      "ON interm_res_items_tmp_", suffix, "(",
                                                      " db_uuid, ",
                                                      " session, ",
                                                      " bundle, ",
                                                      " seq_end_id",
                                                      ")")
  database.DDL.emuDB_intermRes_itemsTmp_idx3 = paste0("CREATE INDEX interm_res_items_tmp_", suffix, "_idx3 ",
                                                      "ON interm_res_items_tmp_", suffix, "(",
                                                      " db_uuid, ",
                                                      " session, ",
                                                      " bundle, ",
                                                      " level, ",
                                                      " seq_start_seq_idx, ",
                                                      " seq_end_seq_idx",
                                                      ")")
  
  database.DDL.emuDB_intermRes_itemsTmp_idx4 = paste0("CREATE INDEX interm_res_items_tmp_", suffix, "_idx4 ",
                                                      "ON interm_res_items_tmp_", suffix, "(",
                                                      " db_uuid, ",
                                                      " session, ",
                                                      " bundle, ",
                                                      " level, ",
                                                      " seq_end_seq_idx",
                                                      ")")
  
  database.DDL.emuDB_intermRes_projItemsTmp = paste0("CREATE TEMP TABLE interm_res_proj_items_tmp_", suffix, " (",
                                                     " db_uuid VARCHAR(36),",
                                                     " session TEXT,",
                                                     " bundle TEXT,",
                                                     " seq_start_id INTEGER,",
                                                     " seq_end_id INTEGER,",
                                                     " p_seq_start_id INTEGER,",
                                                     " p_seq_end_id INTEGER,",
                                                     " p_seq_len INTEGER,",
                                                     " p_level TEXT,",
                                                     " p_attribute TEXT,",
                                                     " p_seq_start_seq_idx INTEGER,",
                                                     " p_seq_end_seq_idx INTEGER",
                                                     ");")
  
  if(!DBI::dbExistsTable(emuDBhandle$connection, paste0("interm_res_items_tmp_", suffix))){
    DBI::dbExecute(emuDBhandle$connection, 
                   database.DDL.emuDB_intermRes_itemsTmp)
    DBI::dbExecute(emuDBhandle$connection, 
                   database.DDL.emuDB_intermRes_itemsTmp_idx1)
    DBI::dbExecute(emuDBhandle$connection, 
                   database.DDL.emuDB_intermRes_itemsTmp_idx2)
    DBI::dbExecute(emuDBhandle$connection, 
                   database.DDL.emuDB_intermRes_itemsTmp_idx3)
    DBI::dbExecute(emuDBhandle$connection, 
                   database.DDL.emuDB_intermRes_itemsTmp_idx4)
  }else{
    DBI::dbExecute(emuDBhandle$connection, 
                   paste0("DELETE FROM interm_res_items_tmp_", suffix))
  }
  if(!DBI::dbExistsTable(emuDBhandle$connection, 
                         paste0("interm_res_proj_items_tmp_", suffix))){
    DBI::dbExecute(emuDBhandle$connection, 
                   database.DDL.emuDB_intermRes_projItemsTmp)
  }else{
    DBI::dbExecute(emuDBhandle$connection, 
                   paste0("DELETE FROM interm_res_proj_items_tmp_", suffix))
  }
}

#####################################
drop_tmpFilteredQueryTablesDBI <- function(emuDBhandle){
  
  tableNames = DBI::dbListTables(emuDBhandle$connection)
  if("items_filtered_tmp" %in% tableNames) DBI::dbExecute(emuDBhandle$connection, 
                                                          "DROP TABLE items_filtered_tmp")
  if("labels_filtered_tmp" %in% tableNames) DBI::dbExecute(emuDBhandle$connection, 
                                                           "DROP TABLE labels_filtered_tmp")
  if("links_filtered_tmp" %in% tableNames) DBI::dbExecute(emuDBhandle$connection, 
                                                          "DROP TABLE links_filtered_tmp")
  
  if("items_filtered_subset_tmp" %in% tableNames) DBI::dbExecute(emuDBhandle$connection, 
                                                                 "DROP TABLE items_filtered_subset_tmp")
  if("labels_filtered_subset_tmp" %in% tableNames) DBI::dbExecute(emuDBhandle$connection, 
                                                                  "DROP TABLE labels_filtered_subset_tmp")
}

#####################################
drop_allTmpTablesDBI <- function(emuDBhandle){
  
  allTables = DBI::dbListTables(emuDBhandle$connection)
  allTmpTables = allTables[grepl(".*tmp.*", allTables)]
  for(tmpTable in allTmpTables){
    DBI::dbExecute(emuDBhandle$connection, paste0("DROP TABLE ", tmpTable))
  }
}

#####################################
## @param emuDBhandle
## @param intermResTableSuffix
clear_intermResTabels <- function(emuDBhandle, 
                                  intermResTableSuffix, 
                                  clearProjectionItems = TRUE){
  DBI::dbExecute(emuDBhandle$connection, 
                 paste0("DELETE FROM interm_res_items_tmp_", intermResTableSuffix))
  
  if(clearProjectionItems) { 
    DBI::dbExecute(emuDBhandle$connection, 
                   paste0("DELETE FROM interm_res_proj_items_tmp_", intermResTableSuffix))
  }
}
###################################################################
################## Functions implementing EQL #####################
###################################################################

#################################
# helper functions for query

create_conditionTextAlternatives <- function(opr, 
                                             values){
  o = list(opr = opr, values = values)
  return(o)
}

emuR_regexprl<-function(pattern, 
                        x){
  m = regexpr(pattern, x, perl = TRUE)
  return((m == 1) & (attr(m, 'match.length') == nchar(x)))
}

check_levelAttributeName <- function(emuDBhandle, 
                                     name){
  aNms = get_allAttributeNames(emuDBhandle)
  if(! (name %in% aNms)){
    stop("Unknown level attribute name: '",
         name,
         "'. Database attribute names are: ",
         paste(aNms,collapse=','),
         "\n")
  }
}

#################################
# actual query functions

#################################
query_labels <- function(emuDBhandle, 
                         attributeName, 
                         intermResTableSuffix, 
                         conditionText, 
                         sessionPattern, 
                         bundlePattern, 
                         useSubsets){
  
  if(useSubsets){
    labelTableName = "labels_filtered_subset_tmp"
  }else{
    labelTableName = "labels"
  }
  
  # clear tables but keep projectionItems so they don't
  # get lost in queries like : [Text == the -> #Text =~ .* & Accent == S]  (right side of ->)
  clear_intermResTabels(emuDBhandle, 
                        intermResTableSuffix, 
                        clearProjectionItems = FALSE)
  
  opr = conditionText[['opr']]
  values = conditionText[['values']]
  res = NULL
  levelName = get_levelNameForAttributeName(emuDBhandle, 
                                            attributeName)
  
  if(opr == '==' | opr == '='){
    for(value in values){
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", intermResTableSuffix, " ", 
                                                    "SELECT ",
                                                    " it.db_uuid, ",
                                                    " it.session, ",
                                                    " it.bundle, ",
                                                    " it.item_id AS seq_start_id, ",
                                                    " it.item_id AS seq_end_id, ",
                                                    " 1 AS seq_len,",
                                                    " it.level AS level,",
                                                    " lt.name AS attribute,",
                                                    " it.seq_idx AS seq_start_seq_idx, ",
                                                    " it.seq_idx AS seq_end_seq_idx ",
                                                    "FROM items AS it, ", 
                                                    labelTableName, " AS lt ",
                                                    "WHERE it.db_uuid = lt.db_uuid ",
                                                    " AND it.session = lt.session ",
                                                    " AND it.bundle = lt.bundle ",
                                                    " AND it.item_id = lt.item_id ",
                                                    " AND it.level = '", levelName, "' ",
                                                    " AND lt.name = '", attributeName, "' ",
                                                    " AND lt.label = '", value, "' ",
                                                    " AND it.session REGEXP '", sessionPattern, "' ",
                                                    " AND it.bundle REGEXP '", bundlePattern, "' ",
                                                    ""))
    }
  }else if(opr == '!='){
    
    sqlStr = paste0("INSERT INTO interm_res_items_tmp_", intermResTableSuffix, " ", 
                    "SELECT ",
                    " it.db_uuid, ",
                    " it.session, ",
                    " it.bundle, ",
                    " it.item_id AS seq_start_id, ",
                    " it.item_id AS seq_end_id, ",
                    " 1 AS seq_len,", 
                    " it.level AS level, ",
                    " lt.name AS attribute, ",
                    " it.seq_idx AS seq_start_seq_idx, ",
                    " it.seq_idx AS seq_end_seq_idx ",
                    "FROM items AS it, ", 
                    labelTableName, " AS lt ",
                    "WHERE it.db_uuid = lt.db_uuid ",
                    " AND it.session = lt.session ",
                    " AND it.bundle = lt.bundle ",
                    " AND it.item_id = lt.item_id ",
                    " AND it.level = '", levelName, "'",
                    " AND lt.name = '", attributeName, "'",
                    " AND it.session REGEXP '", sessionPattern, "' ",
                    " AND it.bundle REGEXP '", bundlePattern, "' ",
                    ""
    )
    for(value in values){
      sqlStr = paste0(sqlStr, " AND label <> '", value, "'")
    }
    
    DBI::dbExecute(emuDBhandle$connection, sqlStr)
    
  }else if(opr == '=~'){
    for(value in values){
      if(value == ".*" || value == ".+" || stringr::str_starts(value, "\\^")){
      }else{
        warning(paste0("=~ now requires ^ if you wish to match the\n", 
                       "first character in a sequence i.e. 'a.*' now also matches\n",
                       "'weakness' as it contains the sequence. '^a.*'\n",
                       "matches sequences that start with 'a.*'\n",
                       "e.g. the word 'amongst'."))
      }
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", intermResTableSuffix, " ", 
                                                    "SELECT ",
                                                    " it.db_uuid, ",
                                                    " it.session, ",
                                                    " it.bundle, ",
                                                    " it.item_id AS seq_start_id, ",
                                                    " it.item_id AS seq_end_id, ",
                                                    " 1 AS seq_len,", 
                                                    " it.level AS level, ",
                                                    " lt.name AS attribute, ",
                                                    " it.seq_idx AS seq_start_seq_idx, ",
                                                    " it.seq_idx AS seq_end_seq_idx ", 
                                                    "FROM items AS it, ", 
                                                    labelTableName, " AS lt ",
                                                    "WHERE it.db_uuid = lt.db_uuid ",
                                                    " AND it.session = lt.session ",
                                                    " AND it.bundle = lt.bundle ",
                                                    " AND it.item_id = lt.item_id ",
                                                    " AND it.level = '", levelName, "' ",
                                                    " AND lt.name = '", attributeName, "' ",
                                                    " AND lt.label REGEXP '", value, "' ",
                                                    " AND it.session REGEXP '", sessionPattern, "' ",
                                                    " AND it.bundle REGEXP '", bundlePattern, "' ",
                                                    ""))
    }
  }else if(opr == '!~'){
    for(value in values){
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", intermResTableSuffix, " ", 
                                                    "SELECT ",
                                                    " it.db_uuid, ",
                                                    " it.session, ",
                                                    " it.bundle, ",
                                                    " it.item_id AS seq_start_id, ",
                                                    " it.item_id AS seq_end_id, ",
                                                    " 1 AS seq_len,", 
                                                    " it.level AS level, ",
                                                    " lt.name AS attribute, ",
                                                    " it.seq_idx AS seq_start_seq_idx, ",
                                                    " it.seq_idx AS seq_end_seq_idx ", 
                                                    "FROM items AS it, ", 
                                                    labelTableName, " AS lt ",
                                                    "WHERE it.db_uuid = lt.db_uuid ",
                                                    " AND it.session = lt.session ",
                                                    " AND it.bundle = lt.bundle ",
                                                    " AND it.item_id = lt.item_id ",
                                                    " AND it.level = '", levelName, "' ",
                                                    " AND lt.name = '", attributeName, "' ",
                                                    " AND lt.label NOT REGEXP '", value, "' ",
                                                    " AND it.session REGEXP '", sessionPattern, "' ",
                                                    " AND it.bundle REGEXP '", bundlePattern, "' ",
                                                    ""))
    }
  }else{
    stop("Syntax error: Unknown operator: '",
         opr,
         "'\n")
  }
}

# EBNF: FUNCQ = POSQ | NUMQ;
query_databaseEqlFUNCQ <- function(emuDBhandle, 
                                   q, 
                                   intermResTableSuffix, 
                                   sessionPattern, 
                                   bundlePattern, 
                                   useSubsets, 
                                   verbose){
  qTrim = stringr::str_trim(q)
  if(useSubsets){
    itemsTableName = "items_filtered_subset_tmp"
  }else{
    itemsTableName = "items"
  }
  
  
  # determine function name
  # TODO duplicate code
  prbOpen = get_stringPosition(string = qTrim,
                               searchStr = '(',
                               literalQuote = "'")
  if(prbOpen != -1){
    prbClose = get_stringPosition(string = qTrim,
                                  searchStr = ')',
                                  literalQuote = "'")
    if(prbClose == -1){
      stop("Syntax error: Missing closing round bracket ')' in '", q, "'\n")
    }else{
      if(prbOpen > prbClose){
        stop("Syntax error: Expected opening round bracket '(' before closing round bracket in '", q, "'\n")
      }
      if(prbOpen == 1){
        stop("Syntax error: Expected function name in '", q, "'\n")
      }
      paramsVec = stringr::str_split(substr(qTrim,
                                            prbOpen + 1,
                                            prbClose - 1),
                                     ',')
      params = paramsVec[[1]]
      paramsLen = length(params)
      # all functions require exactly two params
      if(paramsLen != 2){
        stop("Syntax error: All EQL functions require exactly two parameters in '", q, "'\n")
      }
      param1 = stringr::str_trim(params[[1]])
      param2 = stringr::str_trim(params[[2]])
      # check attribute names
      aNms = get_allAttributeNames(emuDBhandle)
      if(!(param1 %in% aNms)){
        msg = paste0("Unknown level attribute name: '", param1, "'.")
        if(length(aNms) > 0){
          msg = paste0(msg," Database attribute names are: ", paste(aNms,collapse=','))
        }
        msg = paste0(msg, "\n")
        stop(msg)
      }
      if(!(param2 %in% aNms)){
        msg = paste0("Unknown level attribute name: '", param2, "'.")
        if(length(aNms) > 0){
          msg = paste0(msg, " Database attribute names are: ", paste(aNms, collapse = ','))
        }
        msg = paste0(msg,"\n")
        stop(msg)
      }
      
      funcValueTerm = stringr::str_trim(substring(qTrim, prbClose + 1))
      
      
      funcName = stringr::str_trim(substr(qTrim, 1, prbOpen - 1))
      # EBNF: POSQ = POSFCT,'(',Level,',',Level,')','=','0'| '1';
      itemsAsSeqs = NULL
      
      level1 = get_levelNameForAttributeName(emuDBhandle, param1)
      level2 = get_levelNameForAttributeName(emuDBhandle, param2)
      
      #######################################
      # connect all children to parents
      level1ItemsTableSuffix = "funcq_level1_items"
      create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = level1ItemsTableSuffix)
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", level1ItemsTableSuffix, " ",
                                                    "SELECT ",
                                                    " db_uuid, ",
                                                    " session, ",
                                                    " bundle, ",
                                                    " item_id AS start_item_id, ",
                                                    " item_id AS seq_end_id, ",
                                                    " 1 AS seq_len, ",
                                                    " level, ",
                                                    " '", param1, "' as attribute, ", 
                                                    " seq_idx AS seq_start_seq_idx, ",
                                                    " seq_idx AS seq_end_seq_idx ",
                                                    "FROM items ",
                                                    "WHERE db_uuid ='", emuDBhandle$UUID, "' ",
                                                    " AND level = '", level1, "'",
                                                    " AND items.session REGEXP '", sessionPattern, "' ",
                                                    " AND items.bundle REGEXP '", bundlePattern, "' ",
                                                    ""))
      
      # get hierarchy paths to check which level is parent
      connectHierPaths = get_hierPathsConnectingLevels(emuDBhandle, 
                                                       level1, 
                                                       level2)
      
      if(connectHierPaths[[1]][length(connectHierPaths[[1]])] == level1){
        stop("Second level/attribute name parameter in:'", 
             qTrim, 
             "is not a child of the first level/attribute.",
             " This in not permitted in FUNCQ queries!")
      } else {
      }
      
      query_hierarchyWalk(emuDBhandle,
                          startItemsTableSuffix = level1ItemsTableSuffix, 
                          targetItemsAttributeName = level2,
                          preserveStartItemsRowLength = TRUE,
                          sessionPattern = sessionPattern,
                          bundlePattern = bundlePattern,
                          verbose = verbose) # result written to lr_exp_res_tmp table (left parents/right children)
      
      # create tmp table to store seqs in (could this be replaced because of the output of query_hierarchyWalk?)
      DBI::dbExecute(emuDBhandle$connection,paste0("CREATE TEMP TABLE IF NOT EXISTS items_as_seqs_tmp ( ",
                                                   " db_uuid VARCHAR(36), ",
                                                   " session TEXT, ",
                                                   " bundle TEXT, ",
                                                   " seq_start_id INTEGER, ",
                                                   " seq_end_id INTEGER, ", 
                                                   " seq_len INTEGER, ", 
                                                   " level TEXT, ",
                                                   " attribute TEXT ",
                                                   ")"))
      
      
      # EBNF: COP = '=' | '!=' | '>' | '<' | '<=' | '>=';
      if(funcName == 'Start' | funcName == 'End' | funcName == 'Medial'){
        
        if(funcValueTerm != ''){
          # check equals operator == or =
          expEqualSign = substr(funcValueTerm, 1, 1)
          if(expEqualSign != '='){
            stop("Syntax error: Expected equal sign '==' for in function term: '", qTrim, "'\n")
          }
          op = '='
          funcValuePos = 2
          if(substr(funcValueTerm, 2, 2) == '='){
            funcValuePos = 3
            op = '=='
          }
          funcValue = stringr::str_trim(substring(text = funcValueTerm, 
                                                  first = funcValuePos))
        }else{
          stop("Syntax error: function ", 
               funcName, 
               " requires function value in: '", 
               qTrim, 
               "'\n")
        }
      } 
      if(funcName == 'Start'){
        cond = NULL
        if(funcValue == '0' | funcValue == 'F' | funcValue == 'FALSE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " lr_exp_res_tmp.db_uuid, ",
                                                        " lr_exp_res_tmp.session, ",
                                                        " lr_exp_res_tmp.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " lr_exp_res_tmp.r_level AS level,  ",
                                                        " lr_exp_res_tmp.r_attribute AS attribute ",
                                                        "FROM lr_exp_res_tmp, ", itemsTableName, " AS i1 ",
                                                        "WHERE lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = i1.session ",
                                                        " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                        " AND lr_exp_res_tmp.r_level = i1.level ",
                                                        " AND lr_exp_res_tmp.r_seq_start_seq_idx < i1.seq_idx ",
                                                        " AND lr_exp_res_tmp.r_seq_end_seq_idx >= i1.seq_idx ",
                                                        ""))
          
        }else if(funcValue == '1' | funcValue == 'T' | funcValue == 'TRUE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " lr_exp_res_tmp.db_uuid, ",
                                                        " lr_exp_res_tmp.session, ",
                                                        " lr_exp_res_tmp.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " lr_exp_res_tmp.r_level AS level,  ",
                                                        " lr_exp_res_tmp.r_attribute AS attribute ",
                                                        "FROM lr_exp_res_tmp, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = i1.session ",
                                                        " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                        " AND lr_exp_res_tmp.r_level = i1.level ",
                                                        " AND lr_exp_res_tmp.r_seq_start_seq_idx = i1.seq_idx ",
                                                        ""))
          
        }else{
          stop("Syntax error: Expected function value TRUE or FALSE / T OR F / 0 or 1 after '",
               op,
               "' in function term: '",
               qTrim,
               "'\n")
        }
        
        resultLevel = param2
        
      }else if(funcName == 'Medial'){
        cond = NULL
        bOp = NULL
        if(funcValue == '0' | funcValue == 'F' | funcValue == 'FALSE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " lr_exp_res_tmp.db_uuid, ",
                                                        " lr_exp_res_tmp.session, ",
                                                        " lr_exp_res_tmp.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " lr_exp_res_tmp.r_level AS level, ",
                                                        " lr_exp_res_tmp.r_attribute AS attribute ",
                                                        "FROM lr_exp_res_tmp, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE (",
                                                        " lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = i1.session ",
                                                        " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                        " AND lr_exp_res_tmp.r_level = i1.level ",
                                                        " AND i1.seq_idx = lr_exp_res_tmp.r_seq_start_seq_idx) ",
                                                        "OR (lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = i1.session ",
                                                        " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                        " AND lr_exp_res_tmp.r_level = i1.level ",
                                                        " AND i1.seq_idx = lr_exp_res_tmp.r_seq_end_seq_idx)",
                                                        ""))
          
        }else if(funcValue == '1' | funcValue == 'T' | funcValue == 'TRUE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " lr_exp_res_tmp.db_uuid, ",
                                                        " lr_exp_res_tmp.session, ",
                                                        " lr_exp_res_tmp.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " lr_exp_res_tmp.r_level AS level, ",
                                                        " lr_exp_res_tmp.r_attribute AS attribute  ",
                                                        "FROM lr_exp_res_tmp, ", itemsTableName, " AS i1 ",
                                                        "WHERE lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = i1.session ",
                                                        " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                        " AND lr_exp_res_tmp.r_level = i1.level ",
                                                        " AND i1.seq_idx > lr_exp_res_tmp.r_seq_start_seq_idx ",
                                                        " AND i1.seq_idx < lr_exp_res_tmp.r_seq_end_seq_idx ",
                                                        ""))
        }else{
          stop("Syntax error: Expected function value 0 or 1 after '",
               op,
               "' in function term: '",
               qTrim,
               "'\n")
        }
        
        resultLevel = param2
      }else if(funcName == 'End'){
        cond = NULL
        if(funcValue == '0' | funcValue == 'F' | funcValue == 'FALSE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " lr_exp_res_tmp.db_uuid, ",
                                                        " lr_exp_res_tmp.session, ",
                                                        " lr_exp_res_tmp.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " lr_exp_res_tmp.r_level AS level,  ",
                                                        " lr_exp_res_tmp.r_attribute AS attribute  ",
                                                        "FROM lr_exp_res_tmp, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = i1.session ",
                                                        " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                        " AND lr_exp_res_tmp.r_level = i1.level ",
                                                        " AND i1.seq_idx >= lr_exp_res_tmp.r_seq_start_seq_idx ",
                                                        " AND i1.seq_idx < lr_exp_res_tmp.r_seq_end_seq_idx ",
                                                        ""))
          
        }else if(funcValue == '1'  | funcValue == 'T' | funcValue == 'TRUE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " lr_exp_res_tmp.db_uuid, ",
                                                        " lr_exp_res_tmp.session, ",
                                                        " lr_exp_res_tmp.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " lr_exp_res_tmp.r_level AS level, ",
                                                        " lr_exp_res_tmp.r_attribute AS attribute ",
                                                        "FROM lr_exp_res_tmp, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = i1.session ",
                                                        " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                        " AND lr_exp_res_tmp.r_level = i1.level ",
                                                        " AND lr_exp_res_tmp.r_seq_end_seq_idx = i1.seq_idx ",
                                                        ""))
          
        }else{
          stop("Syntax error: Expected function value 0 or 1 after '",
               op,
               "' in function term: '",
               qTrim,
               "'\n")
        }
        resultLevel = param2
      }else if(funcName == 'Num'){
        funcVal = NULL
        funcOpr = NULL
        for(opr in c('==',
                     '!=',
                     '<=',
                     '>=',
                     '=',
                     '>',
                     '<')){
          p = get_stringPosition(string = funcValueTerm,
                                 searchStr = opr)
          if(p == 1){
            oprLen = nchar(opr)
            funcOpr = substr(funcValueTerm, 1, oprLen)
            funcValStr = stringr::str_trim(substring(funcValueTerm, oprLen + 1))
            funcVal = as.integer(funcValStr)
            if(is.na(funcVal)){
              stop("Syntax error: Could not parse Num function value as integer: '",
                   funcValStr,
                   "'\n")
            }
            break
          }
        }
        if(is.null(funcOpr) | is.null(funcVal)){
          stop("Syntax error: Unknown operator and/or value for Num  function: '",
               funcValueTerm,
               "'\n")
        }
        if(funcOpr == '=='){
          sqlFuncOpr = '='
        }else{
          sqlFuncOpr = funcOpr
        }
        
        # EBNF: NUMQ = 'Num','(',Level,',',Level,')',COP,INTPN;
        DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                      "SELECT ",
                                                      " lr_exp_res_tmp.db_uuid, ",
                                                      " lr_exp_res_tmp.session, ",
                                                      " lr_exp_res_tmp.bundle, ",
                                                      " i1.item_id AS seq_start_id, ",
                                                      " i1.item_id AS seq_end_id, ",
                                                      " 1 AS seq_len, ",
                                                      " '", level1, "' AS level, ",
                                                      " '", param1, "' AS attribute ",
                                                      "FROM lr_exp_res_tmp, ", 
                                                      itemsTableName, " AS i1 ",
                                                      "WHERE lr_exp_res_tmp.db_uuid = i1.db_uuid ",
                                                      " AND lr_exp_res_tmp.session = i1.session ",
                                                      " AND lr_exp_res_tmp.bundle = i1.bundle ",
                                                      " AND lr_exp_res_tmp.l_seq_start_id = i1.item_id ", # parents are never sequences
                                                      " AND (lr_exp_res_tmp.r_seq_end_seq_idx - lr_exp_res_tmp.r_seq_start_seq_idx) + 1 ", 
                                                      sqlFuncOpr, " ",  
                                                      funcVal, " ",
                                                      ""))
        resultLevel = param1
      }else{
        stop("Syntax error: Unknwon function: '", funcName, "'")
      }
      
      # merge results with itemsTableName table (in case filtered subsets are used) 
      # and place in interm_res_items_tmp_ + intermResTableSuffix table
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM interm_res_items_tmp_", intermResTableSuffix))
      
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("INSERT INTO interm_res_items_tmp_", intermResTableSuffix, " ",
                            "SELECT ",
                            " iast.db_uuid, ",
                            " iast.session, ",
                            " iast.bundle, ",
                            " iast.seq_start_id, ",
                            " iast.seq_end_id, ",
                            " iast.seq_len, ",
                            " iast.level, ",
                            " iast.attribute, ",
                            " it.seq_idx AS seq_start_seq_idx, ",
                            " it.seq_idx AS seq_end_seq_idx ", 
                            "FROM items_as_seqs_tmp AS iast, ", 
                            itemsTableName, " AS it ", 
                            "WHERE iast.db_uuid = it.db_uuid ",
                            " AND iast.session = it.session ",
                            " AND iast.bundle = it.bundle ",
                            " AND iast.seq_start_id = it.item_id ",
                            ""))
      
      # drop temp table
      DBI::dbExecute(emuDBhandle$connection,
                     paste0("DROP TABLE IF EXISTS seq_idx_tmp"))
      DBI::dbExecute(emuDBhandle$connection,
                     paste0("DROP TABLE IF EXISTS items_as_seqs_tmp"))
      
    }
  }else{
    stop("Syntax error: Missing opening round bracket '(' in '",
         q,
         "'\n")
  }
}

###########################
query_databaseEqlLABELQ <- function(emuDBhandle, 
                                    q, 
                                    sessionPattern, 
                                    bundlePattern, 
                                    useSubsets, 
                                    intermResTableSuffix){
  # EBNF: LABELQ = ['#'],LEVEL,("=" | "==" | "!=" | "=~" | "!~"),LABELALTERNATIVES;
  
  qTrim = stringr::str_trim(q)
  dbConfig = load_DBconfig(emuDBhandle)
  for(opr in c('==',
               '!=',
               '=~',
               '!~',
               '=')){
    p = get_stringPosition(string = q, 
                           searchStr = opr, 
                           literalQuote = "'")
    if(p != -1){
      oprLen = nchar(opr)
      level = substr(q, 1, p - 1)
      projectionLevel = FALSE
      attributeTrim = stringr::str_trim(level)
      attributeName = attributeTrim
      if(grepl('^#', attributeTrim)){
        # projection marker
        # the EBNF does not allow white space between '#' and level string
        # but the implementation of Emu does, so we allow it here too
        
        attributeName = stringr::str_trim(substring(attributeTrim, 2))
        projectionLevel = TRUE
      }
      aNms = get_allAttributeNames(emuDBhandle)
      if(! (attributeName %in% aNms)){
        stop("Unknown level attribute name: '",
             attributeName,
             "'. Database attribute names are: ",
             paste(aNms, collapse = ','),
             "\n")
      }
      labelStr = substring(q, p + oprLen)
      labelTrim = stringr::str_trim(labelStr)  
      
      
      # check label for key chars
      # TODO Labels should to be allowed to contain key chars if they are single quoted 
      deniedStrs = c('^', '->', '==', '!=', '=')
      for(deniedStr in deniedStrs){
        pt = get_stringPosition(string = labelTrim, 
                                searchStr = deniedStr, 
                                literalQuote = "'")
        if(pt != -1){
          stop("Syntax error label ",
               labelStr,
               " contains '",
               deniedStr,
               "'. Quote label with ''.")
        }
      }
      
      # EBNF: LABELALTERNATIVES = LABEL , {'|',LABEL};
      # parse alternatives
      labelAlts = c()
      lp = 1
      lsp = 0
      while(lsp != -1){
        lsp = get_stringPosition(string = labelTrim, 
                                 pos = lp,
                                 searchStr = '|',
                                 literalQuote = "'")
        if(lsp != -1){
          if(lsp == 1){
            stop("Syntax error: label alternatives cannot start with '|' character in '",
                 labelTrim,
                 "'")
          }
          labelAltTerm = substr(labelTrim, lp, lsp - 1)
          labelAlt = stringr::str_trim(labelAltTerm)
          labelAlts = c(labelAlts, labelAlt)
          lp = lsp + 1
        }
      }
      # add last term
      labelAltTerm = substring(labelTrim,lp)
      labelAlt = stringr::str_trim(labelAltTerm)
      labelAlts = c(labelAlts, labelAlt)
      
      labelAltsUq = c()
      # unquote labels
      # EBNF: LABEL = LABELING | ("'",LABELING,"'");
      # Suggestion for improvement:
      # labelGroups (legacy EMU 'legal' directive) MUST NOT be quoted, to distinguish labelGroups from ordinary label or label pattern:
      # EBNF: LABEL = LABEL_GROUP_NAME | LABELING | ("'",LABELING,"'");
      #        LABELING = {ALPHA|DIGIT}
      
      for(labelAlt in labelAlts){
        label = NULL
        if(substr(labelAlt, 1, 1) == "'"){
          lblTrimLen = nchar(labelAlt)
          if(substring(labelAlt,lblTrimLen) != "'"){
            stop("Syntax error: expected closing single quote at end of label '",
                 labelAlt,
                 "'\n")
          }
          label = substr(labelAlt, 2, lblTrimLen - 1)
          labelAltsUq = c(labelAltsUq, label)
        }else{
          # check for labelGroup on level
          lvlDefs = dbConfig[['levelDefinitions']]
          isLabelGroup = FALSE
          for(lvlDef in lvlDefs){
            for(attrDef in lvlDef[['attributeDefinitions']]){
              if(attributeName == attrDef[['name']]){
                lblGrps = attrDef[['labelGroups']]
                for(lblGrp in lblGrps){
                  if(labelAlt == lblGrp[['name']]){
                    # is label group, expand
                    for(lblGrpVal in lblGrp[['values']]){
                      labelAltsUq = c(labelAltsUq,lblGrpVal)
                    }
                    isLabelGroup = TRUE
                    break
                  }
                }
              }
            }
          }
          if(!isLabelGroup){
            # check for database labelGroup
            dbLblGrps=dbConfig$labelGroups
            for(dbLblGrp in dbLblGrps){
              if(labelAlt == dbLblGrp[['name']]){
                # is label group, expand
                for(dbLblGrpVal in dbLblGrp[['values']]){
                  labelAltsUq = c(labelAltsUq, dbLblGrpVal)
                }
                isLabelGroup = TRUE
                break
              }
            }
          }
          if(!isLabelGroup){
            # ordinary label
            label = labelAlt
            labelAltsUq = c(labelAltsUq, label)
          }
        }
      }
      cond = NULL
      cond = create_conditionTextAlternatives(opr, labelAltsUq)
      
      query_labels(emuDBhandle, 
                   attributeName = attributeName, 
                   intermResTableSuffix = intermResTableSuffix, 
                   cond, 
                   sessionPattern, 
                   bundlePattern, 
                   useSubsets)
      if(projectionLevel){
        DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_proj_items_tmp_", intermResTableSuffix, " ",
                                                      "SELECT ",
                                                      " db_uuid, ",
                                                      " session, ",
                                                      " bundle, ",
                                                      " seq_start_id, ",
                                                      " seq_end_id, ",
                                                      " seq_start_id AS p_seq_start_id, ",
                                                      " seq_end_id AS p_seq_end_id, ",
                                                      " seq_len AS p_seq_len, ",
                                                      " level AS p_level, ",
                                                      " attribute AS p_attribute, ",
                                                      " seq_start_seq_idx AS p_seq_start_seq_idx, ",
                                                      " seq_end_seq_idx AS p_seq_end_seq_idx ",
                                                      "FROM interm_res_items_tmp_", intermResTableSuffix))
        
      }
      return()
    }
  }
  stop("Syntax error: No operator found.")
}

# EBNF: SQ = LABELQ | FUNCQ;
query_databaseEqlSQ <- function(emuDBhandle, 
                                q, 
                                sessionPattern, 
                                bundlePattern, 
                                intermResTableSuffix, 
                                useSubsets,
                                verbose){
  
  qTrim = stringr::str_trim(q)
  res = NULL
  # detect function calls by existence of round brackets
  prbOpen = get_stringPosition(string = qTrim, 
                               searchStr = '(', 
                               literalQuote = "'")
  if(prbOpen != -1){
    prbClose = get_stringPosition(string = qTrim, 
                                  searchStr = ')',
                                  literalQuote = "'")
    if(prbClose == -1){
      stop("Syntax error: Missing closing round bracket ')' in '",
           q,
           "'\n")
    }else{
      if(prbOpen > prbClose){
        stop("Syntax error: Expected opening round bracket '(' before closing round bracket in '",
             q,
             "'\n")
      }
      if(prbOpen == 1){
        stop("Syntax error: Expected function name in '",
             q,
             "'\n")
      }
      query_databaseEqlFUNCQ(emuDBhandle, 
                             qTrim, 
                             intermResTableSuffix, 
                             sessionPattern, 
                             bundlePattern, 
                             useSubsets, 
                             verbose = verbose)
    }
  }else{
    # No round brackets, assuming a level query
    query_databaseEqlLABELQ(emuDBhandle, 
                            qTrim, 
                            sessionPattern, 
                            bundlePattern, 
                            useSubsets, 
                            intermResTableSuffix = intermResTableSuffix)
  }
}

# EBNF: CONJQ = SQ,{'&',SQ};
query_databaseEqlCONJQ <- function(emuDBhandle, 
                                   q,
                                   sessionPattern, 
                                   bundlePattern, 
                                   intermResTableSuffix,
                                   verbose){
  qTrim = stringr::str_trim(q)
  conditions = list()
  # initialize with empty result
  startPos = 1
  p = 0
  resultAttribute = NULL
  projection = FALSE
  useSubsets = FALSE
  
  # parse through all terms of and (&) operation
  while(p >= 0){
    # find ampersand '&' char
    p = get_stringPosition(string = qTrim,
                           searchStr = '&',
                           pos = startPos,
                           literalQuote = "'")
    if(p == -1){
      # get single term
      condStr = stringr::str_trim(substring(qTrim, startPos))
    }else{
      # get leading term
      condStr = stringr::str_trim(substr(qTrim, startPos, p - 1))
      # advance to next
      startPos = p + 1
    }
    # find projection marker (#) in condStr
    pHash = get_stringPosition(string = condStr,
                               searchStr = '#',
                               literalQuote = "'")
    if(pHash != -1){
      if(projection){
        stop("Only one hashtag allowed in linear query term: ",
             qTrim)
      }else{
        projection = TRUE
      }
    }
    # execute query on term
    query_databaseEqlSQ(emuDBhandle, 
                        condStr, 
                        sessionPattern, 
                        bundlePattern, 
                        intermResTableSuffix, 
                        useSubsets = useSubsets,
                        verbose = verbose)
    
    # set resultAttribute of first term
    if(is.null(resultAttribute)){
      resultAttribute = DBI::dbGetQuery(emuDBhandle$connection, 
                                        paste0("SELECT DISTINCT attribute FROM interm_res_items_tmp_", intermResTableSuffix))$attribute
    }
    
    nRes = DBI::dbGetQuery(emuDBhandle$connection, 
                           paste0("SELECT COUNT(*) AS n FROM interm_res_items_tmp_", intermResTableSuffix))$n
    if(nRes == 0){
      # empty result stop here and return
      return()
    }else{
      # remove all entries from subsets
      DBI::dbExecute(emuDBhandle$connection, "DELETE FROM items_filtered_subset_tmp")
      DBI::dbExecute(emuDBhandle$connection, "DELETE FROM labels_filtered_subset_tmp")
      
      # Proceed with items matching current condition by placeing them into subset tabels
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_filtered_subset_tmp ",
                                                    "SELECT DISTINCT i.* ",
                                                    "FROM items AS i, ", 
                                                    " interm_res_items_tmp_", intermResTableSuffix, " imr ",
                                                    "WHERE i.db_uuid = imr.db_uuid ",
                                                    " AND i.session = imr.session ",
                                                    " AND i.bundle = imr.bundle ",
                                                    " AND i.item_id = imr.seq_start_id ",
                                                    " AND i.session REGEXP '", sessionPattern, "' ",
                                                    " AND i.bundle REGEXP '", bundlePattern, "' ",
                                                    ""))
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO labels_filtered_subset_tmp ",
                                                    "SELECT DISTINCT l.* ",
                                                    "FROM labels AS l, ",
                                                    " interm_res_items_tmp_", intermResTableSuffix, " imr ",
                                                    "WHERE l.db_uuid = imr.db_uuid ",
                                                    " AND l.session = imr.session ",
                                                    " AND l.bundle = imr.bundle ",
                                                    " AND l.item_id = imr.seq_start_id ",
                                                    " AND l.session REGEXP '", sessionPattern, "' ",
                                                    " AND l.bundle REGEXP '", bundlePattern, "' ",
                                                    ""))
      
      useSubsets = TRUE
      
    }
  }
  DBI::dbExecute(emuDBhandle$connection, 
                 paste0("UPDATE interm_res_items_tmp_", intermResTableSuffix, " ",
                        "SET attribute ='", resultAttribute, "'"))
}


# Attempt of a function to replace the old query_databaseHier function with a "simple"
# and more perfomant CTE version that walks up and down the hierarchy
# @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
# @param startItemsTableSuffix suffix of 'interm_res_items_tmp_' table
# in which starting item sequences are stored
# @param targetItemsAttributeName name of target attribute to walk down/up to.
# The level name containing the attribute is acquired using the \code{get_levelNameForAttributeName}
# function.
# @param preserveStartItemsRowLength preserve the length (nrow()) of the table
# that is passed in (currently unused)
# @param walkDown if set to TRUE (the default) start items seqs are parents, targets are childs
# if FALSE start items seqs are children and targets are parents
# @param sessionPattern Regex used to filter sessions
# @param bundlePattern Regex used to filter bundles
# @param verbose be verbose (interactively query hierarchy path if multiple paths are available)
query_hierarchyWalk <- function(emuDBhandle, 
                                startItemsTableSuffix, 
                                targetItemsAttributeName,
                                preserveStartItemsRowLength, 
                                walkDown = TRUE,
                                sessionPattern = ".*",
                                bundlePattern = ".*",
                                verbose) {
  
  # get hierarchy paths
  startItemsAttributeName = unique(na.omit(DBI::dbReadTable(emuDBhandle$connection, 
                                                            paste0("interm_res_items_tmp_", 
                                                                   startItemsTableSuffix))$level))
  
  startItemsLevelName = get_levelNameForAttributeName(emuDBhandle, startItemsAttributeName)
  
  targetItemsLevelName = get_levelNameForAttributeName(emuDBhandle, targetItemsAttributeName)
  
  connectHierPaths = get_hierPathsConnectingLevels(emuDBhandle, 
                                                   startItemsLevelName,
                                                   targetItemsLevelName)
  
  # check if multiple paths are available
  # and ask user to choose a path (only in verbose mode)
  if(verbose & length(connectHierPaths) >= 2){
    
    cat(paste0("More than one path connecting: '", 
               startItemsLevelName, 
               "' and '", 
               targetItemsLevelName, 
               "' was found! The paths were: \n" ))
    for(i in 1:length(connectHierPaths)){
      cat(paste0(i, ".) ", 
                 paste0(connectHierPaths[[i]], collapse = "->")), 
          "\n")
    }
    idx <- readline(prompt="Choose a path by selecting its number (note that comma seperated numbers (e.g., 1, 2, 3) works to select multiple paths): ")
    
    idx = as.integer(stringr::str_split(idx, ",\\s*", simplify = TRUE))
    # check if on path in CTE (see below)
    sqlStr_checkIfOnPath = paste0("    AND i.level IN ('", paste0(connectHierPaths[idx], collapse = "', '"), "')")
  } else {
    # no checks if on path in CTE (see below)
    sqlStr_checkIfOnPath = ""  
  }
  
  # empty table just to be safe
  DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM lr_exp_res_tmp"))
  
  #########################################################
  # perform CTE that walks up/down the hierarchy using links 
  # and checks if on correct path
  # results are written to lr_exp_res_tmp
  # where left side are starting items seqs and right side are target items 
  
  # depending on preserveStartItemsRowLength
  # collapse children into sequences and preserve
  # NA row placement using diff. join types & ORDER BY
  if(preserveStartItemsRowLength){
    joinType = "LEFT JOIN"
    
    selectString = "SELECT " # carefull but this is needed to not : UNION ALL doesn't check for duplicates
    
    groupByString = paste0("GROUP BY irit.rowid, ", # using irit.rowid to preserve duplicates (requery only)
                           " irit.db_uuid, ", 
                           " irit.session, ",
                           " irit.bundle, ", 
                           " irit.seq_start_id, ", 
                           " irit.seq_end_id ")
    
    orderByString = "ORDER BY irit.rowid" # don't reorder if left joining to perserve NA/NULL row placement
  }else{
    joinType = "INNER JOIN"
    
    selectString = "SELECT DISTINCT " # distinct because UNION ALL doesn't check for duplicates
    
    groupByString = paste0("GROUP BY cte_hier.db_uuid, ",
                           " cte_hier.session, ", 
                           " cte_hier.bundle, ",
                           " cte_hier.item_id ")
    
    orderByString = paste0("ORDER BY irit.db_uuid, ",
                           " irit.session, ",
                           " irit.bundle, ",
                           " irit.seq_start_seq_idx")
    
  }
  
  # depending on walkDown switch to/from_id order in join
  if(walkDown) {
    sqlStr_firstItemTableLinkId = "    AND ch.item_id = l.from_id "
    sqlStr_secondItemTableLinkId = "    AND l.to_id = i.item_id "
  }else {
    sqlStr_firstItemTableLinkId = "    AND ch.item_id = l.to_id "
    sqlStr_secondItemTableLinkId = "    AND l.from_id = i.item_id "
  }
  
  DBI::dbExecute(emuDBhandle$connection, paste0("WITH RECURSIVE cte_hier AS (",
                                                " SELECT irit.rowid AS start_items_table_row_idx, ", # anchor: expand seqs
                                                "  items.* ", 
                                                " FROM interm_res_items_tmp_", startItemsTableSuffix, " AS irit, ",
                                                "    items ",
                                                "  WHERE irit.db_uuid = items.db_uuid ",
                                                "    AND irit.session = items.session ",
                                                "    AND irit.bundle = items.bundle ",
                                                "    AND items.level = '", startItemsLevelName, "' ",
                                                "    AND items.seq_idx BETWEEN irit.seq_start_seq_idx AND irit.seq_end_seq_idx ",  
                                                " UNION ALL ", # contains repeats -> faster coz no checking of duplicates
                                                " SELECT ch.start_items_table_row_idx, i.* ", # recursive part of CTE: join cte_hier to items using links
                                                " FROM cte_hier AS ch ",
                                                " INNER JOIN links AS l ",
                                                " ON ch.db_uuid = l.db_uuid ",
                                                "    AND ch.session = l.session ",
                                                "    AND ch.bundle = l.bundle ",
                                                sqlStr_firstItemTableLinkId,
                                                "    AND l.session REGEXP '", sessionPattern, "' ", # limit to session RegEx
                                                "    AND l.bundle REGEXP '", bundlePattern, "' ", # limit to bundle RegEx
                                                " INNER JOIN items AS i ",
                                                " ON l.db_uuid = i.db_uuid ",
                                                "    AND l.session = i.session ",
                                                "    AND l.bundle = i.bundle ",
                                                sqlStr_secondItemTableLinkId,
                                                sqlStr_checkIfOnPath, # check that on path (if str is set)
                                                "    AND i.session REGEXP '", sessionPattern, "' ", # limit to session RegEx
                                                "    AND i.bundle REGEXP '", bundlePattern, "' ", # limit to bundle RegEx
                                                ") ",
                                                "INSERT INTO lr_exp_res_tmp ",
                                                # "SELECT * FROM cte_hier ",
                                                selectString,
                                                " irit.db_uuid, ",
                                                " irit.session, ",
                                                " irit.bundle, ",
                                                " irit.seq_start_id AS l_seq_start_id, ",
                                                " irit.seq_end_id AS l_seq_end_id, ",
                                                " irit.seq_len AS l_seq_len, ",
                                                " irit.level AS l_level, ",
                                                " irit.attribute AS l_attribute, ",
                                                " irit.seq_start_seq_idx AS l_seq_start_seq_idx, ",
                                                " irit.seq_end_seq_idx AS l_seq_end_seq_idx,",
                                                " NULL AS r_seq_start_id, ",
                                                " NULL AS r_seq_end_id, ",
                                                " 1 AS r_seq_len, ",
                                                " cte_hier.level AS r_level, ",
                                                " '", targetItemsAttributeName, "' AS r_attribute, ",
                                                " min(cte_hier.seq_idx) AS r_seq_start_seq_idx, ",
                                                " max(cte_hier.seq_idx) AS r_seq_end_seq_idx ",
                                                "FROM interm_res_items_tmp_", startItemsTableSuffix ," AS irit ",
                                                joinType, " cte_hier ",
                                                "ON irit.rowid = cte_hier.start_items_table_row_idx ",
                                                " AND cte_hier.level = '", targetItemsLevelName, "'", # extract only child levels
                                                groupByString,
                                                orderByString,
                                                ""))
  # View(DBI::dbReadTable(emuDBhandle$connection, paste0("lr_exp_res_tmp")))
  
  # calculate and update missing r_seq_start_id & r_seq_end_id
  DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE lr_exp_res_tmp ",
                                                "SET r_seq_start_id = joined.item_id ",
                                                "FROM ( ",
                                                " SELECT items.item_id AS item_id, ",
                                                "  items.db_uuid, ",
                                                "  items.session, ",
                                                "  items.bundle, ",
                                                "  items.level, ",
                                                "  items.seq_idx, ",
                                                "  items.item_id ",
                                                " FROM lr_exp_res_tmp, ",
                                                "  items ",
                                                " WHERE lr_exp_res_tmp.db_uuid = items.db_uuid ",
                                                " AND lr_exp_res_tmp.session = items.session ",
                                                " AND lr_exp_res_tmp.bundle = items.bundle ",
                                                " AND lr_exp_res_tmp.r_level = items.level ",
                                                " AND lr_exp_res_tmp.r_seq_start_seq_idx = items.seq_idx ",
                                                ") as joined ",
                                                "WHERE lr_exp_res_tmp.db_uuid = joined.db_uuid ",
                                                "AND lr_exp_res_tmp.session = joined.session ",
                                                "AND lr_exp_res_tmp.bundle = joined.bundle ",
                                                "AND lr_exp_res_tmp.r_level = joined.level ",
                                                "AND lr_exp_res_tmp.r_seq_start_seq_idx = joined.seq_idx ",
                                                ""))
  
  DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE lr_exp_res_tmp ",
                                                "SET r_seq_end_id = joined.item_id ",
                                                "FROM ( ",
                                                " SELECT items.item_id AS item_id, ",
                                                "  items.db_uuid, ",
                                                "  items.session, ",
                                                "  items.bundle, ",
                                                "  items.level, ",
                                                "  items.seq_idx, ",
                                                "  items.item_id ",
                                                " FROM lr_exp_res_tmp, ",
                                                "  items ",
                                                " WHERE lr_exp_res_tmp.db_uuid = items.db_uuid ",
                                                " AND lr_exp_res_tmp.session = items.session ",
                                                " AND lr_exp_res_tmp.bundle = items.bundle ",
                                                " AND lr_exp_res_tmp.r_level = items.level ",
                                                " AND lr_exp_res_tmp.r_seq_end_seq_idx = items.seq_idx ",
                                                ") as joined ",
                                                "WHERE lr_exp_res_tmp.db_uuid = joined.db_uuid ",
                                                "AND lr_exp_res_tmp.session = joined.session ",
                                                "AND lr_exp_res_tmp.bundle = joined.bundle ",
                                                "AND lr_exp_res_tmp.r_level = joined.level ",
                                                "AND lr_exp_res_tmp.r_seq_end_seq_idx = joined.seq_idx ",
                                                ""))
}


##################################
query_databaseEqlInBracket<-function(emuDBhandle, 
                                     q, 
                                     sessionPattern, 
                                     bundlePattern, 
                                     intermResTableSuffix, 
                                     leftRightTableNrCounter = 0,
                                     verbose){
  parseRes = list()
  qTrim = stringr::str_trim(q)
  # parse SEQQ or DOMQ
  seqPos = get_stringPositionOutsideBrackets(qTrim, '->', literalQuote = "'", bracket = c('[',']'))
  domPos = get_stringPositionOutsideBrackets(qTrim, '^', literalQuote = "'", bracket = c('[',']'))
  if(seqPos != -1 || domPos != -1){
    # parse DOMQ or SEQQ
    lExpRes = NULL
    prjIts = NULL
    if(domPos != -1){
      left = stringr::str_trim(substr(qTrim, 1, domPos - 1))
      right = stringr::str_trim(substring(qTrim, domPos + 1))
    }else if(seqPos != -1){
      left = stringr::str_trim(substr(qTrim, 1, seqPos - 1))
      right = stringr::str_trim(substring(qTrim, seqPos + 2))
    }
    
    # create left & right temp table
    leftTableSuffix = paste0("left_", leftRightTableNrCounter)
    rightTableSuffix = paste0("right_", leftRightTableNrCounter)
    leftRightTableNrCounter = leftRightTableNrCounter + 1
    
    create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = leftTableSuffix)
    create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = rightTableSuffix)
    
    query_databaseWithEql(emuDBhandle, 
                          left, 
                          sessionPattern, 
                          bundlePattern, 
                          intermResTableSuffix = leftTableSuffix, 
                          leftRightTableNrCounter,
                          verbose)
    query_databaseWithEql(emuDBhandle, 
                          right, 
                          sessionPattern, 
                          bundlePattern, 
                          intermResTableSuffix = rightTableSuffix, 
                          leftRightTableNrCounter + 1, 
                          verbose)
    
    # check if left or right side results are empty -> clear tabels and return
    nLeftResIts = DBI::dbGetQuery(emuDBhandle$connection, 
                                  paste0("SELECT COUNT(*) AS n FROM interm_res_items_tmp_", leftTableSuffix))$n
    if(nLeftResIts == 0){
      clear_intermResTabels(emuDBhandle, leftTableSuffix)
      clear_intermResTabels(emuDBhandle, rightTableSuffix)
      return()
    }
    
    
    nRightResIts = DBI::dbGetQuery(emuDBhandle$connection, 
                                   paste0("SELECT COUNT(*) AS n FROM interm_res_items_tmp_", rightTableSuffix))$n
    if(nRightResIts == 0){
      clear_intermResTabels(emuDBhandle, leftTableSuffix)
      clear_intermResTabels(emuDBhandle, rightTableSuffix)
      return()
    }
    
    nLeftProjItems = DBI::dbGetQuery(emuDBhandle$connection, 
                                     paste0("SELECT COUNT(*) AS n FROM interm_res_proj_items_tmp_", leftTableSuffix))$n
    nRightProjItems = DBI::dbGetQuery(emuDBhandle$connection, 
                                      paste0("SELECT COUNT(*) AS n FROM interm_res_proj_items_tmp_", rightTableSuffix))$n
    
    if(nLeftProjItems != 0 & nRightProjItems != 0){
      stop("Multiple hash tags '#' not allowed in EQL2 query!")
    }
    # get items on dominance compare levels
    lResAttrName = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT attribute ",
                                                                  "FROM interm_res_items_tmp_", leftTableSuffix))$attribute
    lResLvl = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT level ",
                                                             "FROM interm_res_items_tmp_", leftTableSuffix))$level
    
    rResAttrName = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT attribute ",
                                                                  "FROM interm_res_items_tmp_", rightTableSuffix))$attribute
    rResLvl = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT level ",
                                                             "FROM interm_res_items_tmp_", rightTableSuffix))$level
    
    if(domPos != -1 & lResLvl == rResLvl){
      stop("Dominance query on same levels impossible.\nLeft level: ",
           lResLvl,
           " (attr:", lResAttrName, ") equals right level: ",
           lResLvl,
           " (attr:", rResAttrName, ")\n")
    }
    # check equal levels for sequence query
    # (Do this already at this point, fixes issue: Sequence query should 
    # always throw an error if arguments not on same level. #39 )
    if(seqPos != -1 & lResLvl != rResLvl){
      stop("Queried attribute names of sequence query '", 
           qTrim,
           "' do not match. (",
           lResAttrName,
           " not equal ",
           rResAttrName,")")
    }
    
    
    if(domPos != -1){
      # parse DOMQ
      # query the result level of left term
      nLinks = DBI::dbGetQuery(emuDBhandle$connection, 
                               paste0("SELECT COUNT(*) AS n FROM links", 
                                      " WHERE links.session REGEXP '", sessionPattern, "' ",
                                      " AND links.bundle REGEXP '", bundlePattern, "' "))$n
      if(nLinks == 0){
        clear_intermResTabels(emuDBhandle, leftTableSuffix)
        clear_intermResTabels(emuDBhandle, rightTableSuffix)
        return()
      }
      # check which side is parent
      hierPaths = get_hierPathsConnectingLevels(emuDBhandle, 
                                                lResLvl,
                                                rResLvl)
      if(which(hierPaths[[1]] == lResLvl) < which(hierPaths[[1]] == rResLvl)) {
        leftIsParent = TRUE
      } else {
        leftIsParent = FALSE
      }
      
      if(leftIsParent){
        # get all child sequences of childLevel that are linked to items on parentLevel
        query_hierarchyWalk(emuDBhandle, 
                            startItemsTableSuffix = leftTableSuffix, 
                            targetItemsAttributeName = rResLvl,
                            preserveStartItemsRowLength = TRUE, # get sequences (i.e. collapse)
                            sessionPattern = sessionPattern,
                            bundlePattern = bundlePattern,
                            walkDown = TRUE,
                            verbose = verbose) # result written to lr_exp_res_tmp table (left parents/right children)
        # reduce to sequences in rightTableSuffix
        #DBI::dbReadTable(emuDBhandle$connection, "lr_exp_res_tmp")
        #DBI::dbReadTable(emuDBhandle$connection, paste0("interm_res_items_tmp_", rightTableSuffix))
        # TODO don't extract and rewrite but to all in SQL
        lrertTmp = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT lrert.db_uuid,", # retain left side as parents
                                                                  " lrert.session, ",
                                                                  " lrert.bundle, ",
                                                                  " lrert.l_seq_start_id, ",
                                                                  " lrert.l_seq_end_id, ",
                                                                  " lrert.l_seq_len, ",
                                                                  " lrert.l_level, ",
                                                                  " lrert.l_attribute, ",
                                                                  " lrert.l_seq_start_seq_idx, ",
                                                                  " lrert.l_seq_end_seq_idx, ",
                                                                  " irit.seq_start_id AS r_seq_start_id, ",
                                                                  " irit.seq_end_id AS r_seq_end_id, ",
                                                                  " irit.seq_len AS r_seq_len, ",
                                                                  " irit.level AS r_level, ",
                                                                  " irit.attribute AS r_attribute, ",
                                                                  " irit.seq_start_seq_idx AS r_seq_start_seq_idx, ",
                                                                  " irit.seq_end_seq_idx AS r_seq_end_seq_idx ",
                                                                  "FROM interm_res_items_tmp_", rightTableSuffix, " AS irit ",
                                                                  "JOIN lr_exp_res_tmp AS lrert ",
                                                                  "ON irit.db_uuid = lrert.db_uuid ",
                                                                  " AND irit.session = lrert.session ", 
                                                                  " AND irit.bundle = lrert.bundle ", 
                                                                  " AND irit.level = lrert.r_level ", 
                                                                  " AND irit.seq_start_seq_idx ", 
                                                                  "  BETWEEN lrert.r_seq_start_seq_idx ",
                                                                  "   AND lrert.r_seq_end_seq_idx ", # r_seq_start_seq_idx coz all have length 1
                                                                  " AND irit.seq_end_seq_idx ", 
                                                                  "  BETWEEN lrert.r_seq_start_seq_idx ",
                                                                  "   AND lrert.r_seq_end_seq_idx ", # r_seq_start_seq_idx coz all have length 1
                                                                  "")) 
        
      } else {
        # get all child sequences of childLevel that are linked to items on parentLevel
        query_hierarchyWalk(emuDBhandle, 
                            startItemsTableSuffix = rightTableSuffix, 
                            targetItemsAttributeName = lResLvl,
                            preserveStartItemsRowLength = TRUE, # get sequences (i.e. collapse)
                            sessionPattern = sessionPattern,
                            bundlePattern = bundlePattern,
                            walkDown = TRUE,
                            verbose = verbose) # result written to lr_exp_res_tmp table (left parents/right children)
        
        # reduce to sequences in rightTableSuffix
        # TODO don't extract and rewrite but to all in SQL
        lrertTmp = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT lrert.db_uuid,", # switch sides to maint. lr_ order of query
                                                                  " lrert.session, ",
                                                                  " lrert.bundle, ",
                                                                  " irit.seq_start_id AS l_seq_start_id, ",
                                                                  " irit.seq_end_id AS l_seq_end_id, ",
                                                                  " irit.seq_len AS l_seq_len, ",
                                                                  " irit.level AS l_level, ",
                                                                  " irit.attribute AS l_attribute, ",
                                                                  " irit.seq_start_seq_idx AS l_seq_start_seq_idx, ",
                                                                  " irit.seq_end_seq_idx AS l_seq_end_seq_idx, ",
                                                                  " lrert.l_seq_start_id AS r_seq_start_id, ",
                                                                  " lrert.l_seq_end_id AS r_seq_end_id, ",
                                                                  " lrert.l_seq_len AS r_seq_len, ",
                                                                  " lrert.l_level AS r_level, ",
                                                                  " lrert.l_attribute AS r_attribute, ",
                                                                  " lrert.l_seq_start_seq_idx AS r_seq_start_seq_idx, ",
                                                                  " lrert.l_seq_end_seq_idx AS r_seq_end_seq_idx ",
                                                                  "FROM interm_res_items_tmp_", leftTableSuffix, " AS irit ",
                                                                  "JOIN lr_exp_res_tmp AS lrert ",
                                                                  "ON irit.db_uuid = lrert.db_uuid ",
                                                                  " AND irit.session = lrert.session ", 
                                                                  " AND irit.bundle = lrert.bundle ", 
                                                                  " AND irit.level = lrert.r_level ", 
                                                                  " AND irit.seq_start_seq_idx ", 
                                                                  "  BETWEEN lrert.r_seq_start_seq_idx ",
                                                                  "   AND lrert.r_seq_end_seq_idx ",
                                                                  " AND irit.seq_end_seq_idx ", 
                                                                  "  BETWEEN lrert.r_seq_start_seq_idx ",
                                                                  "   AND lrert.r_seq_end_seq_idx ", 
                                                                  ""))
        
        
      }
      # write back to table
      DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM lr_exp_res_tmp"))
      
      DBI::dbWriteTable(emuDBhandle$connection, 
                        name = "lr_exp_res_tmp",
                        value = lrertTmp, 
                        append = TRUE)
      
      nLrExpRes = DBI::dbGetQuery(emuDBhandle$connection, 
                                  "SELECT COUNT(*) AS n FROM lr_exp_res_tmp")$n
      if(nLrExpRes > 0){
        if(nLeftProjItems != 0){
          # reduce projection items to DOMQ result items and store in correct table
          qStr = paste0("SELECT ",
                        " i.db_uuid, ",
                        " i.session, ",
                        " i.bundle, ",
                        " i.l_seq_start_id AS seq_start_id, ",
                        " i.l_seq_end_id AS seq_end_id, ",
                        " pi.p_seq_start_id, ",
                        " pi.p_seq_end_id, ",
                        " pi.p_seq_len, ",
                        " pi.p_level, ",
                        " pi.p_attribute, ",
                        " pi.p_seq_start_seq_idx, ",
                        " pi.p_seq_end_seq_idx ",
                        "FROM lr_exp_res_tmp i, ",
                        " interm_res_proj_items_tmp_", leftTableSuffix, " AS pi ",
                        "WHERE ",
                        " i.db_uuid = pi.db_uuid AND ",
                        " i.session = pi.session ",
                        " AND i.bundle = pi.bundle ",
                        " AND i.l_seq_start_id = pi.seq_start_id ",
                        " AND i.l_seq_end_id = pi.seq_end_id ")
          reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
          DBI::dbExecute(emuDBhandle$connection, 
                         paste0("DELETE FROM interm_res_proj_items_tmp_", intermResTableSuffix))
          DBI::dbWriteTable(emuDBhandle$connection, 
                            paste0("interm_res_proj_items_tmp_", intermResTableSuffix), 
                            reducedPI, 
                            append = TRUE,
                            row.names = FALSE)
          
        }
        
        if(nRightProjItems != 0) {
          # reduce projection items to DOMQ result items and store in correct table
          qStr = paste0("SELECT ",
                        " i.db_uuid, ",
                        " i.session, ",
                        " i.bundle, ",
                        " i.l_seq_start_id AS seq_start_id, ",
                        " i.l_seq_end_id AS seq_end_id, ",
                        " pi.p_seq_start_id, ",
                        " pi.p_seq_end_id, ",
                        " pi.p_seq_len, ",
                        " pi.p_level, ",
                        " pi.p_attribute, ",
                        " pi.p_seq_start_seq_idx, ",
                        " pi.p_seq_end_seq_idx ",
                        "FROM lr_exp_res_tmp i, ",
                        " interm_res_proj_items_tmp_", rightTableSuffix, " AS pi ",
                        "WHERE i.db_uuid = pi.db_uuid ",
                        " AND i.session = pi.session ",
                        " AND i.bundle=pi.bundle ",
                        " AND i.r_seq_start_id = pi.seq_start_id ",
                        " AND i.r_seq_end_id=pi.seq_end_id ",
                        "")
          reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
          DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM interm_res_proj_items_tmp_", intermResTableSuffix))
          DBI::dbWriteTable(emuDBhandle$connection, 
                            paste0("interm_res_proj_items_tmp_", intermResTableSuffix), 
                            reducedPI, 
                            append = TRUE,
                            row.names = FALSE)
          
        }
      }
      
      # place result in correct table
      resItems = DBI::dbGetQuery(emuDBhandle$connection, 
                                 paste0("SELECT DISTINCT ",
                                        " db_uuid, ",
                                        " session, ",
                                        " bundle, ",
                                        " l_seq_start_id AS seq_start_id, ",
                                        " l_seq_end_id AS seq_end_id, ",
                                        " l_seq_len AS seq_len, ",
                                        " l_level AS level, ",
                                        " l_attribute AS attribute, ",
                                        " l_seq_start_seq_idx AS seq_start_seq_idx, ",
                                        " l_seq_end_seq_idx AS seq_end_seq_idx ",
                                        "FROM lr_exp_res_tmp"))
      
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM interm_res_items_tmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, 
                        paste0("interm_res_items_tmp_", intermResTableSuffix), 
                        resItems, 
                        append = TRUE,
                        row.names = FALSE)
      
    }
    if(seqPos != -1){
      # query the result level of left term (removed lid.seq_end_id AS leId,rid.seq_start_id AS rsId,)
      lrSeqQueryStr = paste0("SELECT ",
                             " lid.db_uuid, ",
                             " lid.session, ",
                             " lid.bundle, ",
                             " lid.seq_start_id AS l_seq_start_id, ",
                             " lid.seq_end_id AS l_seq_end_id, ",
                             " lid.seq_len AS l_seq_len, ",
                             " lid.level AS l_level, ",
                             " lid.attribute AS l_attribute, ",
                             " lid.seq_start_seq_idx AS l_seq_start_seq_idx, ",
                             " lid.seq_end_seq_idx AS l_seq_end_seq_idx, ",
                             " rid.seq_start_id AS r_seq_start_id, ",
                             " rid.seq_end_id AS r_seq_end_id, ",
                             " rid.seq_len AS r_seq_len, ",
                             " rid.level AS r_level, ", # this was lid.level?
                             " rid.attribute AS r_attribute, ",
                             " rid.seq_start_seq_idx AS r_seq_start_seq_idx, ",
                             " rid.seq_end_seq_idx AS r_seq_end_seq_idx ",
                             "FROM interm_res_items_tmp_", leftTableSuffix, " AS lid, ",
                             " interm_res_items_tmp_", rightTableSuffix, " AS rid, ",
                             " items AS il, ",
                             " items AS ir ",
                             "WHERE il.db_uuid = ir.db_uuid ",
                             " AND il.session=ir.session ",
                             " AND il.bundle=ir.bundle ",
                             " AND il.db_uuid = lid.db_uuid ",
                             " AND il.session = lid.session ",
                             " AND il.bundle = lid.bundle ",
                             " AND il.db_uuid = rid.db_uuid ",
                             " AND il.session=rid.session ",
                             " AND il.bundle=rid.bundle ",
                             " AND il.item_id = lid.seq_end_id ",
                             " AND ir.item_id = rid.seq_start_id ",
                             " AND il.level = ir.level ",
                             " AND ir.seq_idx = il.seq_idx + 1 ",
                             " AND il.session REGEXP '", sessionPattern, "' ",
                             " AND il.bundle REGEXP '", bundlePattern, "' ",
                             "")
      
      
      # perform query an place result into lr_exp_res_tmp table
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM lr_exp_res_tmp"))
      insertQueryStr = paste0("INSERT INTO lr_exp_res_tmp ", lrSeqQueryStr)
      DBI::dbExecute(emuDBhandle$connection, insertQueryStr)
      
      # check if no sequences where found -> clear & return
      nSeq = DBI::dbGetQuery(emuDBhandle$connection, 
                             paste0("SELECT COUNT(*) AS n FROM lr_exp_res_tmp"))$n
      if(nSeq == 0) {
        clear_intermResTabels(emuDBhandle, leftTableSuffix)
        clear_intermResTabels(emuDBhandle, rightTableSuffix)
        return()
      }
      
      
      if(nLeftProjItems != 0){
        # reduce to projection items
        # check if SEQQ result items and store in correct table
        qStr=paste0("SELECT ",
                    " i.db_uuid, ",
                    " i.session, ",
                    " i.bundle, ",
                    " i.l_seq_start_id AS seq_start_id, ",
                    " i.r_seq_end_id AS seq_end_id, ",
                    " pi.p_seq_start_id, ",
                    " pi.p_seq_end_id, ",
                    " pi.p_seq_len, ",
                    " pi.p_level, ",
                    " pi.p_attribute, ",
                    " pi.p_seq_start_seq_idx, ",
                    " pi.p_seq_end_seq_idx ",
                    "FROM lr_exp_res_tmp i, ",
                    " interm_res_proj_items_tmp_", leftTableSuffix, " AS pi ",
                    "WHERE i.db_uuid = pi.db_uuid ",
                    " AND i.session = pi.session ",
                    " AND i.bundle=pi.bundle ",
                    " AND i.l_seq_start_id = pi.seq_start_id ",
                    " AND i.l_seq_end_id = pi.seq_end_id")
        
        reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
        DBI::dbExecute(emuDBhandle$connection, 
                       paste0("DELETE FROM interm_res_proj_items_tmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, 
                          paste0("interm_res_proj_items_tmp_", intermResTableSuffix), 
                          reducedPI, 
                          append = TRUE,
                          row.names = FALSE)
      }
      
      if(nRightProjItems != 0){
        # reduce to projection items
        qStr=paste0("SELECT ",
                    " i.db_uuid, ",
                    " i.session, ",
                    " i.bundle, ",
                    " i.l_seq_start_id AS seq_start_id, ",
                    " i.r_seq_end_id AS seq_end_id, ",
                    " pi.p_seq_start_id, ",
                    " pi.p_seq_end_id, ",
                    " pi.p_seq_len, ",
                    " pi.p_level, ",
                    " pi.p_attribute, ",
                    " pi.p_seq_start_seq_idx, ",
                    " pi.p_seq_end_seq_idx ",
                    "FROM lr_exp_res_tmp AS i, ",
                    " interm_res_proj_items_tmp_", rightTableSuffix, " AS pi ",
                    "WHERE i.db_uuid = pi.db_uuid ",
                    " AND i.session = pi.session ",
                    " AND i.bundle = pi.bundle ",
                    " AND i.r_seq_start_id = pi.seq_start_id ",
                    " AND i.r_seq_end_id = pi.seq_end_id")
        
        reducedPI = DBI::dbGetQuery(emuDBhandle$connection, qStr)
        DBI::dbExecute(emuDBhandle$connection, 
                       paste0("DELETE FROM interm_res_proj_items_tmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, 
                          paste0("interm_res_proj_items_tmp_", intermResTableSuffix), 
                          reducedPI, 
                          append = TRUE,
                          row.names = FALSE)
      }
      
      # place result in correct table
      resItems = DBI::dbGetQuery(emuDBhandle$connection, 
                                 paste0("SELECT DISTINCT ",
                                        " db_uuid, ",
                                        " session, ",
                                        " bundle, ",
                                        " l_seq_start_id AS seq_start_id, ",
                                        " r_seq_end_id AS seq_end_id, ",
                                        " l_seq_len + r_seq_len AS seq_len, ",
                                        " l_level AS level, ",
                                        " l_attribute AS attribute, ",
                                        " l_seq_start_seq_idx AS seq_start_seq_idx, ",
                                        " r_seq_end_seq_idx AS seq_end_seq_idx ",
                                        "FROM lr_exp_res_tmp"))
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM interm_res_items_tmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, 
                        paste0("interm_res_items_tmp_", intermResTableSuffix), 
                        resItems, 
                        append = TRUE,
                        row.names = FALSE)
    }
    return()
  }else{
    query_databaseWithEql(emuDBhandle,
                          qTrim,
                          sessionPattern,
                          bundlePattern,
                          intermResTableSuffix,
                          leftRightTableNrCounter,
                          verbose = verbose)
  }
}

###################
query_databaseWithEql <- function(emuDBhandle, 
                                  query, 
                                  sessionPattern, 
                                  bundlePattern, 
                                  intermResTableSuffix, 
                                  leftRightTableNrCounter, 
                                  verbose){
  parseRes = list()
  qTrim = stringr::str_trim(query)
  brOpenPos = get_charPosition(qTrim, '[', literalQuote = "'")
  if(brOpenPos == -1){
    query_databaseEqlCONJQ(emuDBhandle, 
                           qTrim, 
                           sessionPattern, 
                           bundlePattern, 
                           intermResTableSuffix = intermResTableSuffix,
                           verbose)
    return()
  }else{
    
    brClosePos = get_lastCharPosition(qTrim, ']', literalQuote = "'")
    if(brClosePos == -1){
      stop("Syntax error: missing close bracket ']' for open bracket at pos ",
           brOpenPos,
           "\n")
    }
    
    if(brOpenPos != 1){
      stop("Syntax error: Expected open bracket '[' at the beginning\n")
    }
    
    if(brClosePos != nchar(qTrim)){
      stop("Syntax error: Expected close bracket ']' at the end\n")
    }
    
    #parse string in bracket
    inBr = substr(qTrim, brOpenPos + 1, brClosePos - 1)
    query_databaseEqlInBracket(emuDBhandle, 
                               inBr, 
                               sessionPattern, 
                               bundlePattern, 
                               intermResTableSuffix, 
                               leftRightTableNrCounter, 
                               verbose = verbose)
    return()
    
  }
  stop("Unknown syntax error.")
}

####################
query_databaseWithEqlEmusegs <- function(emuDBhandle, 
                                         query, 
                                         sessionPattern, 
                                         bundlePattern, 
                                         timeRefSegmentLevel, 
                                         calcTimes, 
                                         verbose){
  # create "root" intermediate result tables
  create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = "root")
  # query emuDB
  query_databaseWithEql(emuDBhandle, 
                        query, 
                        sessionPattern, 
                        bundlePattern, 
                        intermResTableSuffix = "root", 
                        leftRightTableNrCounter = 0, 
                        verbose)
  # escape singel quotes
  query = gsub("'", "''", query)
  
  emusegs = convert_queryResultToEmusegs(emuDBhandle, 
                                         timeRefSegmentLevel, 
                                         sessionPattern,
                                         bundlePattern,
                                         query,
                                         calcTimes, 
                                         verbose)
  return(emusegs)
  
}

####################
query_databaseWithEqlEmuRsegs <- function(emuDBhandle, 
                                          query, 
                                          sessionPattern, 
                                          bundlePattern, 
                                          timeRefSegmentLevel, 
                                          calcTimes, 
                                          verbose){
  # create "root" intermediate result tables
  create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = "root")
  
  # query emuDB
  query_databaseWithEql(emuDBhandle, 
                        query, 
                        sessionPattern, 
                        bundlePattern, 
                        intermResTableSuffix = "root", 
                        leftRightTableNrCounter = 0, 
                        verbose = verbose)
  # escape single quotes
  queryStr = gsub("'", "''", query)
  emuRsegs = convert_queryResultToEmuRsegs(emuDBhandle, 
                                           timeRefSegmentLevel, 
                                           sessionPattern, 
                                           bundlePattern, 
                                           queryStr = queryStr, 
                                           calcTimes = calcTimes, 
                                           verbose = verbose)
  return(emuRsegs)
  
}


##' Query emuDB
##' @description Function to query annotation items/structures in an emuDB
##' @details Evaluates a query string of query language queryLang on an 
##' emuDB referenced by \code{emuDBhandle} and returns a segment list of the desired type resultType.  
##' For details of the query language please refer to the EMU-SDMS manual's query 
##' system chapter (\url{https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-querysys.html}).
##' This function extracts a list of segments which meet the conditions given by the query string. 
##' A segment can consist of one (e.g. 's') or more (e.g. 's->t') items from 
##' the specified emuDB level. Segment objects (type 'SEGMENT') contain the label 
##' string and the start and end time information of the segment (in ms). 
##' The \code{tibble} return type (now the defaults) objects additionally contain 
##' sample position of start and end item. 
##' Time information of symbolic elements (type 'ITEM') are derived from linked SEGMENT 
##' levels if available. If multiple linked SEGMENT levels exist, you can specify the 
##' level with the \code{timeRefSegmentLevel} argument. If time and sample values cannot be 
##' derived they will be set to \code{\link{NA}}. \link[tibble]{tibble}s will 
##' be ordered by the columns UUID, session, bundle and sequence index (seq_idx). 
##' Legacy \link{emusegs} lists are ordered by the columns utts and start.
##' The query may be limited to session and/or bundle names specified by regular 
##' expression pattern strings (see \link{regex}) in parameters \code{sessionPattern} 
##' respectively \code{bundlePattern}.
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param query string (see vignette \url{https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-querysys.html})
##' @param sessionPattern A regular expression pattern matching session names to 
##' be searched from the database
##' @param bundlePattern A regular expression pattern matching bundle names to be 
##' searched from the database
##' @param queryLang query language used for evaluating the query string 
##' @param timeRefSegmentLevel set time segment level from which to derive time 
##' information. It is only necessary to set this parameter if more than one child 
##' level contains time information and the queried parent level is of type ITEM.
##' @param resultType type (class name) of result (either 'tibble', 'emuRsegs' or 
##' 'emusegs' (use 'emusegs' for legacy compatablility only))
##' @param calcTimes calculate times for resulting segments (results in 
##' \code{NA} values for start and end times in emuseg/emuRsegs). As it can be 
##' very computationally expensive to 
##' calculate the times for large nested hierarchies, it can be turned off via this 
##' parameter.
##' @param verbose be verbose. Set this to \code{TRUE} if you wish to choose which 
##' path to traverse on intersecting hierarchies. If set to \code{FALSE} (the default) 
##' all paths will be traversed (= legacy EMU behavior).
##' @return result set object of class resultType (default: \link[tibble]{tibble}, 
##' compatible to legacy types \link{emuRsegs} and \link{emusegs})
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
##' ## Query database ae with EQL query "[Phonetic=t -> Phonetic=s]":
##' ## 'Find all sequences /ts/ on the level named Phonetics'.
##' ## and store result seglist in variable segListTs
##' 
##' seglistTs = query(ae, "[Phonetic == t -> Phonetic == s]")
##' 
##' ## Query database ae with EQL query "[Syllable == S ^ Phoneme == t]":
##' ## 'Find all items 't' on the level named Phoneme that are dominated by 
##' ## items 'S' in level Syllable.'
##' ## Return legacy Emu result type 'emusegs'
##' 
##' query(ae, "[Syllable == S ^ Phoneme == t]", resultType = "emusegs")
##' 
##' ## Query 'p' items on the level named Phoneme from bundles whose 
##' ## bundle names start with 'msajc07' 
##' ## and whose session names start with '00'
##' ## (Note that here the query uses the operator '=' (meaning '==') 
##' ## which is kept for backwards compatibilty to EQL1.)  
##' 
##' query(ae, "Phoneme = p", bundlePattern = "msajc05.*", sessionPattern = "00.*")
##' 
##' }
##' 
query <- function(emuDBhandle, 
                  query, 
                  sessionPattern = '.*', 
                  bundlePattern = '.*', 
                  queryLang = 'EQL2', 
                  timeRefSegmentLevel = NULL, 
                  resultType = "tibble", 
                  calcTimes = TRUE, 
                  verbose = FALSE){
  
  check_emuDBhandle(emuDBhandle)
  
  if(queryLang=='EQL2'){
    # create temp tables 
    drop_allTmpTablesDBI(emuDBhandle)
    create_tmpFilteredQueryTablesDBI(emuDBhandle)
    
    if(is.null(resultType)){
      emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,
                                               query,
                                               sessionPattern, 
                                               bundlePattern, 
                                               timeRefSegmentLevel, 
                                               calcTimes, 
                                               verbose = verbose)
      drop_allTmpTablesDBI(emuDBhandle)
      return(emuRsegs)
    }else{
      if(resultType == 'emuRsegs'){
        emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,
                                                 query,
                                                 sessionPattern, 
                                                 bundlePattern, 
                                                 timeRefSegmentLevel, 
                                                 calcTimes, 
                                                 verbose)
        drop_allTmpTablesDBI(emuDBhandle)
        return(emuRsegs)
      }else if(resultType == 'emusegs'){
        if(!is.null(timeRefSegmentLevel)){
          # TODO 
          stop("Parameter timeRefSegmentLevel not yet supported for",
               " resultType 'emusegs'. Please use resultType 'tibble' (the default).")
        }
        return(query_databaseWithEqlEmusegs(emuDBhandle, 
                                            query, 
                                            sessionPattern, 
                                            bundlePattern, 
                                            timeRefSegmentLevel, 
                                            calcTimes, 
                                            verbose))
      }else if(resultType == 'tibble'){
        emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,
                                                 query,
                                                 sessionPattern, 
                                                 bundlePattern, 
                                                 timeRefSegmentLevel, 
                                                 calcTimes, 
                                                 verbose)
        
        res_tibble = convert_queryEmuRsegsToTibble(emuDBhandle, emuRsegs)
        drop_allTmpTablesDBI(emuDBhandle)
        return(res_tibble)
      }else{
        stop("Unknown result type: '",
             resultType,
             "'. Supported result types: 'emuRsegs', 'emusegs' or 'tibble'")
      }
    }
    
  }else{
    stop("Unknown query language '",queryLang,"'.")
  }
}
