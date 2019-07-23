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
                                          " l_seq_start_seq_idx INTEGER,",
                                          " l_seq_end_seq_idx INTEGER,",
                                          " r_seq_start_id INTEGER,",
                                          " r_seq_end_id INTEGER,",
                                          " r_seq_len INTEGER,",
                                          " r_level TEXT,",
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
  
  database.DDL.emuDB_intermRes_metaInfosTmp = paste0("CREATE TEMP TABLE interm_res_meta_infos_tmp_", suffix, " (",
                                                     " result_level TEXT,",
                                                     " projection_attr_level TEXT,",
                                                     " query_str TEXT",
                                                     ");")
  
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
                         paste0("interm_res_meta_infos_tmp_", suffix))){
    DBI::dbExecute(emuDBhandle$connection, 
                   database.DDL.emuDB_intermRes_metaInfosTmp)
  }else{
    DBI::dbExecute(emuDBhandle$connection, 
                   paste0("DELETE FROM interm_res_meta_infos_tmp_", suffix))
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
  DBI::dbExecute(emuDBhandle$connection, 
                 paste0("DELETE FROM interm_res_meta_infos_tmp_", intermResTableSuffix))
  if(clearProjectionItems) DBI::dbExecute(emuDBhandle$connection, 
                                          paste0("DELETE FROM interm_res_proj_items_tmp_", intermResTableSuffix))
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
  m = regexpr(pattern, x, perl = T)
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
                         levelName, 
                         intermResTableSuffix, 
                         conditionText, 
                         useSubsets, 
                         filteredTablesSuffix){
  
  if(useSubsets){
    labelTableName = "labels_filtered_subset_tmp"
  }else{
    labelTableName = paste0("labels", 
                            filteredTablesSuffix)
  }
  
  itemTableName = paste0("items",
                         filteredTablesSuffix)
  
  # clear tables but keep projectionItems so they don't
  # get lost in queries like : [Text == the -> #Text =~ .* & Accent == S]  (right side of ->)
  clear_intermResTabels(emuDBhandle, 
                        intermResTableSuffix, 
                        clearProjectionItems = FALSE)
  
  opr = conditionText[['opr']]
  values = conditionText[['values']]
  res = NULL
  if(opr == '==' | opr == '='){
    for(value in values){
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", intermResTableSuffix, " ", 
                                                    "SELECT ",
                                                    " it.db_uuid, ",
                                                    " it.session, ",
                                                    " it.bundle, ",
                                                    " it.item_id AS seq_start_id, ",
                                                    " it.item_id AS seq_end_id, ",
                                                    " 1 AS seq_len,'", levelName, "' AS level, ",
                                                    " it.seq_idx AS seq_start_seq_idx, ",
                                                    " it.seq_idx AS seq_end_seq_idx ", 
                                                    "FROM ", itemTableName, " AS it, ", 
                                                    labelTableName, " AS lt ",
                                                    "WHERE it.db_uuid = lt.db_uuid ",
                                                    " AND it.session = lt.session ",
                                                    " AND it.bundle = lt.bundle ",
                                                    " AND it.item_id = lt.item_id ",
                                                    " AND lt.name = '", levelName, "' ",
                                                    " AND lt.label = '", value, "' ",
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
                    " 1 AS seq_len,'", 
                    levelName, "' AS level, ",
                    " it.seq_idx AS seq_start_seq_idx, ",
                    " it.seq_idx AS seq_end_seq_idx ",
                    "FROM ", itemTableName, " AS it, ", 
                    labelTableName, " AS lt ",
                    "WHERE it.db_uuid = lt.db_uuid ",
                    " AND it.session = lt.session ",
                    " AND it.bundle = lt.bundle ",
                    " AND it.item_id = lt.item_id ",
                    " AND name = '", levelName, "'")
    for(value in values){
      sqlStr = paste0(sqlStr, " AND label <> '", value, "'")
    }
    
    DBI::dbExecute(emuDBhandle$connection, sqlStr)
    
  }else if(opr == '=~'){
    for(value in values){
      sqlStr = paste0("SELECT ",
                      " it.db_uuid, ",
                      " it.session, ",
                      " it.bundle, ",
                      " it.item_id AS seq_start_id, ",
                      " it.item_id AS seq_end_id, ",
                      " 1 AS seq_len,'", 
                      levelName, "' AS level, ",
                      " it.seq_idx AS seq_start_seq_idx, ",
                      " it.seq_idx AS seq_end_seq_idx, ",
                      " lt.label ",
                      "FROM ", itemTableName, " AS it, ", 
                      labelTableName, " AS lt ",
                      "WHERE it.db_uuid = lt.db_uuid ",
                      " AND it.session = lt.session ",
                      " AND it.bundle = lt.bundle ",
                      " AND it.item_id = lt.item_id ",
                      " AND name = '", levelName, "'")
      
      ldf = DBI::dbGetQuery(emuDBhandle$connection, sqlStr)
      ssl = emuR_regexprl(value,ldf[['label']])
      res = ldf[ssl,]
      DBI::dbWriteTable(emuDBhandle$connection, 
                        paste0("interm_res_items_tmp_", intermResTableSuffix), 
                        subset(res, select = -label), 
                        append = T, 
                        row.names = F) # label column is ignored by DBI::dbWriteTable
      
    }
  }else if(opr == '!~'){
    for(value in values){
      sqlStr = paste0("SELECT ",
                      " it.db_uuid, ",
                      " it.session, ",
                      " it.bundle, ",
                      " it.item_id AS seq_start_id, ",
                      " it.item_id AS seq_end_id, ",
                      " 1 AS seq_len,'", levelName, "' AS level, ",
                      " it.seq_idx AS seq_start_seq_idx, ",
                      " it.seq_idx AS seq_end_seq_idx, ",
                      " lt.label ",
                      "FROM ", itemTableName, " AS it, ", 
                      labelTableName, " AS lt ",
                      "WHERE it.db_uuid = lt.db_uuid ",
                      " AND it.session = lt.session ",
                      " AND it.bundle = lt.bundle ",
                      " AND it.item_id = lt.item_id ",
                      " AND name = '", levelName, "'")
      
      ldf = DBI::dbGetQuery(emuDBhandle$connection, sqlStr)
      ssl = !emuR_regexprl(value,ldf[['label']])
      res = ldf[ssl,]
      DBI::dbWriteTable(emuDBhandle$connection, 
                        paste0("interm_res_items_tmp_", intermResTableSuffix), 
                        subset(res, select = -label), 
                        append = T, 
                        row.names = F) # label column is ignored by DBI::dbWriteTable
    }
  }else{
    stop("Syntax error: Unknown operator: '",
         opr,
         "'\n")
  }
  # clear insert result_level
  DBI::dbExecute(emuDBhandle$connection, 
                 paste0("DELETE FROM interm_res_meta_infos_tmp_", intermResTableSuffix))
  DBI::dbExecute(emuDBhandle$connection, 
                 paste0("INSERT INTO interm_res_meta_infos_tmp_", intermResTableSuffix, " (result_level) ",
                        "VALUES ('", levelName, "')"))
}

##############################
query_databaseEqlFUNCQ <- function(emuDBhandle, 
                                   q, 
                                   intermResTableSuffix, 
                                   useSubsets, 
                                   filteredTablesSuffix, 
                                   verbose){
  # EBNF: FUNCQ = POSQ | NUMQ;
  qTrim = stringr::str_trim(q)
  if(useSubsets){
    itemsTableName = "items_filtered_subset_tmp"
  }else{
    itemsTableName = paste0("items", filteredTablesSuffix)
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
                                                    " item_id AS seq_start_seq_idx, ",
                                                    " item_id AS seq_end_seq_idx ",
                                                    "FROM items", filteredTablesSuffix, " ",
                                                    "WHERE db_uuid ='", emuDBhandle$UUID, "' ",
                                                    " AND level = '", level1, "'"))
      
      # place all level2 items into temp table
      level2ItemsTableSuffix = "funcq_level2_items"
      create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = level2ItemsTableSuffix)
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_items_tmp_", level2ItemsTableSuffix, " ",
                                                    "SELECT ",
                                                    " db_uuid, ",
                                                    " session, ",
                                                    " bundle, ",
                                                    " item_id AS start_item_id, ",
                                                    " item_id AS seq_end_id, ",
                                                    " 1 AS seq_len, ",
                                                    " level, ",
                                                    " item_id AS seq_start_seq_idx, ",
                                                    " item_id AS seq_end_seq_idx ",
                                                    "FROM items", filteredTablesSuffix, " ",
                                                    "WHERE db_uuid ='", emuDBhandle$UUID, "' ",
                                                    " AND level = '", level2, "'"))
      
      
      
      query_databaseHier(emuDBhandle, 
                         firstLevelName = level1, 
                         secondLevelName = level2, 
                         leftTableSuffix = level1ItemsTableSuffix, 
                         rightTableSuffix = level2ItemsTableSuffix, 
                         filteredTablesSuffix = filteredTablesSuffix, 
                         verbose = verbose) # result written to lr_exp_res_tmp table
      
      
      # create temp table to insert 
      DBI::dbExecute(emuDBhandle$connection,paste0("CREATE TEMP TABLE IF NOT EXISTS seq_idx_tmp ( ",
                                                   " db_uuid VARCHAR(36), ",
                                                   " session TEXT, ",
                                                   " bundle TEXT, ",
                                                   " level TEXT,",
                                                   " min_seq_idx INTEGER, ",
                                                   " max_seq_idx INTEGER, ",
                                                   " parent_item_id INTEGER",
                                                   ")"))
      
      
      DBI::dbExecute(emuDBhandle$connection,paste0("CREATE TEMP TABLE IF NOT EXISTS items_as_seqs_tmp ( ",
                                                   " db_uuid VARCHAR(36), ",
                                                   " session TEXT, ",
                                                   " bundle TEXT, ",
                                                   " seq_start_id INTEGER, ",
                                                   " seq_end_id INTEGER, ", 
                                                   " seq_len INTEGER, ", 
                                                   " level TEXT",
                                                   ")"))
      
      # first step in two step process to group by and get seq min/max
      # then extract according item_id
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO seq_idx_tmp ",
                                                    "SELECT ",
                                                    " lr.db_uuid, ",
                                                    " lr.session, ",
                                                    " lr.bundle, ",
                                                    " r_level AS level, ",
                                                    " min(i_start.seq_idx) AS min_seq_idx, ",
                                                    " max(i_end.seq_idx) AS max_seq_idx, ",
                                                    " l_seq_start_id AS parent_item_id ",
                                                    "FROM lr_exp_res_tmp AS lr, items AS i_start, items AS i_end ",
                                                    "WHERE lr.db_uuid = i_start.db_uuid ",
                                                    " AND lr.session = i_start.session ",
                                                    " AND lr.bundle = i_start.bundle ",
                                                    " AND lr.r_seq_start_id = i_start.item_id ",
                                                    " AND lr.db_uuid = i_end.db_uuid ",
                                                    " AND lr.session = i_end.session ",
                                                    " AND lr.bundle = i_end.bundle ",
                                                    " AND lr.r_seq_end_id = i_end.item_id ",
                                                    "GROUP BY ",
                                                    " lr.db_uuid, ",
                                                    " lr.session, ",
                                                    " lr.bundle, ",
                                                    " lr.l_seq_start_id, ",
                                                    " lr.l_seq_end_id",
                                                    ""))
      
      
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
                                                        " sit.db_uuid, ",
                                                        " sit.session, ",
                                                        " sit.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " sit.level AS level  ",
                                                        "FROM seq_idx_tmp AS sit, ", itemsTableName, " AS i1 ",
                                                        "WHERE sit.db_uuid = i1.db_uuid ",
                                                        " AND sit.session = i1.session ",
                                                        " AND sit.bundle = i1.bundle ",
                                                        " AND sit.level = i1.level ",
                                                        " AND sit.min_seq_idx < i1.seq_idx ",
                                                        " AND sit.max_seq_idx >= i1.seq_idx ",
                                                        ""))
          
        }else if(funcValue == '1' | funcValue == 'T' | funcValue == 'TRUE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " sit.db_uuid, ",
                                                        " sit.session, ",
                                                        " sit.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " sit.level AS level  ",
                                                        "FROM seq_idx_tmp AS sit, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE sit.db_uuid = i1.db_uuid ",
                                                        " AND sit.session = i1.session ",
                                                        " AND sit.bundle = i1.bundle ",
                                                        " AND sit.level = i1.level ",
                                                        " AND sit.min_seq_idx = i1.seq_idx ",
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
                                                        " sit.db_uuid, ",
                                                        " sit.session, ",
                                                        " sit.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " sit.level AS level ",
                                                        "FROM seq_idx_tmp AS sit, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE (",
                                                        " sit.db_uuid = i1.db_uuid ",
                                                        " AND sit.session = i1.session ",
                                                        " AND sit.bundle = i1.bundle ",
                                                        " AND sit.level = i1.level ",
                                                        " AND i1.seq_idx = sit.min_seq_idx) ",
                                                        "OR (sit.db_uuid = i1.db_uuid ",
                                                        " AND sit.session = i1.session ",
                                                        " AND sit.bundle = i1.bundle ",
                                                        " AND sit.level = i1.level ",
                                                        " AND i1.seq_idx = sit.max_seq_idx)",
                                                        ""))
          
        }else if(funcValue == '1' | funcValue == 'T' | funcValue == 'TRUE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " sit.db_uuid, ",
                                                        " sit.session, ",
                                                        " sit.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " sit.level AS level  ",
                                                        "FROM seq_idx_tmp AS sit, ", itemsTableName, " AS i1 ",
                                                        "WHERE sit.db_uuid = i1.db_uuid ",
                                                        " AND sit.session = i1.session ",
                                                        " AND sit.bundle = i1.bundle ",
                                                        " AND sit.level = i1.level ",
                                                        " AND i1.seq_idx > sit.min_seq_idx ",
                                                        " AND i1.seq_idx < sit.max_seq_idx ",
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
                                                        " sit.db_uuid, ",
                                                        " sit.session, ",
                                                        " sit.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " sit.level AS level  ",
                                                        "FROM seq_idx_tmp AS sit, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE sit.db_uuid = i1.db_uuid ",
                                                        " AND sit.session = i1.session ",
                                                        " AND sit.bundle = i1.bundle ",
                                                        " AND sit.level = i1.level ",
                                                        " AND i1.seq_idx >= sit.min_seq_idx ",
                                                        " AND i1.seq_idx < sit.max_seq_idx ",
                                                        ""))
          
        }else if(funcValue == '1'  | funcValue == 'T' | funcValue == 'TRUE'){
          #extract according items
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                        "SELECT ",
                                                        " sit.db_uuid, ",
                                                        " sit.session, ",
                                                        " sit.bundle, ",
                                                        " i1.item_id AS seq_start_id, ",
                                                        " i1.item_id AS seq_end_id, ",
                                                        " 1 AS seq_len, ",
                                                        " sit.level AS level ",
                                                        "FROM seq_idx_tmp AS sit, ", 
                                                        itemsTableName, " AS i1 ",
                                                        "WHERE sit.db_uuid = i1.db_uuid ",
                                                        " AND sit.session = i1.session ",
                                                        " AND sit.bundle = i1.bundle ",
                                                        " AND sit.level = i1.level ",
                                                        " AND sit.max_seq_idx = i1.seq_idx ",
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
        # NOTE: return value level is param1 here
        DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO items_as_seqs_tmp ",
                                                      "SELECT ",
                                                      " sit.db_uuid, ",
                                                      " sit.session, ",
                                                      " sit.bundle, ",
                                                      " i1.item_id AS seq_start_id, ",
                                                      " i1.item_id AS seq_end_id, ",
                                                      " 1 AS seq_len, ",
                                                      " '", param1, "' AS level ",
                                                      "FROM seq_idx_tmp AS sit, ", 
                                                      itemsTableName, " AS i1 ",
                                                      "WHERE sit.db_uuid = i1.db_uuid ",
                                                      " AND sit.session = i1.session ",
                                                      " AND sit.bundle = i1.bundle ",
                                                      " AND sit.parent_item_id = i1.item_id ",
                                                      " AND (sit.max_seq_idx - sit.min_seq_idx) + 1 ", 
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
                            " it.seq_idx AS seq_start_seq_idx, ",
                            " it.seq_idx AS seq_end_seq_idx ", 
                            "FROM items_as_seqs_tmp AS iast, ", 
                            itemsTableName, " AS it ", 
                            "WHERE iast.db_uuid = it.db_uuid ",
                            " AND iast.session = it.session ",
                            " AND iast.bundle = it.bundle ",
                            " AND iast.seq_start_id = it.item_id ",
                            ""))
      # move meta infos to correct table
      DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM interm_res_meta_infos_tmp_", 
                                                    intermResTableSuffix))
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO interm_res_meta_infos_tmp_", intermResTableSuffix, " ",
                                                    "VALUES (",
                                                    " '", resultLevel, "', ",
                                                    " NULL, ",
                                                    " '", qTrim, "' ",
                                                    ")"))
      
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
                                    useSubsets, 
                                    intermResTableSuffix, 
                                    filteredTablesSuffix){
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
      lvlTrim = stringr::str_trim(level)
      lvlName = lvlTrim
      if(grepl('^#', lvlTrim)){
        # projection marker
        # the EBNF does not allow white space between '#' and level string
        # but the implementation of Emu does, so we allow it here too
        
        lvlName = stringr::str_trim(substring(lvlTrim, 2))
        projectionLevel = TRUE
      }
      aNms = get_allAttributeNames(emuDBhandle)
      if(! (lvlName %in% aNms)){
        stop("Unknown level attribute name: '",
             lvlName,
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
              if(lvlName == attrDef[['name']]){
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
                   levelName = lvlName, 
                   intermResTableSuffix = intermResTableSuffix, 
                   cond, 
                   useSubsets, 
                   filteredTablesSuffix)
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
                                                      " seq_start_seq_idx AS p_seq_start_seq_idx, ",
                                                      " seq_end_seq_idx AS p_seq_end_seq_idx ",
                                                      "FROM interm_res_items_tmp_", intermResTableSuffix))
        
        DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE interm_res_meta_infos_tmp_", intermResTableSuffix, " ",
                                                      "SET projection_attr_level = '", lvlName, "'"))
      }
      return()
    }
  }
  stop("Syntax error: No operator found.")
}

query_databaseEqlSQ <- function(emuDBhandle, 
                                q, 
                                intermResTableSuffix, 
                                useSubsets, 
                                filteredTablesSuffix, 
                                verbose){
  # EBNF: SQ = LABELQ | FUNCQ;
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
                             useSubsets, 
                             filteredTablesSuffix, 
                             verbose = verbose)
    }
  }else{
    # No round brackets, assuming a level query
    query_databaseEqlLABELQ(emuDBhandle, 
                            qTrim, 
                            useSubsets, 
                            intermResTableSuffix = intermResTableSuffix, 
                            filteredTablesSuffix)
  }
}

query_databaseEqlCONJQ <- function(emuDBhandle, 
                                   q, 
                                   intermResTableSuffix, 
                                   filteredTablesSuffix, 
                                   verbose){
  # EBNF: CONJQ = SQ,{'&',SQ};
  qTrim = stringr::str_trim(q)
  conditions = list()
  # initialize with empty result
  startPos = 1
  p = 0
  resultLevel = NULL
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
                        intermResTableSuffix, 
                        useSubsets = useSubsets, 
                        filteredTablesSuffix, 
                        verbose = verbose)
    # set resultLevel of first term
    if(is.null(resultLevel)){
      termResLevel = DBI::dbGetQuery(emuDBhandle$connection, 
                                     paste0("SELECT * FROM interm_res_meta_infos_tmp_", intermResTableSuffix))$result_level
      if(!is.null(termResLevel)){
        resultLevel = termResLevel
      }
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
                                                    "FROM items", filteredTablesSuffix, " i, ", 
                                                    " interm_res_items_tmp_", intermResTableSuffix, " imr ",
                                                    "WHERE i.db_uuid = imr.db_uuid ",
                                                    " AND i.session = imr.session ",
                                                    " AND i.bundle = imr.bundle ",
                                                    " AND i.item_id = imr.seq_start_id"))
      
      DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO labels_filtered_subset_tmp ",
                                                    "SELECT DISTINCT l.* ",
                                                    "FROM labels", filteredTablesSuffix, " l, ",
                                                    " interm_res_items_tmp_", intermResTableSuffix, " imr ",
                                                    "WHERE l.db_uuid = imr.db_uuid ",
                                                    " AND l.session = imr.session ",
                                                    " AND l.bundle = imr.bundle ",
                                                    " AND l.item_id = imr.seq_start_id"))
      
      useSubsets = TRUE
      
    }
  }
  
  DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE interm_res_items_tmp_", intermResTableSuffix, " ",
                                                "SET level ='", resultLevel, "'"))
  DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE interm_res_meta_infos_tmp_", intermResTableSuffix, " ", 
                                                "SET result_level = '", resultLevel, "'"))
}

# reduces the results stored in hier_left/right_trapeze_interm_res tables to min and max seq_idx of leafs
reduce_hierTrapezeIntermRes_minMaxSeqIdx <- function(emuDBhandle){
  # reduce hier_left_trapeze_interm_res_tmp to leftest leaf per parent group
  hltirt_tmp = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT hltirt1.* ",
                                                              "FROM  hier_left_trapeze_interm_res_tmp AS hltirt1 ",
                                                              "LEFT OUTER JOIN hier_left_trapeze_interm_res_tmp AS hltirt2 ",
                                                              " ON hltirt1.db_uuid = hltirt2.db_uuid ",
                                                              " AND hltirt1.session = hltirt2.session ",
                                                              " AND hltirt1.bundle = hltirt2.bundle ",
                                                              " AND hltirt1.seq_start_id = hltirt2.seq_start_id ",
                                                              " AND hltirt1.seq_end_id = hltirt2.seq_end_id ",
                                                              " AND hltirt1.seq_start_seq_idx_leaf > hltirt2.seq_start_seq_idx_leaf ",
                                                              "WHERE hltirt2.db_uuid IS NULL",
                                                              ""))
  
  DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_left_trapeze_interm_res_tmp")
  
  DBI::dbWriteTable(emuDBhandle$connection, "hier_left_trapeze_interm_res_tmp", 
                    hltirt_tmp, 
                    append = T, 
                    row.names = F)
  
  # reduce hier_right_trapeze_interm_res_tmp to rightest leaf per parent group
  hrtirt_tmp = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT * ",
                                                              "FROM  hier_right_trapeze_interm_res_tmp AS hrtirt1 ",
                                                              "LEFT OUTER JOIN hier_right_trapeze_interm_res_tmp AS hrtirt2 ",
                                                              " ON hrtirt1.db_uuid = hrtirt2.db_uuid ",
                                                              " AND hrtirt1.session = hrtirt2.session ",
                                                              " AND hrtirt1.bundle = hrtirt2.bundle ",
                                                              " AND hrtirt1.seq_start_id = hrtirt2.seq_start_id ",
                                                              " AND hrtirt1.seq_end_id = hrtirt2.seq_end_id ",
                                                              " AND hrtirt1.seq_end_seq_idx_leaf < hrtirt2.seq_end_seq_idx_leaf ",
                                                              "WHERE hrtirt2.db_uuid IS NULL",
                                                              ""))
  
  DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_right_trapeze_interm_res_tmp")
  
  DBI::dbWriteTable(emuDBhandle$connection, "hier_right_trapeze_interm_res_tmp", 
                    hltirt_tmp, 
                    append = T, 
                    row.names = F)
  
}

##########################
query_databaseHier <- function(emuDBhandle, 
                               firstLevelName, 
                               secondLevelName, 
                               leftTableSuffix, 
                               rightTableSuffix, 
                               filteredTablesSuffix, 
                               minMaxSeqIdxLeafOnly = F, 
                               preserveLeafLength = F,
                               preserveAnchorLength = F,
                               verbose = F) {
  
  # create temp tables for hier query (should maybe be moved to external functions)
  hier_left_trapeze_interm_res_tmp = paste0("CREATE TEMP TABLE IF NOT EXISTS hier_left_trapeze_interm_res_tmp (",
                                            " db_uuid VARCHAR(36),",
                                            " session TEXT,",
                                            " bundle TEXT,",
                                            " seq_start_id INTEGER,",
                                            " seq_end_id INTEGER,",
                                            " seq_len INTEGER,",
                                            " level TEXT,",
                                            " seq_start_seq_idx INTEGER,",
                                            " seq_end_seq_idx INTEGER,",
                                            " db_uuid_leaf VARCHAR(36),",
                                            " session_leaf TEXT,",
                                            " bundle_leaf TEXT,",
                                            " seq_start_id_leaf INTEGER,",
                                            " seq_end_id_leaf INTEGER,",
                                            " seq_len_leaf INTEGER,",
                                            " level_leaf TEXT,",
                                            " seq_start_seq_idx_leaf INTEGER,",
                                            " seq_end_seq_idx_leaf INTEGER,",
                                            " PRIMARY KEY (",
                                            "  db_uuid, ",
                                            "  session, ",
                                            "  bundle, ",
                                            "  seq_start_id, ",
                                            "  seq_end_id, ",
                                            "  db_uuid_leaf, ",
                                            "  session_leaf, ",
                                            "  bundle_leaf, ",
                                            "  seq_start_id_leaf, ",
                                            "  seq_end_id_leaf)",
                                            ");")
  
  hier_right_trapeze_interm_res_tmp = paste0("CREATE TEMP TABLE IF NOT EXISTS hier_right_trapeze_interm_res_tmp (",
                                             " db_uuid VARCHAR(36),",
                                             " session TEXT,",
                                             " bundle TEXT,",
                                             " seq_start_id INTEGER,",
                                             " seq_end_id INTEGER,",
                                             " seq_len INTEGER,",
                                             " level TEXT,",
                                             " seq_start_seq_idx INTEGER,",
                                             " seq_end_seq_idx INTEGER,",
                                             " db_uuid_leaf VARCHAR(36),",
                                             " session_leaf TEXT,",
                                             " bundle_leaf TEXT,",
                                             " seq_start_id_leaf INTEGER,",
                                             " seq_end_id_leaf INTEGER,",
                                             " seq_len_leaf INTEGER,",
                                             " level_leaf TEXT,",
                                             " seq_start_seq_idx_leaf INTEGER,",
                                             " seq_end_seq_idx_leaf INTEGER,",
                                             "PRIMARY KEY (",
                                             "  db_uuid, ",
                                             "  session, ",
                                             "  bundle, ",
                                             "  seq_start_id, ",
                                             "  seq_end_id, ",
                                             "  db_uuid_leaf, ",
                                             "  session_leaf, ",
                                             "  bundle_leaf, ",
                                             "  seq_start_id_leaf, ",
                                             "  seq_end_id_leaf)",
                                             ");")
  
  
  hier_left_trapeze_interm_res_tmp_idx = paste0("CREATE INDEX IF NOT EXISTS hier_left_trapeze_interm_res_tmp_idx ",
                                                "ON hier_left_trapeze_interm_res_tmp (",
                                                " db_uuid, ",
                                                " session, ",
                                                " bundle, ",
                                                " seq_start_id, ",
                                                " seq_end_id,",
                                                " seq_len, ",
                                                " level",
                                                ")")
  hier_right_trapeze_interm_res_tmp_idx = paste0("CREATE INDEX IF NOT EXISTS hier_right_trapeze_interm_res_tmp_idx ",
                                                 "ON hier_right_trapeze_interm_res_tmp (",
                                                 " db_uuid,",
                                                 " session,",
                                                 " bundle,",
                                                 " seq_start_id",
                                                 ")")
  DBI::dbExecute(emuDBhandle$connection, hier_left_trapeze_interm_res_tmp)
  DBI::dbExecute(emuDBhandle$connection, hier_left_trapeze_interm_res_tmp_idx)
  DBI::dbExecute(emuDBhandle$connection, hier_right_trapeze_interm_res_tmp)
  DBI::dbExecute(emuDBhandle$connection, hier_right_trapeze_interm_res_tmp_idx)
  
  # get hierarchy paths
  connectHierPaths = get_hierPathsConnectingLevels(emuDBhandle, 
                                                   firstLevelName, 
                                                   secondLevelName)
  
  if(verbose & length(connectHierPaths) >= 2){
    
    cat(paste0("More than one path connecting: '", 
               firstLevelName, 
               "' and '", 
               secondLevelName, 
               "' was found! The paths were: \n" ))
    for(i in 1:length(connectHierPaths)){
      cat(paste0(i, ".) ", 
                 paste0(connectHierPaths[[i]], collapse = "->")), 
          "\n")
    }
    idx <- readline(prompt="Choose a path by selecting its number (note that comma seperated numbers (e.g., 1, 2, 3) works to select multiple paths): ")
    
    idx = as.integer(stringr::str_split(idx, ",\\s*", simplify = T))
    
    connectHierPaths = connectHierPaths[idx]
  }
  
  # loop through multiple paths
  for(connectHierPath in connectHierPaths){
    
    #############################################################
    # loop through path of hierarchy starting at the bottom
    # to reduce the search space
    
    # empty tables just to be safe
    DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_left_trapeze_interm_res_tmp")
    DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_right_trapeze_interm_res_tmp")
    DBI::dbExecute(emuDBhandle$connection, paste0("DELETE FROM lr_exp_res_tmp"))
    
    # depending on what side is the leaf (== further down in hierarchy) get the correct table name
    if(firstLevelName == connectHierPath[length(connectHierPath)]){
      # left is leaf
      leafSideTableName = paste0("interm_res_items_tmp_", leftTableSuffix)
      # right is anchor
      anchorSideTableName = paste0("interm_res_items_tmp_", rightTableSuffix)
      # 
      leftIsLeaf = TRUE
    }else if(secondLevelName == connectHierPath[length(connectHierPath)]){
      # right is leaf
      leafSideTableName = paste0("interm_res_items_tmp_", rightTableSuffix)
      # left is anchor
      anchorSideTableName = paste0("interm_res_items_tmp_", leftTableSuffix)
      # 
      leftIsLeaf = FALSE
    }
    
    for(i in length(connectHierPath):1){
      if(i == length(connectHierPath)){
        # start at bottom of connectHierPath
        # walk up left side of trapeze
        DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO hier_left_trapeze_interm_res_tmp ",
                                                      "SELECT DISTINCT ",
                                                      " lstn.db_uuid, ",
                                                      " lstn.session, ",
                                                      " lstn.bundle, ",
                                                      " ift.item_id AS seq_start_id, ",
                                                      " ift.item_id AS seq_end_id, ",
                                                      " 1 AS seq_len, ",
                                                      " ift.level, ",
                                                      " ift.seq_idx AS seq_start_seq_idx, ",
                                                      " ift.seq_idx AS seq_end_seq_idx, ",
                                                      " lstn.db_uuid AS db_uuid_leaf, ",
                                                      " lstn.session AS session_leaf, ",
                                                      " lstn.bundle AS bundle_leaf, ",
                                                      " lstn.seq_start_id AS seq_start_id_leaf, ",
                                                      " lstn.seq_end_id AS seq_end_id_leaf, ",
                                                      " lstn.seq_len AS seq_len_leaf, ",
                                                      " lstn.level AS level_leaf, ",
                                                      " lstn.seq_start_seq_idx AS seq_start_seq_idx, ",
                                                      " lstn.seq_end_seq_idx AS seq_end_seq_idx ",
                                                      "FROM ", leafSideTableName, " AS lstn, ",
                                                      " links", filteredTablesSuffix, " AS lft, ",
                                                      " items", filteredTablesSuffix, " AS ift ",
                                                      "WHERE lstn.db_uuid = lft.db_uuid ",
                                                      " AND lstn.session = lft.session ",
                                                      " AND lstn.bundle = lft.bundle ",
                                                      " AND lstn.seq_start_id = lft.to_id ",
                                                      " AND lft.db_uuid = ift.db_uuid ",
                                                      " AND lft.session = ift.session ",
                                                      " AND lft.bundle = ift.bundle ",
                                                      " AND lft.from_id = ift.item_id",
                                                      ""))
        
        # walk up right side of trapeze
        DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO hier_right_trapeze_interm_res_tmp ",
                                                      "SELECT DISTINCT ",
                                                      " lstn.db_uuid, ",
                                                      " lstn.session, ",
                                                      " lstn.bundle, ",
                                                      " ift.item_id AS seq_start_id, ",
                                                      " ift.item_id AS seq_end_id, ",
                                                      " 1 AS seq_len, ",
                                                      " ift.level, ",
                                                      " ift.seq_idx AS seq_start_seq_idx, ",
                                                      " ift.seq_idx AS seq_end_seq_idx, ",
                                                      " lstn.db_uuid AS db_uuid_leaf, ",
                                                      " lstn.session AS session_leaf, ",
                                                      " lstn.bundle AS bundle_leaf, ",
                                                      " lstn.seq_start_id AS seq_start_id_leaf, ",
                                                      " lstn.seq_end_id AS seq_end_id_leaf, ",
                                                      " lstn.seq_len AS seq_len_leaf, ",
                                                      " lstn.level AS level_leaf, ",
                                                      " lstn.seq_start_seq_idx AS seq_start_seq_idx, ",
                                                      " lstn.seq_end_seq_idx AS seq_end_seq_idx  ",
                                                      "FROM ", leafSideTableName, " AS lstn, ",
                                                      " links", filteredTablesSuffix, " AS lft, ",
                                                      " items", filteredTablesSuffix, " AS ift ",
                                                      "WHERE lstn.db_uuid = lft.db_uuid ",
                                                      " AND lstn.session = lft.session ",
                                                      " AND lstn.bundle = lft.bundle ",
                                                      " AND lstn.seq_end_id = lft.to_id ",
                                                      " AND lft.db_uuid = ift.db_uuid ",
                                                      " AND lft.session = ift.session ",
                                                      " AND lft.bundle = ift.bundle ",
                                                      " AND lft.from_id = ift.item_id",
                                                      ""))
        
        if(minMaxSeqIdxLeafOnly){
          reduce_hierTrapezeIntermRes_minMaxSeqIdx(emuDBhandle)
        }
        
      }else if(i != 1){
        
        # walk up left side of trapeze
        leftTrapezeTmp = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT ",
                                                                        " hltirt.db_uuid, ",
                                                                        " hltirt.session, ",
                                                                        " hltirt.bundle, ",
                                                                        " ift.item_id AS seq_start_id, ",
                                                                        " ift.item_id AS seq_end_id, ",
                                                                        " 1 AS seq_len, ",
                                                                        " ift.level, ",
                                                                        " ift.seq_idx AS seq_start_seq_idx, ",
                                                                        " ift.seq_idx AS seq_end_seq_idx, ",
                                                                        " hltirt.db_uuid_leaf, ",
                                                                        " hltirt.session_leaf, ",
                                                                        " hltirt.bundle_leaf, ",
                                                                        " hltirt.seq_start_id_leaf, ",
                                                                        " hltirt.seq_end_id_leaf, ",
                                                                        " hltirt.seq_len_leaf, ",
                                                                        " hltirt.level_leaf, ",
                                                                        " hltirt.seq_start_seq_idx_leaf, ",
                                                                        " hltirt.seq_end_seq_idx_leaf ",
                                                                        "FROM hier_left_trapeze_interm_res_tmp AS hltirt, ",
                                                                        " links", filteredTablesSuffix, " AS lft, ",
                                                                        " items", filteredTablesSuffix, " AS ift ",
                                                                        "WHERE hltirt.db_uuid = lft.db_uuid ",
                                                                        " AND hltirt.session = lft.session ",
                                                                        " AND hltirt.bundle = lft.bundle ",
                                                                        " AND hltirt.seq_start_id = lft.to_id ",
                                                                        " AND lft.db_uuid = ift.db_uuid ",
                                                                        " AND lft.session = ift.session ",
                                                                        " AND lft.bundle = ift.bundle ",
                                                                        " AND lft.from_id = ift.item_id", 
                                                                        ""))
        
        # walk up right side of trapeze
        rightTrapezeTmp = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT DISTINCT ",
                                                                         " hrtirt.db_uuid, ",
                                                                         " hrtirt.session, ",
                                                                         " hrtirt.bundle, ",
                                                                         " ift.item_id AS seq_start_id, ",
                                                                         " ift.item_id AS seq_end_id, ",
                                                                         " 1 AS seq_len, ",
                                                                         " ift.level, ",
                                                                         " ift.seq_idx AS seq_start_seq_idx, ",
                                                                         " ift.seq_idx AS seq_end_seq_idx, ",
                                                                         " hrtirt.db_uuid_leaf, ",
                                                                         " hrtirt.session_leaf, ",
                                                                         " hrtirt.bundle_leaf, ",
                                                                         " hrtirt.seq_start_id_leaf, ",
                                                                         " hrtirt.seq_end_id_leaf, ",
                                                                         " hrtirt.seq_len_leaf, ",
                                                                         " hrtirt.level_leaf, ",
                                                                         " hrtirt.seq_start_seq_idx_leaf, ",
                                                                         " hrtirt.seq_end_seq_idx_leaf ",
                                                                         "FROM hier_right_trapeze_interm_res_tmp AS hrtirt, ",
                                                                         " links", filteredTablesSuffix, " AS lft, ",
                                                                         " items", filteredTablesSuffix, " AS ift ",
                                                                         "WHERE hrtirt.db_uuid = lft.db_uuid ",
                                                                         " AND hrtirt.session = lft.session ",
                                                                         " AND hrtirt.bundle = lft.bundle ",
                                                                         " AND hrtirt.seq_start_id = lft.to_id ",
                                                                         " AND lft.db_uuid = ift.db_uuid ",
                                                                         " AND lft.session = ift.session ",
                                                                         " AND lft.bundle = ift.bundle ",
                                                                         " AND lft.from_id = ift.item_id ",
                                                                         ""))
        
        DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_left_trapeze_interm_res_tmp")
        DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_right_trapeze_interm_res_tmp")
        
        DBI::dbWriteTable(emuDBhandle$connection, "hier_left_trapeze_interm_res_tmp", 
                          leftTrapezeTmp, 
                          append = T, 
                          row.names = F)
        DBI::dbWriteTable(emuDBhandle$connection, "hier_right_trapeze_interm_res_tmp", 
                          rightTrapezeTmp, 
                          append = T, 
                          row.names = F)
        
        if(minMaxSeqIdxLeafOnly){
          reduce_hierTrapezeIntermRes_minMaxSeqIdx(emuDBhandle)
        }
        
      }else{
        # at the top of the trapeze:
        # extract leaf and anchor values persisting the left and right sides as the input tables
        # in the resulting lr_exp_res_tmp table
        if(leftIsLeaf){
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT OR IGNORE INTO lr_exp_res_tmp ",
                                                        "SELECT DISTINCT ",
                                                        " hltirt.db_uuid,  ",
                                                        " hltirt.session, ",
                                                        " hltirt.bundle, ",
                                                        " hrtirt.seq_start_id_leaf AS l_seq_start_id, ",
                                                        " hrtirt.seq_end_id_leaf AS l_seq_end_id, ",
                                                        " hrtirt.seq_len_leaf AS l_seq_len, ",
                                                        " hrtirt.level_leaf AS l_level, ",
                                                        " hrtirt.seq_start_seq_idx_leaf AS l_seq_start_seq_idx, ",
                                                        " hrtirt.seq_end_seq_idx_leaf AS l_seq_end_seq_idx, ",
                                                        " hltirt.seq_start_id AS r_seq_start_id, ",
                                                        " hrtirt.seq_end_id AS r_seq_end_id, ",
                                                        " NULL AS r_seq_len, ",
                                                        " hltirt.level AS r_level, ",
                                                        " hltirt.seq_start_seq_idx AS r_seq_start_seq_idx, ",
                                                        " hrtirt.seq_end_seq_idx AS r_seq_end_seq_idx ",
                                                        "FROM ", anchorSideTableName , " AS astn, ",
                                                        " hier_left_trapeze_interm_res_tmp AS hltirt, ",
                                                        " hier_right_trapeze_interm_res_tmp AS hrtirt ",
                                                        "WHERE astn.db_uuid = hltirt.db_uuid ",
                                                        " AND astn.session = hltirt.session ",
                                                        " AND astn.bundle = hltirt.bundle ",
                                                        " AND astn.seq_start_id = hltirt.seq_start_id ",
                                                        " AND astn.db_uuid = hrtirt.db_uuid ",
                                                        " AND astn.session = hrtirt.session ",
                                                        " AND astn.bundle = hrtirt.bundle ",
                                                        " AND astn.seq_end_id = hrtirt.seq_start_id ",
                                                        ""))
          
          # calculate and update missing r_seq_len
          DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE lr_exp_res_tmp ",
                                                        "SET r_seq_len = (",
                                                        "SELECT ift2.seq_idx - ift1.seq_idx + 1 ",
                                                        "FROM lr_exp_res_tmp, items", filteredTablesSuffix, " AS ift1, ",
                                                        " items", filteredTablesSuffix, " AS ift2 ",
                                                        "WHERE lr_exp_res_tmp.db_uuid = ift1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = ift1.session ",
                                                        " AND lr_exp_res_tmp.bundle = ift1.bundle ",
                                                        " AND lr_exp_res_tmp.l_seq_start_id = ift1.item_id ",
                                                        " AND lr_exp_res_tmp.db_uuid = ift2.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = ift2.session ",
                                                        " AND lr_exp_res_tmp.bundle = ift2.bundle ",
                                                        " AND lr_exp_res_tmp.l_seq_end_id = ift2.item_id ",
                                                        ")"))
          
        }else{
          #
          DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO lr_exp_res_tmp ",
                                                        "SELECT DISTINCT ",
                                                        " hltirt.db_uuid,  ",
                                                        " hltirt.session, ",
                                                        " hltirt.bundle, ",
                                                        " hltirt.seq_start_id AS l_seq_start_id, ",
                                                        " hrtirt.seq_end_id AS l_seq_end_id, ",
                                                        " NULL AS l_seq_len, ",
                                                        " hltirt.level AS l_level, ",
                                                        " hltirt.seq_start_seq_idx AS l_seq_start_seq_idx, ",
                                                        " hltirt.seq_end_seq_idx AS l_seq_end_seq_idx, ",
                                                        " hrtirt.seq_start_id_leaf AS r_seq_start_id, ",
                                                        " hrtirt.seq_end_id_leaf AS r_seq_end_id, ",
                                                        " hrtirt.seq_len_leaf AS r_seq_len, ",
                                                        " hrtirt.level_leaf AS r_level, ",
                                                        " hrtirt.seq_start_seq_idx_leaf AS r_seq_start_seq_idx, ",
                                                        " hrtirt.seq_end_seq_idx_leaf AS r_seq_end_seq_idx ",
                                                        "FROM ", anchorSideTableName , " AS astn, ",
                                                        " hier_left_trapeze_interm_res_tmp AS hltirt, ",
                                                        " hier_right_trapeze_interm_res_tmp AS hrtirt ",
                                                        "WHERE astn.db_uuid = hltirt.db_uuid ",
                                                        " AND astn.session = hltirt.session ",
                                                        " AND astn.bundle = hltirt.bundle ",
                                                        " AND astn.seq_start_id = hltirt.seq_start_id ",
                                                        " AND astn.db_uuid = hrtirt.db_uuid ",
                                                        " AND astn.session = hrtirt.session ",
                                                        " AND astn.bundle = hrtirt.bundle ",
                                                        " AND astn.seq_end_id = hrtirt.seq_start_id ",
                                                        ""))
          # calculate and update missing l_seq_len
          DBI::dbExecute(emuDBhandle$connection, paste0("UPDATE lr_exp_res_tmp ",
                                                        "SET l_seq_len = (",
                                                        "SELECT ift2.seq_idx - ift1.seq_idx + 1 ",
                                                        "FROM lr_exp_res_tmp, items", filteredTablesSuffix, " AS ift1, ",
                                                        " items", filteredTablesSuffix, " AS ift2 ",
                                                        "WHERE lr_exp_res_tmp.db_uuid = ift1.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = ift1.session ",
                                                        " AND lr_exp_res_tmp.bundle = ift1.bundle ",
                                                        " AND lr_exp_res_tmp.l_seq_start_id = ift1.item_id ",
                                                        " AND lr_exp_res_tmp.db_uuid = ift2.db_uuid ",
                                                        " AND lr_exp_res_tmp.session = ift2.session ",
                                                        " AND lr_exp_res_tmp.bundle = ift2.bundle ",
                                                        " AND lr_exp_res_tmp.l_seq_end_id = ift2.item_id ",
                                                        ")"))
        }
        
        
      }
    }
  }
  # clean up tmp tables
  DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_left_trapeze_interm_res_tmp")
  DBI::dbExecute(emuDBhandle$connection, "DELETE FROM hier_right_trapeze_interm_res_tmp")
  
  # insert NA rows if preserveLeafLength or preserveAnchorLength are set
  # if(preserveLeafLength){
  #   preservedLengthTable = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT lr_exp_res_tmp.* ",
  #                                                                         "FROM ", leafSideTableName, " AS lstn ",
  #                                                                         "LEFT JOIN lr_exp_res_tmp ",
  #                                                                         "ON lstn.db_uuid = lr_exp_res_tmp.db_uuid ",
  #                                                                         " AND lstn.session = lr_exp_res_tmp.session ",
  #                                                                         " AND lstn.bundle = lr_exp_res_tmp.bundle ",
  #                                                                         " AND lstn.seq_start_id = lr_exp_res_tmp.l_seq_start_id ",
  #                                                                         " AND lstn.seq_end_id = lr_exp_res_tmp.l_seq_end_id ",
  #                                                                         "ORDER BY lstn.rowid ",
  #                                                                         ""))
  #   
  #   DBI::dbExecute(emuDBhandle$connection, "DELETE FROM lr_exp_res_tmp")
  #   DBI::dbWriteTable(emuDBhandle$connection, "lr_exp_res_tmp", preservedLengthTable, append = T, row.names = F)
  #   
  # }else 
  
  if(preserveLeafLength | preserveAnchorLength){
    
    # set table name and seq_*_id prefix used for collapsing
    # e.g.: if preserveLeafLength & leftIsLeaf -> use left side for grouping (hence collapsing)
    if(preserveLeafLength){
      preserveTableName = leafSideTableName
      if(leftIsLeaf){ 
        seqIdPrefix = "l_"
      }else{
        seqIdPrefix = "r_"
      }
    }else{
      preserveTableName = anchorSideTableName
      if(leftIsLeaf){ 
        seqIdPrefix = "r_"
      }else{
        seqIdPrefix = "l_"
      }
      
    }
    
    
    #####################################
    # first step (in two step process): collapse into new table using MIN(seq_start_id) & MAX(seq_end_id)
    # this avoids expanstion in e.g. MANY_TO_MANY relationships
    DBI::dbExecute(emuDBhandle$connection, paste0("CREATE TEMP TABLE IF NOT EXISTS lr_minmax_seqidx_tmp ( ",
                                                  " db_uuid VARCHAR(36), ",
                                                  " session TEXT, ",
                                                  " bundle TEXT, ",
                                                  " l_level TEXT, ", # preserve other side for NA preservation
                                                  " l_min_seq_idx TEXT,  ",
                                                  " l_max_seq_idx TEXT, ",
                                                  " r_level TEXT,",
                                                  " r_min_seq_idx INTEGER, ",
                                                  " r_max_seq_idx INTEGER ",
                                                  ")"))
    
    DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO lr_minmax_seqidx_tmp ",
                                                  "SELECT lr.db_uuid, ", 
                                                  " lr.session, ", 
                                                  " lr.bundle, ", 
                                                  " l_level, ", 
                                                  " min(lr.l_seq_start_seq_idx) AS l_min_seq_idx, ", 
                                                  " max(lr.l_seq_end_seq_idx) AS l_max_seq_idx, ",
                                                  " r_level, ",
                                                  " min(lr.r_seq_start_seq_idx) AS r_min_seq_idx, ", 
                                                  " max(lr.r_seq_end_seq_idx) AS r_max_seq_idx ",
                                                  "FROM lr_exp_res_tmp AS lr ",
                                                  "GROUP BY lr.db_uuid, ",
                                                  " lr.session, ", 
                                                  " lr.bundle, ", 
                                                  " lr.", seqIdPrefix, "seq_start_id, ",
                                                  " lr.", seqIdPrefix, "seq_end_id ",
                                                  ""))
    
    # step two: extract according item_ids and place into new table
    DBI::dbExecute(emuDBhandle$connection, paste0("CREATE TEMP TABLE lr_exp_res_collapsed_tmp (",
                                                  " db_uuid VARCHAR(36),",
                                                  " session TEXT,",
                                                  " bundle TEXT,",
                                                  " l_seq_start_id INTEGER,",
                                                  " l_seq_end_id INTEGER,",
                                                  " l_seq_len INTEGER,",
                                                  " l_level TEXT,",
                                                  " l_seq_start_seq_idx INTEGER,",
                                                  " l_seq_end_seq_idx INTEGER,",
                                                  " r_seq_start_id INTEGER,",
                                                  " r_seq_end_id INTEGER,",
                                                  " r_seq_len INTEGER,",
                                                  " r_level TEXT,",
                                                  " r_seq_start_seq_idx INTEGER,",
                                                  " r_seq_end_seq_idx INTEGER",
                                                  ");"))
    
    
    
    DBI::dbExecute(emuDBhandle$connection, paste0("INSERT INTO lr_exp_res_collapsed_tmp ",
                                                  "SELECT ",
                                                  " lr_minmax_seqidx_tmp.db_uuid, ",
                                                  " lr_minmax_seqidx_tmp.session, ",
                                                  " lr_minmax_seqidx_tmp.bundle, ",
                                                  " i_left_min_idx.item_id AS l_seq_start_id, ",
                                                  " i_left_max_idx.item_id AS l_seq_end_id, ",
                                                  " (i_left_max_idx.seq_idx - i_left_max_idx.seq_idx) + 1 AS seq_len, ",
                                                  " i_left_min_idx.level, ",
                                                  " i_left_min_idx.seq_idx AS l_seq_start_seq_idx, ",
                                                  " i_left_max_idx.seq_idx AS l_seq_end_seq_idx, ",
                                                  " i_right_min_idx.item_id AS l_seq_start_id, ",
                                                  " i_right_max_idx.item_id AS l_seq_end_id, ",
                                                  " (i_right_max_idx.seq_idx - i_right_max_idx.seq_idx) + 1 AS seq_len, ",
                                                  " i_right_min_idx.level, ",
                                                  " i_right_min_idx.seq_idx AS l_seq_start_seq_idx, ",
                                                  " i_right_max_idx.seq_idx AS l_seq_end_seq_idx ",
                                                  "FROM lr_minmax_seqidx_tmp ", # re-inserts NAs
                                                  "LEFT JOIN items AS i_left_min_idx ",
                                                  "ON lr_minmax_seqidx_tmp.db_uuid = i_left_min_idx.db_uuid ",
                                                  " AND lr_minmax_seqidx_tmp.session = i_left_min_idx.session ",
                                                  " AND lr_minmax_seqidx_tmp.bundle = i_left_min_idx.bundle ",
                                                  " AND lr_minmax_seqidx_tmp.l_level = i_left_min_idx.level ",
                                                  " AND lr_minmax_seqidx_tmp.l_min_seq_idx = i_left_min_idx.seq_idx ",
                                                  "LEFT JOIN items AS i_left_max_idx ",
                                                  "ON lr_minmax_seqidx_tmp.db_uuid = i_left_max_idx.db_uuid ",
                                                  " AND lr_minmax_seqidx_tmp.session = i_left_max_idx.session ",
                                                  " AND lr_minmax_seqidx_tmp.bundle = i_left_max_idx.bundle ",
                                                  " AND lr_minmax_seqidx_tmp.l_level = i_left_max_idx.level ",
                                                  " AND lr_minmax_seqidx_tmp.l_max_seq_idx = i_left_max_idx.seq_idx ",
                                                  "LEFT JOIN items AS i_right_min_idx ",
                                                  "ON lr_minmax_seqidx_tmp.db_uuid = i_right_min_idx.db_uuid ",
                                                  " AND lr_minmax_seqidx_tmp.session = i_right_min_idx.session ",
                                                  " AND lr_minmax_seqidx_tmp.bundle = i_right_min_idx.bundle ",
                                                  " AND lr_minmax_seqidx_tmp.r_level = i_right_min_idx.level ",
                                                  " AND lr_minmax_seqidx_tmp.r_min_seq_idx = i_right_min_idx.seq_idx ",
                                                  "LEFT JOIN items AS i_right_max_idx ",
                                                  "ON lr_minmax_seqidx_tmp.db_uuid = i_right_max_idx.db_uuid ",
                                                  " AND lr_minmax_seqidx_tmp.session = i_right_max_idx.session ",
                                                  " AND lr_minmax_seqidx_tmp.bundle = i_right_max_idx.bundle ",
                                                  " AND lr_minmax_seqidx_tmp.r_level = i_right_max_idx.level ",
                                                  " AND lr_minmax_seqidx_tmp.r_max_seq_idx = i_right_max_idx.seq_idx ",
                                                  ""))
    
    # finally left join to preserveTableName
    preservedLengthTable = DBI::dbGetQuery(emuDBhandle$connection, paste0("SELECT lr.* ",
                                                                          "FROM ", preserveTableName, " AS astn ",
                                                                          "LEFT JOIN lr_exp_res_collapsed_tmp AS lr ",
                                                                          "ON astn.db_uuid = lr.db_uuid ",
                                                                          " AND astn.session = lr.session ",
                                                                          " AND astn.bundle = lr.bundle ",
                                                                          " AND astn.seq_start_id = lr.", seqIdPrefix, "seq_start_id ",
                                                                          " AND astn.seq_end_id = lr.", seqIdPrefix, "seq_end_id ",
                                                                          "LEFT JOIN items AS items_start ",
                                                                          "ON lr.db_uuid = items_start.db_uuid ",
                                                                          " AND lr.session = items_start.session ",
                                                                          " AND lr.bundle = items_start.bundle ",
                                                                          " AND lr.", seqIdPrefix, "seq_start_id = items_start.item_id ",
                                                                          "LEFT JOIN items AS items_end ",
                                                                          "ON lr.db_uuid = items_end.db_uuid ",
                                                                          " AND lr.session = items_end.session ",
                                                                          " AND lr.bundle = items_end.bundle ",
                                                                          " AND lr.", seqIdPrefix, "seq_end_id = items_end.item_id ",
                                                                          "GROUP BY astn.rowid ", # preserve astn length
                                                                          "ORDER BY astn.rowid ", # preserve astn ordering
                                                                          ""))
    
    # drop temp table
    DBI::dbExecute(emuDBhandle$connection,paste0("DROP TABLE IF EXISTS lr_minmax_seqidx_tmp"))
    DBI::dbExecute(emuDBhandle$connection,paste0("DROP TABLE IF EXISTS lr_exp_res_collapsed_tmp"))
    
    DBI::dbExecute(emuDBhandle$connection, "DELETE FROM lr_exp_res_tmp")
    DBI::dbWriteTable(emuDBhandle$connection, "lr_exp_res_tmp", preservedLengthTable, append = T, row.names = F)
  }
  
}


##################################
query_databaseEqlInBracket<-function(emuDBhandle, 
                                     q, 
                                     intermResTableSuffix, 
                                     leftRightTableNrCounter = 0, 
                                     filteredTablesSuffix, 
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
                          intermResTableSuffix = leftTableSuffix, 
                          leftRightTableNrCounter, 
                          filteredTablesSuffix, 
                          verbose)
    query_databaseWithEql(emuDBhandle, 
                          right, 
                          intermResTableSuffix = rightTableSuffix, 
                          leftRightTableNrCounter + 1, 
                          filteredTablesSuffix, 
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
    lResAttrName = DBI::dbGetQuery(emuDBhandle$connection, 
                                   paste0("SELECT result_level FROM interm_res_meta_infos_tmp_", leftTableSuffix))$result_level
    lResLvl = get_levelNameForAttributeName(emuDBhandle, lResAttrName)
    
    rResAttrName = DBI::dbGetQuery(emuDBhandle$connection, 
                                   paste0("SELECT result_level FROM interm_res_meta_infos_tmp_", rightTableSuffix))$result_level
    rResLvl = get_levelNameForAttributeName(emuDBhandle, rResAttrName)
    
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
    # Edit : in the same attribyte of a level #215
    if(seqPos != -1 & lResAttrName != rResAttrName){
      stop("Sequential queries need to reference a labels in the same attributes of a level.\n The sequence query '", 
           qTrim,
           "' tries to find labels in different attributes ('",
           lResAttrName,
           "' and '",
           rResAttrName,"') and is therefore not valid.",call.=FALSE)
    }
    
    
    if(domPos != -1){
      # parse DOMQ
      # query the result level of left term
      nLinks = DBI::dbGetQuery(emuDBhandle$connection, 
                               paste0("SELECT COUNT(*) AS n FROM links", filteredTablesSuffix))$n
      if(nLinks == 0){
        clear_intermResTabels(emuDBhandle, leftTableSuffix)
        clear_intermResTabels(emuDBhandle, rightTableSuffix)
        return()
      }
      
      query_databaseHier(emuDBhandle, 
                         lResLvl, 
                         rResLvl, 
                         leftTableSuffix, 
                         rightTableSuffix, 
                         filteredTablesSuffix, 
                         verbose = verbose) # result written to lr_exp_res_tmp
      
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
                            append = T, 
                            row.names = F)
          
        }
        
        if(nRightProjItems != 0){
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
                            append = T, 
                            row.names = F)
          
        }
      }
      
      # if no projItems -> place left Meta infos in result table
      allMeta = DBI::dbGetQuery(emuDBhandle$connection, 
                                paste0("SELECT * FROM interm_res_meta_infos_tmp_", leftTableSuffix))
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM interm_res_meta_infos_tmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, 
                        paste0("interm_res_meta_infos_tmp_", intermResTableSuffix), 
                        allMeta, 
                        append = T, 
                        row.names = F)
      
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
                                        " l_seq_start_seq_idx AS seq_start_seq_idx, ",
                                        " l_seq_end_seq_idx AS seq_end_seq_idx ",
                                        "FROM lr_exp_res_tmp"))
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM interm_res_items_tmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, 
                        paste0("interm_res_items_tmp_", intermResTableSuffix), 
                        resItems, 
                        append = T, 
                        row.names = F)
      
    }
    if(seqPos!=-1){
      # query the result level of left term (removed lid.seq_end_id AS leId,rid.seq_start_id AS rsId,)
      lrSeqQueryStr = paste0("SELECT ",
                             " lid.db_uuid, ",
                             " lid.session, ",
                             " lid.bundle, ",
                             " lid.seq_start_id AS l_seq_start_id, ",
                             " lid.seq_end_id AS l_seq_end_id, ",
                             " lid.seq_len AS l_seq_len, ",
                             " lid.level AS l_level, ",
                             " lid.seq_start_seq_idx AS l_seq_start_seq_idx, ",
                             " lid.seq_end_seq_idx AS l_seq_end_seq_idx, ",
                             " rid.seq_start_id AS r_seq_start_id, ",
                             " rid.seq_end_id AS r_seq_end_id, ",
                             " rid.seq_len AS r_seq_len, ",
                             " lid.level AS r_level, ",
                             " rid.seq_start_seq_idx AS r_seq_start_seq_idx, ",
                             " rid.seq_end_seq_idx AS r_seq_end_seq_idx ",
                             "FROM interm_res_items_tmp_", leftTableSuffix, " AS lid, ",
                             " interm_res_items_tmp_", rightTableSuffix, " AS rid, ",
                             " items", filteredTablesSuffix, " AS il, ",
                             " items", filteredTablesSuffix, " AS ir ",
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
                             "")
      
      
      # perform query an place result into lr_exp_res_tmp table
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM lr_exp_res_tmp"))
      insertQueryStr = paste0("INSERT INTO lr_exp_res_tmp ", lrSeqQueryStr)
      DBI::dbExecute(emuDBhandle$connection, insertQueryStr)
      
      # check if no sequences where found -> clear & return
      nSeq = DBI::dbGetQuery(emuDBhandle$connection, 
                             paste0("SELECT COUNT(*) AS n FROM lr_exp_res_tmp"))$n
      if(nSeq == 0){
        # move left meta infos (to avoid empty meta table for ("query_str" entry))
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, 
                                  paste0("SELECT * FROM interm_res_meta_infos_tmp_", leftTableSuffix))
        DBI::dbExecute(emuDBhandle$connection, 
                       paste0("DELETE FROM interm_res_meta_infos_tmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, 
                          paste0("interm_res_meta_infos_tmp_", intermResTableSuffix), 
                          allMeta, 
                          append = T, 
                          row.names = F)
        
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
                          append = T, 
                          row.names = F)
        # move meta infos to correct table
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, 
                                  paste0("SELECT * FROM interm_res_meta_infos_tmp_", leftTableSuffix))
        DBI::dbExecute(emuDBhandle$connection, 
                       paste0("DELETE FROM interm_res_meta_infos_tmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, 
                          paste0("interm_res_meta_infos_tmp_", intermResTableSuffix), 
                          allMeta, 
                          append = T, 
                          row.names = F)
        
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
                          append = T, 
                          row.names = F)
        # move meta infos to correct table
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, 
                                  paste0("SELECT * FROM interm_res_meta_infos_tmp_", rightTableSuffix))
        DBI::dbExecute(emuDBhandle$connection, 
                       paste0("DELETE FROM interm_res_meta_infos_tmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, 
                          paste0("interm_res_meta_infos_tmp_", intermResTableSuffix), 
                          allMeta, 
                          append = T, 
                          row.names = F)
        
      }
      
      if(nLeftProjItems == 0 & nRightProjItems == 0){
        # if no projItems -> place left Meta infos in result table
        allMeta = DBI::dbGetQuery(emuDBhandle$connection, 
                                  paste0("SELECT * FROM interm_res_meta_infos_tmp_", leftTableSuffix))
        DBI::dbExecute(emuDBhandle$connection, 
                       paste0("DELETE FROM interm_res_meta_infos_tmp_", intermResTableSuffix))
        DBI::dbWriteTable(emuDBhandle$connection, 
                          paste0("interm_res_meta_infos_tmp_", intermResTableSuffix), 
                          allMeta, 
                          append = T, 
                          row.names = F)
      }
      
      # place result in correct table
      resItems = DBI::dbGetQuery(emuDBhandle$connection, 
                                 paste0("SELECT DISTINCT ",
                                        " db_uuid, ",
                                        " session, ",
                                        " bundle, ",
                                        " l_seq_start_id AS seq_start_id, ",
                                        " r_seq_end_id AS seq_end_id, ",
                                        " l_seq_len+r_seq_len AS seq_len, ",
                                        " l_level AS level, ",
                                        " l_seq_start_seq_idx AS seq_start_seq_idx, ",
                                        " r_seq_end_seq_idx AS seq_end_seq_idx ",
                                        "FROM lr_exp_res_tmp"))
      DBI::dbExecute(emuDBhandle$connection, 
                     paste0("DELETE FROM interm_res_items_tmp_", intermResTableSuffix))
      DBI::dbWriteTable(emuDBhandle$connection, 
                        paste0("interm_res_items_tmp_", intermResTableSuffix), 
                        resItems, 
                        append = T, 
                        row.names = F)
    }
    return()
  }else{
    query_databaseWithEql(emuDBhandle, 
                          qTrim, 
                          intermResTableSuffix, 
                          leftRightTableNrCounter, 
                          filteredTablesSuffix, 
                          verbose = verbose)
  }
}

###################
query_databaseWithEql <- function(emuDBhandle, 
                                  query, 
                                  intermResTableSuffix, 
                                  leftRightTableNrCounter, 
                                  filteredTablesSuffix, 
                                  verbose){
  parseRes = list()
  qTrim = stringr::str_trim(query)
  brOpenPos = get_charPosition(qTrim, '[', literalQuote = "'")
  if(brOpenPos == -1){
    query_databaseEqlCONJQ(emuDBhandle, 
                           qTrim, 
                           intermResTableSuffix = intermResTableSuffix, 
                           filteredTablesSuffix, 
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
                               intermResTableSuffix, 
                               leftRightTableNrCounter, 
                               filteredTablesSuffix, 
                               verbose = verbose)
    return()
    
  }
  stop("Unknown syntax error.")
}

####################
query_databaseWithEqlEmusegs <- function(emuDBhandle, 
                                         query, 
                                         timeRefSegmentLevel, 
                                         filteredTablesSuffix, 
                                         calcTimes, 
                                         verbose){
  # create "root" intermediate result tables
  create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = "root")
  # query emuDB
  query_databaseWithEql(emuDBhandle, 
                        query, 
                        intermResTableSuffix = "root", 
                        leftRightTableNrCounter = 0, 
                        filteredTablesSuffix, 
                        verbose)
  # escape singel quotes
  query = gsub("'", "''", query)
  DBI::dbExecute(emuDBhandle$connection, 
                 paste0("UPDATE interm_res_meta_infos_tmp_root SET query_str = '", query, "'"))
  emusegs = convert_queryResultToEmusegs(emuDBhandle, 
                                         timeRefSegmentLevel, 
                                         filteredTablesSuffix, 
                                         calcTimes, 
                                         verbose)
  return(emusegs)
  
}

####################
query_databaseWithEqlEmuRsegs <- function(emuDBhandle, 
                                          query, 
                                          timeRefSegmentLevel, 
                                          filteredTablesSuffix, 
                                          calcTimes, 
                                          verbose){
  # create "root" intermediate result tables
  create_intermResTmpQueryTablesDBI(emuDBhandle, suffix = "root")
  # query emuDB
  query_databaseWithEql(emuDBhandle, 
                        query, 
                        intermResTableSuffix = "root", 
                        leftRightTableNrCounter = 0, 
                        filteredTablesSuffix, 
                        verbose = verbose)
  # escape single quotes
  queryStr = gsub("'", "''", query)
  # DBI::dbGetQuery(emuDBhandle$connection, paste0("UPDATE interm_res_meta_infos_tmp_root SET query_str = '", queryStr, "'"))
  emuRsegs = convert_queryResultToEmuRsegs(emuDBhandle, 
                                           timeRefSegmentLevel, 
                                           filteredTablesSuffix, 
                                           queryStr = queryStr, 
                                           calcTimes = calcTimes, 
                                           verbose = verbose)
  return(emuRsegs)
  
}


##' Query emuDB
##' @description Function to query annotation items/structures in a emuDB
##' @details Evaluates a query string of query language queryLang on an 
##' emuDB referenced by dbName and returns a segment list of the desired type resultType.  
##' For details of the query language please refer to the EMU-SDMS manual's query 
##' system chapter (\url{https://ips-lmu.github.io/The-EMU-SDMS-Manual/chap-querysys.html}).
##' Returns a list of segments which meet the conditions given by the query string. 
##' A segment can consist of one (e.g. 's') or more (e.g. 's->t') items from 
##' the specified emuDB level. Segment objects (type 'SEGMENT') contain the label 
##' string and the start and end time information of the segment (in ms). 
##' \link{emuRsegs} objects additionally contain sample position of start and end item. 
##' Time information of symbolic elements (type 'ITEM') are derived from linked SEGMENT 
##' levels if available. If multiple linked SEGMENT levels exist, you can specify the 
##' level with the \code{timeRefSegmentLevel} argument. If time and sample values cannot be 
##' derived they will be set to \code{\link{NA}}. \link{emuRsegs} result lists will 
##' be ordered by the hidden columns UUID, session, bundle and sequence index (seq_idx). 
##' Legacy \link{emusegs} lists are ordered by the columns utts and start.
##' The query may be limited to session and/or bundle names specified by regular 
##' expression pattern strings (see \link{regex}) in parameters \code{sessionPattern} 
##' respectively \code{bundlePattern}.
##' @param emuDBhandle emuDB handle object (see \link{load_emuDB})
##' @param query string (see vignette \code{EQL})
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
##' boolean parameter.
##' @param verbose be verbose. Set this to \code{TRUE} if you wish to choose which 
##' path to traverse on intersecting hierarchies. If set to \code{FALSE} (the default) 
##' all paths will be traversed (= legacy EMU behaviour).
##' @return result set object of class resultType (default: \link{emuRsegs}, 
##' compatible to legacy type \link{emusegs})
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
##' ## 'Find all sequences /ts/ in level Phonetics'.
##' ## and store result seglist in variable segListTs
##' 
##' seglistTs=query(ae, "[Phonetic == t -> Phonetic == s]")
##' 
##' ## Query database ae with EQL query "[Syllable == S ^ Phoneme == t]":
##' ## 'Find all items 't' in level Phoneme that are dominated by 
##' ## items 'S' in level Syllable.'
##' ## Return legacy Emu result type 'emusegs'
##' 
##' query(ae, "[Syllable == S ^ Phoneme == t]", resultType="emusegs")
##' 
##' ## Query 'p' items of level Phoneme from bundles whose 
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
    # precheck if sessionPattern & bundlePattern are not set
    if((is.null(sessionPattern) || sessionPattern == '.*') 
       && (is.null(bundlePattern) || bundlePattern == '.*')){
      # simply use original tables (no "_filtered_tmp suffix")
      filteredTablesSuffix = ""
    }else{
      # extract all items for session/bundlePattern regEx matching (should check if REGEXP is available and is so use that instead)
      queryItems <- DBI::dbGetQuery(emuDBhandle$connection, 
                                    paste0("SELECT * FROM items WHERE db_uuid='", emuDBhandle$UUID, "'"))
      queryLabels <- DBI::dbGetQuery(emuDBhandle$connection, 
                                     paste0("SELECT * FROM labels WHERE db_uuid='", emuDBhandle$UUID, "'"))
      queryLinks <- DBI::dbGetQuery(emuDBhandle$connection, 
                                    paste0("SELECT * FROM links WHERE db_uuid='", emuDBhandle$UUID,"'"))
      
      # if set get logical vectors that match sessionPattern and bundlePattern
      if(!is.null(sessionPattern) && sessionPattern!='.*'){
        sesSelIts = emuR_regexprl(sessionPattern, queryItems$session)
        sesSelLbls = emuR_regexprl(sessionPattern, queryLabels$session)
        sesSelLks = emuR_regexprl(sessionPattern, queryLinks$session)
      }else{
        sesSelIts = rep(TRUE, nrow(queryItems))
        sesSelLbls = rep(TRUE, nrow(queryLabels))
        sesSelLks = rep(TRUE, nrow(queryLinks))
      }
      if(!is.null(bundlePattern) && bundlePattern!='.*'){
        bndlSelIts = emuR_regexprl(bundlePattern, queryItems$bundle)
        bndlSelLbls = emuR_regexprl(bundlePattern, queryLabels$bundle)
        bndlSelLks = emuR_regexprl(bundlePattern, queryLinks$bundle)
      }else{
        bndlSelIts = rep(TRUE, nrow(queryItems))
        bndlSelLbls = rep(TRUE, nrow(queryLabels))
        bndlSelLks = rep(TRUE, nrow(queryLinks))
      }
      # write to tmp tables
      DBI::dbWriteTable(emuDBhandle$connection, 
                        "items_filtered_tmp", 
                        queryItems[sesSelIts & bndlSelIts, ], 
                        append = TRUE, 
                        row.names = FALSE)
      DBI::dbWriteTable(emuDBhandle$connection, 
                        "labels_filtered_tmp", 
                        queryLabels[sesSelLbls & bndlSelLbls, ], 
                        append = TRUE, 
                        row.names = FALSE)
      DBI::dbWriteTable(emuDBhandle$connection, 
                        "links_filtered_tmp", 
                        queryLinks[sesSelLks & bndlSelLks, ], 
                        append = TRUE, 
                        row.names = FALSE)
      
      filteredTablesSuffix = "_filtered_tmp"
    }
    if(is.null(resultType)){
      emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,
                                               query,
                                               timeRefSegmentLevel, 
                                               filteredTablesSuffix, 
                                               calcTimes, 
                                               verbose = verbose)
      drop_allTmpTablesDBI(emuDBhandle)
      return(emuRsegs)
    }else{
      if(resultType == 'emuRsegs'){
        emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,
                                                 query,
                                                 timeRefSegmentLevel, 
                                                 filteredTablesSuffix, 
                                                 calcTimes, 
                                                 verbose)
        drop_allTmpTablesDBI(emuDBhandle)
        return(emuRsegs)
      }else if(resultType == 'emusegs'){
        if(!is.null(timeRefSegmentLevel)){
          # TODO 
          stop("Parameter timeRefSegmentLevel not yet supported for",
               " resultType 'emusegs'. Please use resultType 'emuRsegs' (default).")
        }
        return(query_databaseWithEqlEmusegs(emuDBhandle, 
                                            query, 
                                            timeRefSegmentLevel, 
                                            filteredTablesSuffix, 
                                            calcTimes, 
                                            verbose))
      }else if(resultType == 'tibble'){
        emuRsegs = query_databaseWithEqlEmuRsegs(emuDBhandle,
                                                 query,
                                                 timeRefSegmentLevel, 
                                                 filteredTablesSuffix, 
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
