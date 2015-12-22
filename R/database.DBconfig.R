create.schema.track <- function(basePath=NULL,name,columnName=name,unitSuffix=NULL,extension=NULL,hasDeepStructure=FALSE){
  o <- list(basePath=basePath,name=name,columnName=columnName,unitSuffix=unitSuffix,fileExtension=extension)
  class(o) <- 'emuDB.schema.track'
  invisible(o)
}

create.schema.annotationDescriptor <- function(name=NULL,basePath=NULL,extension=NULL,type=NULL,timeFactor=NULL,levelDefinitions=NULL){
  o <- list(name=name,basePath=basePath,extension=extension,type=type,timeFactor=timeFactor,levelDefinitions=levelDefinitions)
  class(o) <- 'emuDB.schema.annotationDescriptor'
  invisible(o)
}

## Create emuDB attribute definition object
## 
## @param name name of the level
## @param type level type (ITEM,EVENT,SEGMENT)
## @return object of class emuDB.schema.attributeDefinition
## @author Klaus Jaensch
## @keywords emuDB attribute level Emu
## 
create.schema.attributeDefinition <- function(name, type='STRING',labelGroups=NULL){
  if(is.null(labelGroups)){
    o <- list(name=name,type=type)
  }else{
    o <- list(name=name,type=type,labelGroups=labelGroups)
  }
  class(o) <- c('emuDB.schema.attributeDefinition','list')
  invisible(o)
}

## Create emuDB level definition object
## 
## @param name name of the level
## @param type level type (ITEM,EVENT,SEGMENT)
## @param attributeDefinitions list of attribute definitions
## @return object of class emuDB.levelDefinition
## @author Klaus Jaensch
## @keywords emuDB level Emu
## 
create.schema.levelDefinition <- function(name,type=NULL,attributeDefinitions=NULL){
  if(is.null(attributeDefinitions)){
    defAttrDef=create.schema.attributeDefinition(name=name)
    attributeDefinitions=list(defAttrDef)
  }
  o <- list(name=name,type=type,attributeDefinitions=attributeDefinitions)
  class(o) <- 'emuDB.schema.levelDefinition'
  invisible(o)
}

## Create emuDB link definition object
## Represents the hierarchical information of EMU hlb files
## 
## @param name name of the link (optional)
## @param type link type ("ONE_TO_ONE", "ONE_TO_MANY", "MANY_TO_MANY")
## @param superlevelName name of the super level (link from)
## @param sublevelName name of the sublevel (link to)
## @return object of class emuDB.schema.linkDefinition
## @author Klaus Jaensch
## @keywords emuDB database schema link Emu
## 
create.schema.linkDefinition <- function(name=NULL,type,superlevelName,sublevelName){
  o <- list(name=name,type=type,superlevelName=superlevelName,sublevelName=sublevelName)
  class(o) <- 'emuDB.schema.linkDefinition'
  invisible(o)
}

create.schema.databaseDefinition <- function(name,UUID=uuid::UUIDgenerate(),mediafileBasePathPattern=NULL,mediafileExtension=NULL,ssffTrackDefinitions=list(),levelDefinitions=list(),linkDefinitions=list(),EMUwebAppConfig=NULL,annotationDescriptors=NULL,tracks=NULL,flags=NULL){
  o <- list(name=name,UUID=UUID,mediafileBasePathPattern=mediafileBasePathPattern,mediafileExtension=mediafileExtension,ssffTrackDefinitions=ssffTrackDefinitions,levelDefinitions=levelDefinitions,linkDefinitions=linkDefinitions,EMUwebAppConfig=EMUwebAppConfig,annotationDescriptors=annotationDescriptors,tracks=tracks,flags=flags)
  class(o) <- c('list','emuDB.schema.databaseDefinition')
  #rTypes=list(levelDefinitions=c('list','emuDB.schema.levelDefinition',linkDefinitions=c('list','emuDB.schema.linkDefinition')
  invisible(o)
}


## searches for all tracks needed by the EMUwebApp and
## returns their ssffTrackDefinitions
get_ssffTracks_used_by_DBconfig <- function(DBconfig){
  allTracks = NULL
  
  # anagestConfig ssffTracks
  for(ld in DBconfig$levelDefinitions){
    allTracks = c(allTracks, ld$anagestConfig$verticalPosSsffTrackName, ld$anagestConfig$velocitySsffTrackName)
  }
  
  for(p in DBconfig$EMUwebAppConfig$perspectives){
    # tracks in signalCanvases$order
    for(sco in p$signalCanvases$order){
      allTracks = c(allTracks, sco)
    }
    # tracks in twoDimCanvases$order
    for(tdco in p$twoDimCanvases$order){
      allTracks = c(allTracks, tdco)
    }
    
    # tracks in signalCanvases$assign
    for(sca in p$signalCanvases$assign){
      allTracks = c(allTracks, sca$ssffTrackName)
    }
    # tracks in p$twoDimCanvases$twoDimDrawingDefinitions
    for(tddd in p$twoDimCanvases$twoDimDrawingDefinitions){
      # dots
      for(dot in tddd$dots){
        allTracks = c(allTracks, dot$xSsffTrack, dot$ySsffTrack)
      }
    }
  }
  # uniq tracks
  allTracks = unique(allTracks)
  # remove OSCI and SPEC tracks
  allTracks = allTracks[allTracks != 'OSCI' & allTracks != 'SPEC']
  
  # get corresponding ssffTrackDefinitions
  allTrackDefs = list()
  for(std in DBconfig$ssffTrackDefinitions){
    if(std$name %in% allTracks){
      allTrackDefs[[length(allTrackDefs) + 1]] = std
    }
  }
  
  return(allTrackDefs)
}



summary.emuDB.schema.databaseDefinition<-function(schema,header=TRUE){
  cat("SSFF track definitions:\n")
  for(td in schema[['ssffTrackDefinitions']]){
    cat("\tName: ",td[['name']],"\tColumn: ",td[['columnName']],"\tExt.: ",td[['fileExtension']],"\n")
  }
  cat("\n")
  cat("Level definitions:\n")
  for(ld in schema[['levelDefinitions']]){
    print(ld)
    cat("\tAttribute definitions:\n")
    for(ad in ld[['attributeDefinitions']]){
      cat("\t")
      print(ad)
    }
    cat("\n")
  }
  cat("\n")
  lblGrps=schema[['labelGroups']]
  if(length(lblGrps)>0){
    cat("Database label group definitions:\n")
    for(lblGrp in lblGrps){
      print.emuDB.schema.labelGroup(lblGrp)
    }
  }
  cat("\n")
  cat("Link definitions:\n")
  for(ld in schema[['linkDefinitions']]){
    print(ld)
  }
  
}

print.emuDB.schema.labelGroup<-function(labelGroup){
  labelVals=c()
  for(lblGrpVal in labelGroup[['values']]){
    labelVals=c(labelVals,lblGrpVal)
  }
  cat("\tLabel group: ",labelGroup[['name']],": ",labelVals,"\n")
}

print.emuDB.schema.levelDefinition<-function(levelDefinition){
  cat(levelDefinition[['name']],"\ttype:\t",levelDefinition[['type']],"\n")
}

print.emuDB.schema.attributeDefinition<-function(attributeDefinition){
  cat(attributeDefinition[['name']],"\ttype:\t",attributeDefinition[['type']],"\n")
  lblGrps=attributeDefinition[['labelGroups']]
  if(length(lblGrps)>0){
    cat("\n\tLabel group definitions:\n")
    for(lblGrp in lblGrps){
      print.emuDB.schema.labelGroup(lblGrp)
    }
  }
}

print.emuDB.schema.linkDefinition<-function(linkDefinition){
  cat(toString(linkDefinition),"\n")
}
toString.emuDB.schema.linkDefinition<-function(linkDefinition){
  paste(linkDefinition[['superlevelName']],'->',linkDefinition[['sublevelName']],linkDefinition[['type']],sep="\t")
}

get.link.level.children<-function(schema,superlevelName){
  subLds=list()
  for(ld in schema[['linkDefinitions']]){
    if(ld[['superlevelName']]==superlevelName){
      subLds[[length(subLds)+1L]]=ld
    }
  }
  return(subLds)
}

get.root.level.names<-function(schema){
  rlNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    hasSuperLevel=FALSE
    for(ld in schema[['linkDefinitions']]){
      if(ld[['sublevelName']]==lvlD[['name']]){
        hasSuperLevel=TRUE
        break
      }
    }
    if(!hasSuperLevel){
      rlNames=c(rlNames,lvlD[['name']])
    }
  }
  return(rlNames)
}

get.level.names<-function(schema){
  lNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    lNames=c(lNames,lvlD[['name']])
  }
  return(lNames)
}

get.level.name.by.attribute.name<-function(schema,attributeName){
  for(lvlD in schema[['levelDefinitions']]){
    aNames=character(0)
    for(ad in lvlD[['attributeDefinitions']]){
      aNames=c(aNames,ad[['name']])
      if(attributeName %in% aNames){
        return(lvlD[['name']])
      }
    }
  }
  return(NULL)
}

get.attribute.names.by.name<-function(schema,levelName){
  aNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    if(lvlD[['name']]==levelName){
      for(ad in lvlD[['attributeDefinitions']]){
        aNames=c(aNames,ad[['name']])
      }
      break
    }
  }
  return(aNames)
}

get.all.attribute.names<-function(schema){
  aNames=character(0)
  for(lvlD in schema[['levelDefinitions']]){
    for(ad in lvlD[['attributeDefinitions']]){
      aNames=c(aNames,ad[['name']])
    }
    
  }
  return(aNames)
}


get.link.level.children.names<-function(schema,superlevelName){
  chNames=character(0)
  for(ld in schema[['linkDefinitions']]){
    if(ld[['superlevelName']]==superlevelName){
      chNames=c(chNames,ld[['sublevelName']])
    }
  }
  return(chNames)
}


build.level.partial.pathes<-function(schema,fromLevelName,toLevelName){
  pathes=list()
  chNames=get.link.level.children.names(schema,fromLevelName)
  if(length(chNames)==0){
    #pathes[[length(pathes)+1]]=c(levelName)
    
  }else{
    for(chName in chNames){
      if(chName==toLevelName){
        # terminated
        pathes[[length(pathes)+1L]]=c(fromLevelName,chName)
      }else{
        chPathes=build.level.partial.pathes(schema,chName,toLevelName)
        for(chPath in chPathes){
          pathes[[length(pathes)+1L]]=c(fromLevelName,chPath)
        }
      }
    }
  }
  return(pathes)
}

build.sublevel.pathes<-function(schema,levelName){
  pathes=list()
  chNames=get.link.level.children.names(schema,levelName)
  if(length(chNames)==0){
    pathes[[length(pathes)+1L]]=c(levelName)
  }else{
    for(chName in chNames){
      
      chPathes=build.sublevel.pathes(schema,chName)
      for(chPath in chPathes){
        pathes[[length(pathes)+1L]]=c(levelName,chPath)
      }
    }
  }
  return(pathes)
}

extract.link.targets.from.pathes<-function(pathes){
  mergedTargets=character(0)
  for(p in pathes){
    pLen=length(p)
    trgs=p[2:pLen]
    mergedTargets=c(mergedTargets,trgs)
  }
  uniqueTargets=unique(mergedTargets)
  return(uniqueTargets)
}

expand.link.path<-function(p){
  expPath=list()
  pLen=length(p)
  if(pLen==1){
    return(list())
  }
  expPath[[length(expPath)+1L]]=p
  expPath=c(expPath,expand.link.path(p[1:(pLen-1)]))
  return(expPath)
}

build.link.defs<-function(schema){
  extLds=list()
  for(ld in schema[['levelDefinitions']]){
    lName=ld[['name']]
    pathes=build.sublevel.pathes(schema,lName)
    for(p in pathes){
      extLds=c(extLds,expand.link.path(p))
    }
  }
  return(unique(extLds))
}

build.level.pathes<-function(schema){
  pathes=list()
  chNames=character(0)
  for(l in schema[['levelDefinitions']]){
    lPathes=build.sublevel.pathes(schema,l[['name']])
    pathes=c(pathes,lPathes)
  }
  return(pathes)
}

#
# builds "extended" link definitions
# lists link definitionsfor every possible directed connection between levels
# returns list of character vectors 
# the first element of each character vector contains the super level name of the levelDefinition,
# the follwing elements contain all exetnded linked sub level names  
build.ext.link.definitions<-function(schema){
  lds=list()
  pathes=build.level.pathes(schema)
  for(p in pathes){
    pLen=length(p)
    for(i in 1:pLen){
      ld=character(0)
      for(j in i:pLen){
        ld=c(ld,p[j])
      }
      lds[[length(lds)+1L]]=ld
    }
  }
  return(lds)
}

find.segment.levels<-function(DBconfig,attrName){
  #cat("Search SEGMENT level for ",attrName,"\n")
  lvlNm=get.level.name.by.attribute.name(DBconfig,attrName)
  extLnkDefs=build.ext.link.definitions(DBconfig)
  segLvlList=character(0)
  for(extLnkDef in extLnkDefs){
    if(extLnkDef[1]==lvlNm){
      for(trgLvlNm in extLnkDef[2:length(extLnkDef)]){
        
        trgLd=get.levelDefinition(DBconfig,trgLvlNm)
        if(trgLd['type']=='SEGMENT'){
          segLvlList=unique(c(segLvlList,trgLvlNm))
        }
      }
    }
  }
  #cat("SEGMENT levels for ",attrName,": ",segLvlList,"\n")
  return(segLvlList)
}

# persistence filters
# the properties listed are not persisted to JSON files
emuR.persist.filters.DBconfig=list()
emuR.persist.filters.DBconfig[[1]]=c('annotationDescriptors')
emuR.persist.filters.DBconfig[[2]]=c('tracks')
emuR.persist.filters.DBconfig[[3]]=c('flags')
emuR.persist.filters.DBconfig[[4]]=c('ssffTrackDefinitions','basePath')
emuR.persist.filters.DBconfig[[5]]=c('mediafileBasePathPattern')
emuR.persist.filters.DBconfig[[6]]=c('maxNumberOfLabels')
emuR.persist.filters.DBconfig[[7]]=c('itemColNames')
emuR.persist.filters.DBconfig[[8]]=c('basePath')
emuR.persist.filters.DBconfig[[9]]=c('DBconfigPath')

# persistent class hierarchy
# the calss names are applied to the object hierarchy loaded from JSON file
emuR.persist.class.DBconfig=list()
emuR.persist.class.DBconfig[['emuDB.schema.databaseDefinition']]=character(0)
emuR.persist.class.DBconfig[['emuDB.schema.levelDefinition']]=list(c('levelDefinitions','*'))
emuR.persist.class.DBconfig[['emuDB.schema.linkDefinition']]=list(c('linkDefinitions','*'))
emuR.persist.class.DBconfig[['emuDB.schema.attributeDefinition']]=list(c('levelDefinitions','*','attributeDefinitions','*'))

load.emuDB.DBconfig<-function(DBconfigFilePath){
  # with warn=TRUE and some files
  # R complains about incomplete (last) line
  # See https://stat.ethz.ch/pipermail/r-help/2006-July/108654.html
  # TODO does problem with jsonlite still exist ?
  dbCfgJSONLns=readLines(DBconfigFilePath,warn=FALSE)
  dbCfgJSON=paste(dbCfgJSONLns,collapse='')
  dbCfgPersisted=jsonlite::fromJSON(dbCfgJSON,simplifyVector=FALSE)
  
  # unmarshal schema object (set class names)
  schema=unmarshal.from.persistence(dbCfgPersisted,emuR.persist.class.DBconfig)
  return(schema)
}

# TODO
.store.schema<-function(db,projectDir=NULL){
  
  if(is.null(projectDir)){
    projectDir=db[['basePath']]
  }
  # store db schema file
  dbCfgNm=paste0(db[['name']],database.schema.suffix)
  dbCfgPath=file.path(projectDir,dbCfgNm)
  
  persistFilter=emuR.persist.filters.DBconfig
  sp=marshal.for.persistence(db[['DBconfig']],persistFilter)
  psJSON=jsonlite::toJSON(sp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
  writeLines(psJSON,dbCfgPath)
  MD5DBconfigJSON = md5sum(dbCfgPath)
  .store.DBconfig.DBI(con = get_emuDBcon(db$DBconfig$UUID),DBconfig = db[['DBconfig']], MD5DBconfigJSON)
}

# TODO 
.store.DBconfig<-function(con,basePath,DBconfig){
  
  # store db schema file
  dbCfgNm=paste0(DBconfig[['name']],database.schema.suffix)
  dbCfgPath=file.path(basePath,dbCfgNm)
  
  persistFilter=emuR.persist.filters.DBconfig
  sp=marshal.for.persistence(DBconfig,persistFilter)
  psJSON=jsonlite::toJSON(sp,auto_unbox=TRUE,force=TRUE,pretty=TRUE)
  writeLines(psJSON,dbCfgPath)
  .store.DBconfig.DBI(con, DBconfig = DBconfig)
}


get.levelDefinition <- function(DBconfig, name){
  res = NULL
  for(ld in DBconfig$levelDefinitions){
    if(ld$name == name){
      res = ld
      break
    }
  }
  return(res)
}

###########################################
# CRUD operation for levelDefinitions

##' Add / List / Remove level definition to / of / from emuDB
##' 
##' Add / List / Remove database operation functions for level definitions. 
##' A level is a more general term for what is often referred to as a "tier". 
##' It is more general in the sense that people usually 
##' expect tiers to contain time information. Levels 
##' can either contain time information if they are of the 
##' type "EVENT" or of the type "SEGMENT" but are timeless 
##' if they are of the type "ITEM". For more information 
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' Note that a level cannot be removed, if it contains instances of annotation items
##' or if it is linked to another level.
##' 
##' @param dbName name of loaded emuDB
##' @param name name of level definition
##' @param type type of level definition ("SEGMENT","EVENT","ITEM")
##' @param store changes to file system (TRUE), changes only in memory (FALSE)
##' @param dbUUID optional UUID of loaded emuDB, in case dbName is ambique
##' @keywords emuDB database schema Emu
##' @name AddListRemoveLevelDefinitions
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # add level called "Phonetic2" to the "ae" emuDB
##' # that could for example contain the transcriptions of a second annotator
##' add_levelDefinition(dbName = "ae",
##'                     name = "Phonetic2",
##'                     type = "SEGMENT")
##'                     
##' # list level definition of "ae" emuDB
##' list_levelDefinitions(dbName = "ae")
##' 
##' # remove newly added level definition
##' remove_levelDefinitions(dbName = "ae",
##'                         name = "Phonetic2")
##' }
##' 
NULL

##' @rdname AddListRemoveLevelDefinitions
##' @export
add_levelDefinition<-function(dbName, name,
                              type, store = TRUE,
                              dbUUID=NULL){
  
  allowedTypes = c('ITEM', 'SEGMENT', 'EVENT')
  # precheck type 
  if(!(type %in% allowedTypes)){
    stop('Bad type given! Type has to be either ', paste(allowedTypes, collapse = ' | ') )
  }
  levelDefinition=create.schema.levelDefinition(name = name,type = type)
  db=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  # check if level definition (name) already exists 
  for(ld in db[['DBconfig']][['levelDefinitions']]){
    if(ld[['name']]==levelDefinition[['name']]){
      stop("Level definition:",levelDefinition[['name']]," already exists in database ",db[['name']])
    }
  }
  # add
  db[['DBconfig']][['levelDefinitions']][[length(db[['DBconfig']][['levelDefinitions']])+1]]=levelDefinition
  
  # update transient values
  db[['DBconfig']]=.update.transient.schema.values(db[['DBconfig']])
  
  # store to disk
  if(store){
    .store.schema(db)
  }else{
    .store.DBconfig.DBI(db$DBconfig)
  }
  invisible(NULL)
}


##' @rdname AddListRemoveLevelDefinitions
##' @export
list_levelDefinitions <- function(dbName, dbUUID=NULL){
  dbObj = .load.emuDB.DBI(name = dbName, uuid = dbUUID)
  df <- data.frame(name=character(),
                   type=character(), 
                   nrOfAttrDefs=numeric(), 
                   stringsAsFactors=FALSE) 
  
  for(ld in dbObj$DBconfig$levelDefinitions){
    df <- rbind(df, data.frame(name = ld$name, 
                               type = ld$type, 
                               nrOfAttrDefs = length(ld$attributeDefinitions),
                               stringsAsFactors = FALSE)) # perfomance problem? 
  }
  return(df)
}


##' @rdname AddListRemoveLevelDefinitions
##' @export
remove_levelDefinition<-function(dbName,name,dbUUID=NULL){
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  dbUUID = get_UUID(dbName = dbName, dbUUID = dbUUID)
  # check if level definition (name)exists 
  if(!any(sapply(dbObj[['DBconfig']][['levelDefinitions']],function(ld) ld[['name']]==name))){
    stop("Level definition:",name," does not exist in database ",dbObj[['name']])
  }
  # check if level is referenced by link defintion
  for(lkd in dbObj[['DBconfig']][['linkDefinitions']]){
    if(lkd[['superlevelName']]==name |  lkd[['sublevelName']]==name){
      lkdStr=toString(lkd)
      stop("Cannot remove level definition ",name,". It is referenced by link definition: ",lkdStr)
    }
  }
  
  # check if level is empty
  itemsDf=dbGetQuery(get_emuDBcon(dbUUID),paste0("SELECT * FROM items i WHERE \
                        i.db_uuid='",dbObj$DBconfig$UUID,"' AND i.level='",name,"'"))
  itemsCnt=nrow(itemsDf)
  if(itemsCnt>0){
    stop("Level is not empty. Remove items first to delete level ",name)
  }
  
  # do removal
  newLvlDefs=list()
  for(lvlDef in dbObj[['DBconfig']][['levelDefinitions']]){
    if(lvlDef[['name']]!=name){
      newLvlDefs[[length(newLvlDefs)+1]]=lvlDef
    }
  }
  dbObj[['DBconfig']][['levelDefinitions']]=newLvlDefs
  
  # update transient values
  dbObj[['DBconfig']]=.update.transient.schema.values(dbObj[['DBconfig']])
  
  # store to disk
  .store.schema(dbObj)
  
  return(invisible(NULL))
}

###################################################
# CRUD operations for attributeDefinitions

##' Add / List / Remove attribute definition to / of / from emuDB
##' 
##' Add / List / Remove database operation functions for attribute definition 
##' to / of / from an existing level definition
##' of a emuDB. Attribute
##' definitions can be viewed as definitions of
##' parallel labels for the annotational units (ITEMs) of the emuDB. 
##' Each level definition is required to have at least one 
##' default attribute definition that has the same name as the level definition
##' (automatically created by \code{\link{add_levelDefinition}}). For more 
##' information on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' Note that as with level definitions, an attribute definition to a level cannot be removed,
##' if it contains labels in the emuDB.
##' 
##' @param dbName name of loaded emuDB
##' @param levelName name of level
##' @param name name of attributeDefinition
##' @param type type of attributeDefinition (currently only "STRING")
##' @param dbUUID optional UUID of loaded emuDB, in case dbName is ambique
##' @keywords emuDB database DBconfig Emu 
##' @name AddListRemoveAttributeDefinitions
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # add additional attribute definition to the "Phonetic" level
##' # of the "ae" emuDB that will contain the UTF8 IPA
##' # symbols of the phonetic transcriptions
##' add_attributeDefinition(dbName = "ae",
##'                         levelName = "Phonetic",
##'                         name = "IPA-UTF8")
##'                         
##' # list attribute definitions for level "Word"
##' # of the "ae" emuDB
##' list_attributeDefinitions(dbName = "ae", 
##'                           levelName = "Word")
##' 
##' # remove newly added attributeDefinition
##' remove_attributeDefinition(dbName = "ae",
##'                            levelName = "Phonetic",
##'                            name = "IPA-UTF8")
##' }
##' 
NULL

##' @rdname AddListRemoveAttributeDefinitions
##' @export
add_attributeDefinition <- function(dbName, levelName, 
                                    name, type = "STRING",
                                    dbUUID=NULL){
  if(type != "STRING"){
    stop("Currently only attributeDefinition of type 'STRING' allowed")
  }
  
  dbObj=.load.emuDB.DBI(name=dbName, uuid = dbUUID)
  
  df = list_attributeDefinitions(dbName, levelName, dbUUID)
  
  
  if(!(name %in% df$name)){
    for(i in 1:length(dbObj$DBconfig$levelDefinitions)){
      if(dbObj$DBconfig$levelDefinitions[[i]]$name == levelName){
        dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[length(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions) + 1]] = create.schema.attributeDefinition(name = name, 
                                                                                                                                                                               type = type,
                                                                                                                                                                               labelGroups = NULL)
        break
      }
    }
  }else{
    stop(paste0("attributeDefinition with name '", name, "' already present in level '", levelName, "'"))
  }
  
  # store changes
  .store.schema(dbObj)
  
}


##' @rdname AddListRemoveAttributeDefinitions
##' @export
list_attributeDefinitions <- function(dbName, levelName, dbUUID=NULL){
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  ld = get.levelDefinition(dbObj$DBconfig, levelName)
  
  if(length(ld$attributeDefinitions) > 1){
    df = data.frame(name = character(), 
                    type = character(), 
                    hasLabelGroups = logical(), 
                    hasLegalLabels = logical(), 
                    stringsAsFactors = F)
    for(ad in ld$attributeDefinitions){
      df = rbind(df, df = data.frame(name = ad$name, 
                                     type = ad$type, 
                                     hasLabelGroups = !is.null(ad$labelGroups),
                                     hasLegalLabels = !is.null(ad$legalLabels),
                                     stringsAsFactors = F))
    }
  }else{
    df <- data.frame(name=ld$attributeDefinitions[[1]]$name, 
                     type=ld$attributeDefinitions[[1]]$type,
                     hasLabelGroups = !is.null(ld$attributeDefinitions[[1]]$labelGroups),
                     hasLegalLabels = !is.null(ld$attributeDefinitions[[1]]$legalLabels),
                     stringsAsFactors = F)
  }
  rownames(df) <- NULL
  return(df)
}


##' @rdname AddListRemoveAttributeDefinitions
##' @export
remove_attributeDefinition <- function(dbName, 
                                       levelName, 
                                       name, 
                                       dbUUID = NULL){
  
  if(levelName == name){
    stop("Can not remove primary attributeDefinition (attributeDefinition with same name as level)")
  }
  
  uuid=get_UUID(dbName, dbUUID)
  dbObj = .load.emuDB.DBI(uuid = uuid)
  
  ld = get.levelDefinition(dbObj$DBconfig, levelName)
  
  # check if instances are present
  qRes = dbGetQuery(get_emuDBcon(uuid), paste0("SELECT * FROM items AS it, labels AS lb WHERE ",
                                               "it.db_uuid = lb.db_uuid AND ", 
                                               "it.session = lb.session AND ", 
                                               "it.bundle = lb.bundle AND ",
                                               "it.itemID = lb.itemID AND ",
                                               "it.level = '", levelName, "' AND ",
                                               "lb.name = '", name, "'"))
  
  if(nrow(qRes) > 0){
    stop("Can not remove attributeDefinition if there are labels present")
  }else{
    levDefIdx = NULL
    for(i in 1:length(dbObj$DBconfig$levelDefinitions)){
      if(dbObj$DBconfig$levelDefinitions[[i]]$name == levelName){
        levDefIdx = i
        break
      }
    }
    
    for(i in 1:length(dbObj$DBconfig$levelDefinitions[[levDefIdx]]$attributeDefinitions)){
      if(dbObj$DBconfig$levelDefinitions[[levDefIdx]]$attributeDefinitions[[i]]$name == name){
        dbObj$DBconfig$levelDefinitions[[levDefIdx]]$attributeDefinitions[[i]] = NULL
        break
      }
    }  
  }
  
  # store changes
  .store.schema(dbObj)
  
}

###################################################
# CRUD operations for legalLabels

##' Set / Get / Remove legal labels of attributeDefinition of emuDB
##' 
##' Set / Get / Remove legal labels of a specific attributeDefinition of a emuDB. 
##' The legal labels are a character vector of strings
##' that specifies the labels that are legal (i.e. allowed / valid) for the given attribute. 
##' As the EMU-webApp won't allow the annotator to enter any labels that are not 
##' specified in this array, this is a simple way of assuring that a level 
##' has a consistent label set. For more information 
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' Note that defining legal labels for an attributeDefinition does not imply that the 
##' existing labels are checked for being 'legal' in the emuDB.
##' 
##' @param dbName name of loaded emuDB
##' @param levelName name of level
##' @param attributeDefinitionName name of attributeDefinition (can be and often is the levelName)
##' @param legalLabels character vector of labels
##' @param dbUUID optional UUID of loaded emuDB, in case the dbName is ambiguous
##' @keywords emuDB database schema Emu
##' @name SetGetRemoveLegalLabels
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' legalPhoneticLabels = c("V", "m", "N", "s", "t", "H", "@:", "f", "r", 
##'                         "E", "n", "z", "S", "i:", "w", "@", "k", "I", "d", 
##'                         "db", "j", "u:", "dH", "l", "ai", "O", "D", "o:", "v")
##' 
##' # set legal labels of the 
##' # default "Phonetic" attributeDefinition of
##' # the "Phonetic" level of "ae" emuDB
##' set_legalLabels(dbName = "ae", 
##'                 levelName = "Phonetic",
##'                 attributeDefinitionName = "Phonetic",
##'                 legalLabels = legalPhoneticLabels)
##' 
##' # get legal labels of the 
##' # default "Phonetic" attributeDefinition of
##' # the "Phonetic" level of "ae" emuDB
##' get_legalLabels(dbName = "ae", 
##'                 levelName = "Phonetic", 
##'                 attributeDefinitionName = "Phonetic")
##'                 
##' 
##' # remove legal labels of the 
##' # default "Phonetic" attributeDefinition of
##' # the "Phonetic" level of "ae" emuDB
##' remove_legalLabels(dbName = "ae", 
##'                    levelName = "Phonetic", 
##'                    attributeDefinitionName = "Phonetic")
##'                 
##' }
##' 
NULL

##' @rdname SetGetRemoveLegalLabels
##' @export
set_legalLabels <- function(dbName,
                            levelName,
                            attributeDefinitionName,
                            legalLabels,
                            dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  
  for(i in 1:length(dbObj$DBconfig$levelDefinitions)){
    for(j in 1:length(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions)){
      if(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$name == attributeDefinitionName){
        dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$legalLabels = legalLabels
      }
    }
  }
  
  # store changes
  .store.schema(dbObj)
  
}


##' @rdname SetGetRemoveLegalLabels
##' @export
get_legalLabels <- function(dbName,
                            levelName,
                            attributeDefinitionName, 
                            dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  ld = get.levelDefinition(dbObj$DBconfig, levelName)
  
  ll = NULL
  for(ad in ld$attributeDefinitions){
    if(ad$name == attributeDefinitionName){
      if(!is.null(ad$legalLabels)){
        ll = unlist(ad$legalLabels)
      }else{
        ll = NA
      }
    }
  }
  
  return(ll)
}


##' @rdname SetGetRemoveLegalLabels
##' @export
remove_legalLabels <- function(dbName,
                               levelName,
                               attributeDefinitionName, 
                               dbUUID = NULL){
  # remove by setting to NULL
  set_legalLabels(dbName, 
                  levelName,
                  attributeDefinitionName,
                  legalLabels = NULL)
}

###################################################
# CRUD operations for attributeDefinition$labelGroups

##' Add / List / Remove labelGroup to / of / from attributeDefinition of emuDB
##' 
##' Add / List / Remove label group to / of / from a specific attribute definition. 
##' This label group can be used as a short hand  
##' to reference groups of labels specific
##' to an attribute definition (compared to global label groups that 
##' are added by \code{\link{add_labelGroup}}) in a 
##' \code{\link{query}}. A common example would be to
##' add a label group for something like the phonetic
##' category of nasals to be able reference them 
##' as "nasals" in a \code{\link{query}}. For more information 
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' In theory you could use a labelGroupName as a label instance within the
##' level, but since this could lead to serious confusion, you'll better avoid doing that.
##' Do not confuse a labelGroup with legal labels: the labelGroup 
##' used to be called 'legal labels' in the legacy Emu system.  
##' 
##' 
##' @param dbName name of loaded emuDB
##' @param levelName name of level
##' @param attributeDefinitionName name of attributeDefinition
##' @param labelGroupName name of label group
##' @param labelGroupValues character vector of labels
##' @param dbUUID optional UUID of loaded emuDB, in case the dbName is ambiguous
##' @keywords emuDB database schema Emu
##' @seealso add_labelGroup
##' @name AddListRemoveAttrDefLabelGroup
##' @examples
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' sampaNasals = c("m", "F", "n", "J", "N")
##' 
##' # add these values to the default Phonetic attribute
##' # definition of the Phonetic level of the "ae" emuDB
##' add_attrDefLabelGroup(dbName = "ae",
##'                       levelName = "Phonetic",
##'                       attributeDefinitionName = "Phonetic",
##'                       labelGroupName = "sampaNasals",
##'                       labelGroupValues = sampaNasals)
##' 
##' # query the labelGroup
##' query("ae", "Phonetic==sampaNasals")
##' 
##' 
##' # list attribute definition label groups
##' # of attributeDefinition "Phonetic" of the level "Phonetic"
##' # of the "ae" emuDB
##' list_attrDefLabelGroups(dbName = "ae", 
##'                         levelName = "Phonetic" , 
##'                         attributeDefinitionName = "Phonetic")
##' 
##' # remove the newly added attrDefLabelGroup
##' remove_attrDefLabelGroup(dbName = "ae",
##'                          levelName = "Phonetic",
##'                          attributeDefinitionName = "Phonetic",
##'                          labelGroupName = "sampaNasals")
##' 
##' }
##' 
NULL

##' @rdname AddListRemoveAttrDefLabelGroup
##' @export
add_attrDefLabelGroup <- function(dbName,
                                  levelName,
                                  attributeDefinitionName, 
                                  labelGroupName,
                                  labelGroupValues,
                                  dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  curLgs = list_attrDefLabelGroups(dbName, 
                                   levelName, 
                                   attributeDefinitionName)
  
  if(labelGroupName %in% curLgs$name){
    stop("labelGroupName '", labelGroupName ,"' already exists!")
  }
  for(i in 1:length(dbObj$DBconfig$levelDefinitions)){
    for(j in 1:length(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions)){
      if(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$name == attributeDefinitionName){
        l = length(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups)
        dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups[[l + 1]] = list(name = labelGroupName, 
                                                                                                   values = labelGroupValues)
      }
    }
  }
  
  # store changes
  .store.schema(dbObj)
}

##' @rdname AddListRemoveAttrDefLabelGroup
##' @export
list_attrDefLabelGroups <- function(dbName,
                                    levelName,
                                    attributeDefinitionName, 
                                    dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  ld = get.levelDefinition(dbObj$DBconfig, levelName)
  
  df = data.frame(name = character(), 
                  values = character(),
                  stringsAsFactors = F)
  for(ad in ld$attributeDefinitions){
    if(ad$name == attributeDefinitionName){
      if(!is.null(ad$labelGroups)){
        for(lg in ad$labelGroups){
          df = rbind(df, data.frame(name = lg$name,
                                    values = paste0(lg$values, collapse = "; ") ))
        }
      }
    }
  }
  
  return(df)
}


##' @rdname AddListRemoveAttrDefLabelGroup
##' @export
remove_attrDefLabelGroup <- function(dbName,
                                     levelName,
                                     attributeDefinitionName, 
                                     labelGroupName,
                                     dbUUID = NULL){
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  curLgs = list_attrDefLabelGroups(dbName, 
                                   levelName, 
                                   attributeDefinitionName)
  
  if(!labelGroupName %in% curLgs$name){
    stop("labelGroupName '", labelGroupName ,"' does not exists!")
  }
  
  for(i in 1:length(dbObj$DBconfig$levelDefinitions)){
    for(j in 1:length(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions)){
      if(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$name == attributeDefinitionName){
        l = length(dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups)
        dbObj$DBconfig$levelDefinitions[[i]]$attributeDefinitions[[j]]$labelGroups[[l]] = NULL
      }
    }
  }
  
  # store changes
  .store.schema(dbObj)
  
}

###################################################
# CRUD operations for linkDefinitions

##' Add / List / Remove linkDefinition to / of / from emuDB
##' 
##' Add / List / Remove new link definition to / of / from emuDB. A link definition
##' specifies the relationship between two levels, the
##' super-level and the sub-level. The entirety of all link 
##' definitions of a emuDB specifies the 
##' hierarchical structure of the database. For more information
##' on the structural elements of an emuDB see \code{vignette(emuDB)}.
##' 
##' Link type descriptions:
##' \itemize{
##' \item{\code{"ONE_TO_MANY"}}{A single ITEM of the super-level can be linked to multiple ITEMs of the sub-level}
##' \item{\code{"MANY_TO_MANY"}}{Multiple ITEMs of the super-level can be linked to multiple ITEMs of the sub-level}
##' \item{\code{"ONE_TO_ONE"}}{A single ITEM of the super-level can be linked to a single ITEM of the sub-level}
##' }
##' 
##' For all link types the rule applies that no links are allowed to cross any other links.
##' A linkDefinition cannot be removed, if there are links present in the emuDB.
##' 
##' @param dbName name of emuDB
##' @param type type of linkDefinition (either \code{"ONE_TO_MANY"}, \code{"MANY_TO_MANY"} or \code{"ONE_TO_ONE"})
##' @param superlevelName name of super-level of linkDefinition
##' @param sublevelName name of sub-level of linkDefinition
##' @param dbUUID optional UUID of emuDB, in case the dbName is ambiguous
##' @name AddListRemoveLinkDefinition
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded emuDB called "myTGcolDB" that was converted
##' # using the convert_TextGridCollection_to_emuDB function
##' # (see ?create_emuRdemoData and ?convert_TextGridCollection_to_emuDB for more information)
##' 
##' # add link definition from super-level "Phoneme"
##' # to sub-level "Phonetic" of type "ONE_TO_MANY"
##' # for "myTGcolDB" emuDB
##' add_linkDefinition(dbName = "myTGcolDB",
##'                    type = "ONE_TO_MANY",
##'                    superlevelName = "Phoneme",
##'                    sublevelName = "Phonetic")
##' 
##' # list link definitions for "myTGcolDB" emuDB
##' list_linkDefinitions(dbName = "myTGcolDB")
##' 
##' # remove newly added link definition
##' remove_linkDefinition(dbName = "myTGcolDB",
##'                       superlevelName = "Phoneme",
##'                       sublevelName = "Phonetic")
##' 
##' 
##' }
NULL

##' @rdname AddListRemoveLinkDefinition
##' @export
add_linkDefinition <- function(dbName, 
                               type,
                               superlevelName,
                               sublevelName,
                               dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  
  allowedTypes = c("ONE_TO_MANY", "MANY_TO_MANY", "ONE_TO_ONE")
  
  if(!type %in% allowedTypes){
    stop("Only the following types permitted: ", paste(allowedTypes, collapse = '; '))
  }
  
  curLds = list_linkDefinitions(dbName = dbName, dbUUID = dbUUID)
  
  # check if level is defined
  curLevs = list_levelDefinitions(dbName = dbName, dbUUID = dbUUID)
  if(!any(curLevs$name == superlevelName) | !any(curLevs$name == sublevelName)){
    stop("Either superlevelName or sublevelName are not defined")
  }
  
  
  # check if link between levels already exists
  if(any(curLds$superlevelName == superlevelName & curLds$sublevelName == sublevelName)){
    stop("linkDefinition already exists for superlevelName: '", 
         superlevelName, "' and sublevelName: '", sublevelName, "'")
  }
  
  l = length(dbObj$DBconfig$linkDefinitions)
  dbObj$DBconfig$linkDefinitions[[l + 1]] = list(type = type, 
                                                 superlevelName = superlevelName,
                                                 sublevelName = sublevelName)
  
  # store changes
  .store.schema(dbObj)
  
}


##' @rdname AddListRemoveLinkDefinition
##' @export
list_linkDefinitions <- function(dbName, dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  
  df = data.frame(type = character(),
                  superlevelName = character(),
                  sublevelName = character(),
                  stringsAsFactors = F)
  
  for(ld in dbObj$DBconfig$linkDefinitions){
    df = rbind(df, data.frame(type = ld$type,
                              superlevelName = ld$superlevelName,
                              sublevelName = ld$sublevelName))
  }
  
  return(df)
  
}


##' @rdname AddListRemoveLinkDefinition
##' @export
remove_linkDefinition <- function(dbName, 
                                  superlevelName,
                                  sublevelName,
                                  dbUUID = NULL){
  
  dbObj = .load.emuDB.DBI(uuid = dbUUID,name=dbName)
  dbUUID = get_UUID(dbName = dbName, dbUUID = dbUUID)
  
  curLds = list_linkDefinitions(dbName = dbName, dbUUID = dbUUID)
  
  # check if linkDef exists
  if(!any(curLds$superlevelName == superlevelName & curLds$sublevelName == sublevelName)){
    stop("No linkDefinition found for superlevelName '", superlevelName, 
         "' and sublevelName '", sublevelName, "'")
  }
  # check if links are present
  res = dbGetQuery(get_emuDBcon(dbUUID), paste0("SELECT * FROM ",
                                                "links ",
                                                "INNER JOIN (SELECT * FROM items WHERE level = '", superlevelName, "' AND db_uuid = '", dbObj$DBconfig$UUID, "') as superItems", 
                                                "    ON links.fromID = superItems.itemID ",
                                                "       AND links.db_uuid = superItems.db_uuid ",
                                                "       AND links.session = superItems.session ",
                                                "       AND links.bundle = superItems.bundle ",
                                                "INNER JOIN (SELECT * FROM items WHERE level = '", sublevelName, "' AND db_uuid = '", dbObj$DBconfig$UUID, "') as subItems", 
                                                "    ON links.toID = subItems.itemID ",
                                                "       AND links.db_uuid = subItems.db_uuid ",
                                                "       AND links.session = subItems.session ",
                                                "       AND links.bundle = subItems.bundle ",
                                                "WHERE links.db_uuid = '", dbObj$DBconfig$UUID, "'"))
  
  if(nrow(res) != 0){
    stop("linkDefinition can not be remove as there are links present")
  }
  
  for(i in 1:length(dbObj$DBconfig$linkDefinitions)){
    if(dbObj$DBconfig$linkDefinitions[[i]]$superlevelName == superlevelName && dbObj$DBconfig$linkDefinitions[[i]]$sublevelName == sublevelName){
      dbObj$DBconfig$linkDefinitions[[i]] = NULL
    }
  }
  
  # store changes
  .store.schema(dbObj)
  
}

###################################################
# CRUD operations for ssffTrackDefinitions

##' Add / List / Remove ssffTrackDefinition to / from / of emuDB
##' 
##' Add / List / Remove ssffTrackDefinition to / from / of emuDB. 
##' An ssffTrack (often simply referred to as a track) references 
##' data that is stored in the Simple Signal File Format (SSFF) 
##' in the according bundle folders. The two most common types of data are:
##' \itemize{
##' \item{complementary data that was acquired during the recording 
##' such as data acquired during electromagnetic 
##' articulographic (EMA) or electropalatography (EPG) recordings;}
##' \item{derived data, i.e. data that was calculated from the original audio signal 
##' such as formant values and their bandwidths or the short-term Root Mean Square amplitude of the signal.}
##' }
##' For more information on the structural elements of an emuDB see \code{vignette(emuDB)}.
##'
##' @param dbName name of emuDB
##' @param name name of ssffTrackDefinition
##' @param columnName columnName of ssffTrackDefinition.
##' If the \code{onTheFlyFunctionName} parameter is set and columnName isn't, the
##' \code{columnName} will default to the first entry in \code{wrasspOutputInfos[[onTheFlyFunctionName]]$tracks}.
##' @param fileExtension fileExtension of ssffTrackDefinitions.
##' If the \code{onTheFlyFunctionName} parameter is set and fileExtension isn't, the
##' \code{fileExtension} will default to the first entry in \code{wrasspOutputInfos[[onTheFlyFunctionName]]$ext}.
##' @param onTheFlyFunctionName name of wrassp function to do on-the-fly calculation. If set to the name of a wrassp 
##' signal processing function, not only the emuDB schema is extended by the ssffTrackDefintion but also 
##' the track istself is calculated from the signal file and stored in the emuDB. See \code{names(wrasspOutputInfos)}
##' for a list of all the signal processing functions provided by the wrassp package.
##' @param onTheFlyParams a list of parameters that will be given to the function 
##' passed in by the onTheFlyFunctionName parameter. This list can easily be 
##' generated using the \code{formals} function on the according signal processing function 
##' provided by the wrassp package and then setting the
##' parameter one wishes to change.     
##' @param onTheFlyOptLogFilePath path to optional log file for on-the-fly function
##' @param deleteFiles delete files that belong to ssffTrackDefinition on removal
##' @param verbose Show progress bars and further information
##' @param interactive ask user for confirmation
##' @param dbUUID optional UUID of emuDB, in case dbName is ambiguous
##' @seealso wrasspOutputInfos
##' @name AddListRemoveSsffTrackDefinition
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' # add ssff track definition to "ae" emuDB
##' # calculating the according SSFF files .zcr on-the-fly
##' # using the wrassp function "zcrana" (zero-crossing-rate analysis)
##' add_ssffTrackDefinition(dbName = "ae",
##'                         name = "ZCRtrack",
##'                         onTheFlyFunctionName = "zcrana")
##'                         
##' # add ssff track definition to "ae" emuDB
##' # for SSFF files that will be added later (either
##' # by adding files to the emuDB using 
##' # the add_files() function or by calculating
##' # them using the according function provided 
##' # by the wrassp package)
##' add_ssffTrackDefinition(dbName = "ae",
##'                         name = "formants",
##'                         columnName = "fm",
##'                         fileExtension = "fms")
##' 
##' # list ssff track definitions for "ae" emuDB
##' list_ssffTrackDefinitions(dbName = "ae")
##' 
##' # remove newly added ssff track definition, but not the .zrc files in the emuDB
##' remove_ssffTrackDefinition <- function(dbName = "ae", 
##'                                        name = "ZCRtrack")
##' 
##' }
##' 
NULL

##' @rdname AddListRemoveSsffTrackDefinition
##' @export
add_ssffTrackDefinition <- function(dbName, name, 
                                    columnName = NULL, fileExtension = NULL, 
                                    onTheFlyFunctionName = NULL, onTheFlyParams = NULL, 
                                    onTheFlyOptLogFilePath = NULL, dbUUID = NULL,
                                    verbose = TRUE, interactive = TRUE){
  # .initialize.DBI.database()
  uuid=get_UUID(dbName,dbUUID)
  dbObj = .load.emuDB.DBI(uuid = uuid)
  
  #########################
  # parameter checks
  
  # set columnName to fist tracks entry in wrasspOutputInfos if columnName is not set
  if(!is.null(onTheFlyFunctionName) && is.null(columnName)){
    columnName = wrasspOutputInfos[[onTheFlyFunctionName]]$tracks[1]
  }

  # set fileExtension to fist ext entry in wrasspOutputInfos if fileExtension is not set
  if(!is.null(onTheFlyFunctionName) && is.null(fileExtension)){
    fileExtension = wrasspOutputInfos[[onTheFlyFunctionName]]$ext[1]
  }
  
  
  # check if three main parameters are not null
  if(is.null(name) || is.null(columnName) || is.null(fileExtension)){
    stop('name, columnName, fileExtension have to be set!')
  }
  
  # check if onTheFlyFunctionName is set if onTheFlyParams is
  if(is.null(onTheFlyFunctionName) && !is.null(onTheFlyParams)){
    stop('onTheFlyFunctionName has to be set if onTheFlyParams is set!')
  }
  
  # check if both onTheFlyFunctionName and onTheFlyParams are set if onTheFlyOptLogFilePath is 
  if( !is.null(onTheFlyOptLogFilePath) && (is.null(onTheFlyFunctionName) || is.null(onTheFlyParams))){
    stop('Both onTheFlyFunctionName and onTheFlyParams have to be set for you to be able to use the onTheFlyOptLogFilePath parameter!')
  }
  
  
  curDefs = list_ssffTrackDefinitions(dbName, dbUUID)
  
  if(sum(curDefs$name == name) != 0){
    stop("ssffTrackDefinitions with name ", name ," already exists for emuDB: ", dbName, "!")
  }
  
  
  # calculate new files
  if(!is.null(onTheFlyFunctionName)){
    # check if files exist
    fp = list_bundleFilePaths(dbName=dbName, fileExtension, dbUUID=dbUUID)
    ans = 'y'
    if(length(fp) != 0){
      if(interactive){
        ans = readline(paste0("There are files present in '",dbName,"' that have the file extention '", 
                              fileExtension, "' Continuing will overwrite these files! Do you wish to proceed? (y/n) "))
      }
    }else{
      if(ans == 'y'){
        
        ###############################
        # set up function formals
        funcFormals = formals(onTheFlyFunctionName)
        funcFormals[names(onTheFlyParams)] = onTheFlyParams
        funcFormals$optLogFilePath = onTheFlyOptLogFilePath
        funcFormals$listOfFiles = list_bundleFilePaths(dbName=dbName, dbObj$DBconfig$mediafileExtension, dbUUID=dbUUID)
        
        # check if columnName is valid track
        if(!(columnName %in% wrasspOutputInfos[[onTheFlyFunctionName]]$tracks)){
          stop("'", columnName ,"' is not a column produced by '", onTheFlyFunctionName, "'! Please check wrasspOutputInfos for information on the tracks of each wrassp function.")
        }
        
        do.call(onTheFlyFunctionName, funcFormals)
      }else{
        stop('Aborted by user...')
      }
    }
  }
  
  # add new ssffTrackDefinition
  dbObj$DBconfig$ssffTrackDefinitions[[length(dbObj$DBconfig$ssffTrackDefinitions) + 1]] = list(name = name, 
                                                                                                columnName = columnName,
                                                                                                fileExtension = fileExtension)
  # store changes
  .store.schema(dbObj)
}

##' @rdname AddListRemoveSsffTrackDefinition
##' @export
list_ssffTrackDefinitions <- function(dbName, dbUUID = NULL){
  # .initialize.DBI.database()
  uuid=get_UUID(dbName,dbUUID)
  dbObj = .load.emuDB.DBI(uuid = uuid)
  
  df <- do.call(rbind, lapply(dbObj$DBconfig$ssffTrackDefinitions, data.frame, stringsAsFactors=FALSE))
  return(df)
}


##' @rdname AddListRemoveSsffTrackDefinition
##' @export
remove_ssffTrackDefinition <- function(dbName, name, 
                                       deleteFiles = FALSE, dbUUID = NULL){
  # .initialize.DBI.database()
  uuid=get_UUID(dbName,dbUUID)
  dbObj = .load.emuDB.DBI(uuid = uuid)
  
  # precheck if exists
  sDefs = list_ssffTrackDefinitions(dbName, dbUUID)  
  
  if(!(name %in% sDefs$name)){
    stop("No ssffTrackDefinitions found with name: '", name, "'")
  }
  # find end delete entry
  deletedDef = NULL
  for(i in 1:length(dbObj$DBconfig$ssffTrackDefinitions)){
    if(dbObj$DBconfig$ssffTrackDefinitions[[i]]$name == name){
      deletedDef = dbObj$DBconfig$ssffTrackDefinitions[[i]]
      dbObj$DBconfig$ssffTrackDefinitions[[i]] = NULL
      break
    }
  }
  
  # find and delete files
  if(deleteFiles){
    filePaths = list_bundleFilePaths(dbName=dbName, deletedDef$fileExtension, dbUUID = dbUUID)
    file.remove(filePaths)
  }
  # store changes
  .store.schema(dbObj)
}

###################################################
# CRUD operations for global labelGroups

##' Add / List / Remove global labelGroup to / of / from emuDB
##' 
##' Add / List / Remove label group that can be used as a short hand  
##' to reference groups of labels that are globally defined
##' for the entire database (compared to attribute definition
##' specific label groups that 
##' are added by \code{\link{add_attrDefLabelGroup}}) in a 
##' \code{\link{query}}. A common example would be to
##' add a label group for something like the phonetic
##' category of nasals to be able to reference them 
##' as "nasals" in a \code{\link{query}}. For 
##' more information on the structural elements of an emuDB 
##' see \code{vignette{emuDB}}.
##' 
##' @param dbName name of loaded emuDB
##' @param name name of label group
##' @param values character vector of labels
##' @param dbUUID optional UUID of loaded emuDB
##' @keywords emuDB database schema Emu
##' @seealso add_attrDefLabelGroup
##' @name AddListRemoveLabelGroup
##' @examples 
##' \dontrun{
##' 
##' ##################################
##' # prerequisite: loaded "ae" emuDB 
##' # (see ?load_emuDB for more information)
##' 
##' sampaNasals = c("m", "F", "n", "J", "N")
##' 
##' # add these values to the "ae" emuDB
##' # as a globally available labelGroup
##' add_labelGroup(dbName = "ae",
##'                name = "sampaNasals",
##'                values = sampaNasals)
##' 
##' # query the labelGroup in the "Phonetic" level
##' query(dbName = "ae", 
##'       query = "Phonetic == sampaNasals")
##' 
##' # query the labelGroup in the "Phoneme" level
##' query(dbName = "ae", 
##'       query = "Phoneme == sampaNasals")
##' 
##' # list global label groups of "ae" emuDB
##' list_labelGroups(dbName = "ae")
##' 
##' # remove the newly added labelGroup
##' remove_labelGroup(dbName = "ae",
##'                   name = "sampaNasals")
##' }
##' 
NULL

##' @rdname AddListRemoveLabelGroup
##' @export
add_labelGroup <- function(dbName,
                           name,
                           values,
                           dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  curLgs = list_labelGroups(dbName,dbUUID)
  
  if(name %in% curLgs$name){
    stop("labelGroup with name '", name ,"' already exists!")
  }
  
  # add labelGroup
  dbObj$DBconfig$labelGroups[[length(dbObj$DBconfig$labelGroups) + 1]] = list(name = name, 
                                                                              values = values)
  
  # store changes
  .store.schema(dbObj)
}


##' @rdname AddListRemoveLabelGroup
##' @export
list_labelGroups <- function(dbName,
                             dbUUID = NULL){
  
  uuid=get_UUID(dbName,dbUUID)
  dbObj = .load.emuDB.DBI(uuid = uuid)
  df = data.frame(name = character(),
                  values = character(),
                  stringsAsFactors = F)
  
  for(lg in dbObj$DBconfig$labelGroups){
    df = rbind(df, data.frame(name = lg$name,
                              values = paste0(lg$values, collapse = "; ")))
  }
  
  return(df)
  
}


##' @rdname AddListRemoveLabelGroup
##' @export
remove_labelGroup <- function(dbName,
                              name,
                              dbUUID = NULL){
  
  dbObj=.load.emuDB.DBI(uuid = dbUUID,name=dbName)
  curLgs = list_labelGroups(dbName)
  
  if(!name %in% curLgs$name){
    stop("No labelGroup with name '", name ,"' found!")
  }
  
  for(i in 1:length(dbObj$DBconfig$labelGroups)){
    if(dbObj$DBconfig$labelGroups[[i]]$name == name){
      dbObj$DBconfig$labelGroups[[i]] = NULL
    }
  }
  
  # store changes
  .store.schema(dbObj)
}



# FOR DEVELOPMENT 
# library('testthat') 
# test_file('tests/testthat/test_database.DBconfig.R')
