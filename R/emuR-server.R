
check_tibbleForServe <- function(tbl){
  req_columns = c("db_uuid", "session", "bundle", "start", 
                  "end", "sample_rate")
  
  if(!all(req_columns %in% names(tbl))){
    stop(paste0("tibble object does not contain all required columns. The required columns are: ", 
                paste(req_columns, collapse = "; ")))
  }
  
}

##' Serve EMU database to EMU-webApp
##' 
##' @description Serves emuDB media files, SSFF tracks and annotations for 
##' EMU-webApp browser GUI \url{http://ips-lmu.github.io/EMU-webApp/}
##' 
##' Instructions:
##' 
##' Start and connect (this should happen automatically):
##' 
##' \itemize{
##' \item Call this function to start the server.
##' \item Start a suitable HTML5 capable Web-Browser (Google Chrome, Firefox,...).
##' \item Navigate to the EMU-Webapp URL: \url{http://ips-lmu.github.io/EMU-webApp/}.
##' \item Press the 'Connect' button in the EMU-webApp and connect with default URL.
##' \item EMU-webApp loads the bundle list and the first 
##' bundles media file, SSFF tracks and annotations.
##' }
##' 
##' Disconnect and stop:
##' \itemize{
##' \item Disconnect and stop the server with the 'Clear' button of 
##' the webapp or the reload button of your browser.
##' \item The server can also be stopped by 
##' calling \code{\link{stopAllServers}} of the \link{httpuv} package
##' }
##' 
##' Hints:
##' \itemize{
##' \item To serve only a subset of sessions or bundles use 
##' the parameters \code{sessionPattern} and/or \code{bundlePattern}.
##' \item Use the \code{seglist} parameter to pass in a segment list
##' which was generated using the \code{query} function. This will
##' allow quick navigation to those segments.
##' }
##'
##' @details Function opens a HTTP/websocket and waits in a loop for browser requests. 
##' Parameter host determines the IP address(es) of hosts allowed to connect to the 
##' server. By default the server only listens to localhost. If you want to allow 
##' connection from any host set the host parameter to \code{0.0.0.0}. Please note 
##' that this might be an safety issue! The \code{port} parameter determines the port 
##' the server listens on. The \code{host} and \code{port} parameters are intended 
##' only for expert users. When started the R console will be blocked. On successful 
##' connection the server sends the session and bundle list of the database referenced 
##' by name by parameter \code{dbName} or by UUID parameter \code{dbUUID}.
##' The Web application requests bundle data for viewing or editing. If a bundle 
##' is modified with the EMU-webApp and the save button is pressed the server modifies 
##' the internal database and saves the changes to disk.
##' Communication between server and EMU webApp is defined by EMU-webApp-websocket-protocol 
##' version 0.0.2 (\url{https://ips-lmu.github.io/The-EMU-SDMS-Manual/app-chap-wsProtocol.html}).
##' 
##' @param emuDBhandle emuDB handle as returned by \code{\link{load_emuDB}}
##' @param sessionPattern A regular expression pattern matching session names to be served
##' @param bundlePattern A regular expression pattern matching bundle names to be served
##' @param seglist segment list to use for times anchors and session + bundle restriction (type: \link{emuRsegs})
##' @param bundleListName name of bundleList stored in emuDB/bundleLists subdir to send to EMU-webApp
##' @param host host IP to listen to (default: 127.0.0.1  (localhost))
##' @param port the port number to listen on (default: 17890)
##' @param autoOpenURL URL passed to \code{\link{browseURL}} function. If NULL or an empty string are passed in
##' \code{\link{browseURL}} will not be invoked.
##' @param browser argument passed on to \code{browser} argument of \code{\link{browseURL}} (see 
##' it's documentation for details )
##' @param debug TRUE to enable debugging (default: no debugging messages)
##' @param debugLevel integer higher values generate more detailed debug output
##' @param useViewer Use the viewer provided by \code{getOption("viewer")} (the viewer pane when using RStudio) 
##' and host a local version of the EMU-webApp in it. This will clone the current 
##' EMU-webApp build (\url{https://github.com/IPS-LMU/EMU-webApp/tree/gh-pages/}) into the directory provided by 
##' \code{\link{tempdir}} and serve this local version. A clone will 
##' only be performed if no \code{file.path(tempdir(), "EMU-webApp")} directory is present. An alternative
##' directory can be also set: \code{options(emuR.emuWebApp.dir="path/to/EMU-webApp")} (use if offline functionality is required).
##' @return TRUE (invisible) if the server was started
##' @export
##' @keywords emuDB EMU-webApp database websocket Emu
##' @examples
##' \dontrun{ 
##' ## Load EMU database 'myDb' and serve it to the EMU-webApp (opens default HTTP/websocket port 17890)
##' 
##' myDb = load_emuDB("/path/to/myDb")
##' serve(myDb)
##' }
##' 
serve <- function(emuDBhandle, 
                  sessionPattern = '.*', 
                  bundlePattern = '.*', 
                  seglist = NULL,
                  bundleListName = NULL,
                  host = '127.0.0.1', 
                  port = 17890, 
                  autoOpenURL = "https://ips-lmu.github.io/EMU-webApp/?autoConnect=true", 
                  browser = getOption("browser"), 
                  useViewer = TRUE,
                  debug = FALSE, 
                  debugLevel = 0){
  
  check_emuDBhandle(emuDBhandle)
  
  if(debug && debugLevel==0){
    debugLevel=2
  }
  
  bundleCount = 0
  DBconfig = load_DBconfig(emuDBhandle)
  if(is.null(seglist)){
    allBundlesDf = list_bundles(emuDBhandle)
  }else{
    check_tibbleForServe(seglist)
    tmp = data.frame(session = seglist$session, 
                     bundle = seglist$bundle, 
                     stringsAsFactors = F)
    allBundlesDf = unique(tmp)
  }
  
  bundlesDf = allBundlesDf
  
  if(!is.null(bundleListName)){
    if(!is.null(seglist)){
      stop("both seglist & bundleListName can't be set at the same time!")
    }
    bundlesDf = read_bundleList(emuDBhandle, bundleListName)
    if(is.null(DBconfig$EMUwebAppConfig$restrictions$bundleComments) || is.null(DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing)){
      # TODO ask user to set?
      DBconfig$EMUwebAppConfig$restrictions$bundleComments = TRUE
      DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing = TRUE
      store_DBconfig(emuDBhandle, DBconfig)
    }
  }
  
  if(!is.null(DBconfig$EMUwebAppConfig$restrictions$bundleComments) || !is.null(DBconfig$EMUwebAppConfig$restrictions$bundleFinishedEditing)){
    if(is.null(bundleListName)){
      warning(paste0("'bundleComments' and/or 'bundleFinishedEditing' are set to true ",
                     "in the DBconfig and the bundleListName parameter wasn't set! Any changes made ",
                     "to those fields in the bundleListSideBar in the EMU-webApp won't be saved as ",
                     "those values are stored in the bundleLists!"))
    }
  }
  
  
  if(!is.null(sessionPattern) && sessionPattern!='.*'){
    ssl = emuR_regexprl(sessionPattern, bundlesDf[['session']])
    bundlesDf = bundlesDf[ssl,]
  }
  if(!is.null(bundlePattern) && bundlePattern!='.*'){
    bsl = emuR_regexprl(bundlePattern,bundlesDf[['name']])
    bundlesDf = bundlesDf[bsl,]
  }
  
  if(!is.null(bundleListName)){
    bundlesDf = read_bundleList(emuDBhandle, bundleListName)
  }
  
  
  httpRequest = function(req){
    # See here: https://github.com/jeffreyhorner/Rook/blob/a5e45f751/README.md
    # for req env params
    if(req$REQUEST_METHOD == "GET"){
      # this is used if URL is used instead of BASE64 in encoding of media file in bundle
      queryStr = shiny::parseQueryString(req$QUERY_STRING)
      # SIC this should also have a third parameter "ext/extension"
      if(!is.null(queryStr$session) && !is.null(queryStr$bundle)){
        #print("processing GET request to media file...")
        mediaFilePath = file.path(emuDBhandle$basePath, 
                                  paste0(queryStr$session, session.suffix), 
                                  paste0(queryStr$bundle, bundle.dir.suffix),
                                  paste0(queryStr$bundle, ".", DBconfig$mediafileExtension))
        
        audioFile = file(mediaFilePath, "rb")
        audioFileData = readBin(audioFile, 
                                raw(), 
                                n = file.info(mediaFilePath)$size)
        close(audioFile)
        # Only 
        # Rook conform answer
        res = list(
          status = 200L, # 
          headers = list(
            'Content-Type' = 'audio/x-wav',
            'Access-Control-Allow-Origin' = "*"
          ),
          body = audioFileData
        )
        return(res)
      } else {
        # Serve local EMU-webApp files
        # adopted from servr:::serve_dir()
        #owd = setwd(dir)
        #on.exit(setwd(owd))
        path = httpuv::decodeURIComponent(req$PATH_INFO)
        Encoding(path) = "UTF-8"
        #print(path)
        status = 200L
        # only allow requests to / -> mapped to getOption("emuR.emuWebApp.dir")
        if (T) { # TODO: check if correct path
          path = file.path(getOption("emuR.emuWebApp.dir"), path)
        } else {
          # reject all other requests
          return(list(
            status = 404L,
            headers = list(
              'Content-Type' = 'text/html'
            ),
            body = paste0("404: Requested path that doesn't contain /EMU-webApp/")
          ))
        }
        
        # test if dir is requested -> use index.html as body 
        body = if (utils::file_test("-d", path)) {
          type = "text/html"
          if (file.exists(idx <- file.path(path, "index.html"))) {
            readLines(idx, warn = FALSE)
          }
          else {
            # this should currently never be reached! 
            d = file.info(list.files(path, all.files = TRUE, 
                                     full.names = TRUE))
            title = utils::URLencode(path,reserved = T)
            c("<!DOCTYPE html>", 
              "<html>", 
              "<head>", 
              sprintf("<title>%s</title>", title), 
              "</head>", 
              "<body>", 
              c(sprintf("<h1>Index of %s</h1>", title), 
                fileinfo_table(d)), 
              "</body>", 
              "</html>")
            
          }
        }
        else {
          type = guess_type(path)
          range = req$HTTP_RANGE
          if (is.null(range) || identical(range, "bytes=0-")){
            readBin(path, "raw", file.info(path)[, "size"])
            #read_raw(path)
          } else {
            # handle range requests
            range = strsplit(range, split = "(=|-)")[[1]]
            b2 = as.numeric(range[2])
            b3 = as.numeric(range[3])
            if (length(range) < 3 || (range[1] != "bytes") || 
                (b2 >= b3) || (b3 == 0)) 
              return(list(status = 416L, 
                          headers = list(`Content-Type` = "text/plain"), 
                          body = "Requested range not satisfiable\r\n"))
            status = 206L
            con = file(path, open = "rb", raw = TRUE)
            on.exit(close(con))
            seek(con, where = b2, origin = "start")
            readBin(con, "raw", b3 - b2 + 1)
          }
        }
        if (is.character(body) && length(body) > 1){
          body = paste(body, collapse = "\n")
        }
        res = list(status = status, 
                   body = body, 
                   headers = c(list(`Content-Type` = type), 
                               if (status == 206L) list(`Content-Range` = paste(sub("=", " ", req$HTTP_RANGE), file.info(path)[, "size"], sep = "/"))))
        
        return(res)  
      }
      
    }
  }
  
  onHeaders <- function(req){
    # following httuv docs we should return NULL here to proceed but that terminates the R session!
    #return(NULL)
  }
  
  serverEstablished = function(ws){
    
    cat("emuR websocket service established\n")
    
    serverClosed = function(ws){
      cat("emuR websocket service closed\n")
    }
    
    sendError = function(ws,errMsg,callbackID){
      status = list(type = 'ERROR', details = errMsg);
      response = list(callbackID = callbackID, 
                      status)
      responseJSON = jsonlite::toJSON(response,
                                      auto_unbox = TRUE,
                                      force = TRUE, 
                                      pretty = TRUE)
      result=ws$send(responseJSON)
    } 
    
    serverReceive = function(isBinary, DATA){
      if(debugLevel >= 4 ){
        cat("onMessage() call, binary:", 
            isBinary, 
            " data: ", 
            DATA, 
            "\n")
        
      }
      D = ""
      if(is.raw(DATA)) {
        D = rawToChar(DATA)
      }else{
        D = DATA
      }
      D = readr::parse_character(D) # ensure UTF-8 encoding windows
      jr = jsonlite::fromJSON(D, simplifyVector = FALSE)
      if(debugLevel >= 2 ){
        cat("Received command from EMU-webApp: ", jr[['type']], "\n")
        if(debugLevel >= 3){
          jrNms = names(jr)
          
          for( jrNm in jrNms){
            value = jr[[jrNm]]
            cat("param: ", jrNm)
            if(class(value) == 'character'){
              cat(": ", jr[[jrNm]])
            }
            cat("\n")
          }
        }
        
      }
      if(!is.null(jr$type)){
        if(debugLevel >= 2 ){
          cat("Received type from EMU-webApp: ", jr[['type']], "\n")
        }
        
      }
      if(jr$type == 'GETPROTOCOL'){
        
        protocolData = list(protocol = 'EMU-webApp-websocket-protocol',
                            version = '0.0.2')
        response=list(status = list(type='SUCCESS'),
                      callbackID = jr$callbackID,
                      data = protocolData)
        responseJSON=jsonlite::toJSON(response,
                                      auto_unbox = TRUE,
                                      force = TRUE,
                                      pretty = TRUE) 
        result = ws$send(responseJSON)
        if(debugLevel >= 2){
          cat("Sent protocol. \n")
        }
        
      }else if(jr$type == 'GETDOUSERMANAGEMENT'){
        # R server mode is single user mode 
        response = list(status = list(type = 'SUCCESS'),
                        callbackID = jr$callbackID,
                        data = "NO")
        responseJSON = jsonlite::toJSON(response,
                                        auto_unbox = TRUE,
                                        force = TRUE,
                                        pretty = TRUE) 
        result = ws$send(responseJSON)
        if(debugLevel >= 2){
          cat("Sent user managment: no. \n")
        }
        
      }else if(jr$type == 'GETGLOBALDBCONFIG'){
        if(debugLevel >= 4){
          cat("Send config: ", as.character(DBconfig), "\n")
        }
        response = list(status = list(type='SUCCESS'),
                        callbackID = jr$callbackID,
                        data = DBconfig)
        responseJSON = jsonlite::toJSON(response,
                                        auto_unbox = TRUE,
                                        force = TRUE,
                                        pretty = TRUE) 
        result = ws$send(responseJSON)
        if(debugLevel >= 2){
          if(debugLevel >=4){
            cat(responseJSON,"\n")
          }
          cat("Sent config. \n")
        }
        #}
        
        
      }else if(jr$type == 'GETBUNDLELIST'){
        response = list(status = list(type = 'SUCCESS'),
                        callbackID = jr$callbackID,
                        dataType = 'uttList',
                        data = bundlesDf)
        # create time anchors
        if(!is.null(seglist)){
          dataWithTimeAnchors = list()
          for(i in 1:nrow(response$data)){
            sesBool = response$data[i,]$session == seglist$session 
            bndlBool = response$data[i,]$bundle == seglist$bundle
            start_sample_vals = round(((seglist[sesBool & bndlBool,]$start / 1000) + 0.5 / seglist[sesBool & bndlBool,]$sample_rate) 
                                      * seglist[sesBool & bndlBool,]$sample_rate)
            # end_sample_vals calculated with + 1 as EMU-webApp seems to always mark the right boundary left of the selected sample
            end_sample_vals = round(((seglist[sesBool & bndlBool,]$end / 1000) + 0.5/seglist[sesBool & bndlBool,]$sample_rate) * 
                                      seglist[sesBool & bndlBool,]$sample_rate)
            dataWithTimeAnchors[[i]] = list(session = response$data[i,]$session, 
                                            name = response$data[i,]$bundle,
                                            timeAnchors = data.frame(sample_start = start_sample_vals,
                                                                     sample_end = end_sample_vals))
            
          }
          response$data = dataWithTimeAnchors
        }
        responseJSON = jsonlite::toJSON(response,
                                        auto_unbox = TRUE,
                                        force = TRUE,
                                        pretty = TRUE)
        if(debugLevel >= 5) cat(responseJSON,"\n")
        result = ws$send(responseJSON)
        if(debugLevel >= 2){
          cat("Sent utterance list with length: ",
              nrow(bundlesDf),
              " \n")
        }
        
      }else if(jr$type == 'GETBUNDLE'){
        
        bundleName = jr[['name']]
        bundleSess = jr[['session']]
        #cat("data:",jr[['data']],"\n")
        if(debugLevel > 2){
          cat("Requested bundle:", bundleName, ",session:", bundleSess, "\n")
        }
        err = NULL
        if(debugLevel > 3){
          cat("Convert bundle to S3 format", bundleName, "\n")
        }
        # construct path to annotJSON
        annotFilePath = normalizePath(file.path(emuDBhandle$basePath, 
                                                paste0(bundleSess, session.suffix), 
                                                paste0(bundleName, bundle.dir.suffix), 
                                                paste0(bundleName, bundle.annotation.suffix, '.json')))
        
        b = jsonlite::fromJSON(annotFilePath, 
                               simplifyVector = F)
        if(is.null(b)){
          # error
          err = simpleError(paste('Could not load bundle ',
                                  bundleName,
                                  ' of session ',
                                  bundleSess))
        }
        
        mediaFile = list(encoding = "GETURL", 
                         data = paste0(rstudioapi::translateLocalUrl(paste0("http://", ws$request$HTTP_HOST)), 
                                       "?session=", 
                                       utils::URLencode(bundleSess, reserved = T),
                                       "&bundle=", 
                                       utils::URLencode(bundleName, reserved = T)))
        if(is.null(err)){
          ssffTracksInUse = get_ssffTracksUsedByDBconfig(DBconfig)
          ssffTrackNmsInUse = c()
          for(ssffTrackInUse in ssffTracksInUse){
            ssffTrackNmsInUse = c(ssffTrackNmsInUse, 
                                  ssffTrackInUse[['name']])
          }          
          if(debugLevel >= 4){
            
            cat(length(ssffTrackNmsInUse), " track definitions in use:\n")
            for(sfInU in ssffTrackNmsInUse){
              cat(sfInU, " ")
            }
            cat("\n")
          }
          ssffFiles = list()
          # Hash (here: named character vector) with SSFF files extension as key and file path as value
          # avoids duplicates in ssff files list
          ssffFilesHash = character(0)
          for(ssffTr in DBconfig$ssffTrackDefinitions){
            if(ssffTr[['name']] %in% ssffTrackNmsInUse){
              fe = ssffTr[['fileExtension']]
              ssffFilesHash[fe] = normalizePath(file.path(emuDBhandle$basePath, 
                                                          paste0(bundleSess, session.suffix), 
                                                          paste0(bundleName, bundle.dir.suffix), 
                                                          paste0(bundleName, ".", fe)))
            }
          }
          # read SSFF track file data
          ssffFileExts = names(ssffFilesHash)
          for(ssffFileExt in ssffFileExts){
            ssffFilePath = ssffFilesHash[ssffFileExt]
            mf = tryCatch(file(ssffFilePath, "rb"), 
                          error = function(e) {err<<-e})
            if(is.null(err)){
              mfData = readBin(mf, 
                               raw(), 
                               n = file.info(ssffFilePath)$size)
              if(inherits(mfData,'error')){
                err = mfData
                break
              }
            }else{
              break
            }
            mfDataBase64 = base64enc::base64encode(mfData)
            encoding = "BASE64"
            ssffDatObj = list(encoding = encoding, 
                              data = mfDataBase64,
                              fileExtension = ssffFileExt)
            ssffFiles[[length(ssffFiles) + 1]] = ssffDatObj
            close(mf)
          }
          if(is.null(err)){
            data = list(mediaFile = mediaFile,
                        ssffFiles = ssffFiles,
                        annotation = b)
          }
        }
        
        if(is.null(err)){
          responseBundle = list(status = list(type = 'SUCCESS'),
                                callbackID = jr$callbackID,
                                responseContent = 'bundle',
                                contentType = 'text/json',
                                data = data)
        }else{
          errMsg = err[['message']]
          cat("Error: ", errMsg, "\n")
          responseBundle = list(status = list(type = 'ERROR', message=errMsg),
                                callbackID = jr[['callbackID']],
                                responseContent = 'status',
                                contentType = 'text/json')
          
        }
        responseBundleJSON = jsonlite::toJSON(responseBundle,
                                              auto_unbox = TRUE,
                                              force = TRUE,
                                              pretty = TRUE)
        result = ws$send(responseBundleJSON)
        
        if(is.null(err) & debugLevel >= 2){
          
          if(debugLevel >= 8){
            cat(responseBundleJSON, "\n")
          }
          cat("Sent bundle containing", length(ssffFiles), "SSFF files\n")
        }
        # reset error
        err = NULL
        
      } else if(jr[['type']] == 'SAVEBUNDLE'){
        jrData = jr[['data']]
        jrAnnotation = jrData[['annotation']]
        bundleSession = jrData[['session']]
        bundleName = jrData[['annotation']][['name']]
        if(debugLevel > 3){
          cat("Save bundle ", bundleName, " from session ", bundleSession, "\n");
        }
        err = NULL
        
        ssffFiles = jr[['data']][['ssffFiles']]
        oldBundleAnnotDFs = load_bundleAnnotDFsDBI(emuDBhandle, bundleSession, bundleName)
        
        # warnings as errors
        warnOptionSave = getOption('warn')
        options('warn' = 2)
        responseBundle = NULL
        
        # check if cached version of bundle is available
        if(is.null(oldBundleAnnotDFs)){
          # error
          err = simpleError(paste('Could not load bundle ',
                                  bundleSession, 
                                  bundleName))
        }else{
          for(ssffFile in ssffFiles){
            inCfg = FALSE
            sp = normalizePath(file.path(emuDBhandle$basePath, 
                                         paste0(bundleSession, session.suffix), 
                                         paste0(bundleName, bundle.dir.suffix), 
                                         paste0(bundleName, ".", ssffFile$fileExtension)))
            if(is.null(sp)){
              errMsg = paste0("SSFF track definition for file extension '",
                              ssffFile[['fileExtension']],
                              "' not found!")
              err = simpleError(errMsg)
            }else{
              # store
              if(debugLevel > 3){
                cat("Writing SSFF track to file: ", sp, "\n")
              }
              ssffTrackBin = base64enc::base64decode(ssffFile[['data']])
              ssffCon = tryCatch(file(sp,'wb'),
                                 error = function(e){err <<- e})
              if(is.null(err)){
                res = tryCatch(writeBin(ssffTrackBin, ssffCon))
                close(ssffCon)
                if(inherits(res,'error')){
                  err = res
                  break
                }
                # modified<<-TRUE
              }
            }
          }
          bundleData = jr[['data']][['annotation']]
          
          # if we do not have an (error) response already
          if(is.null(err)){
            ##### emuDB ####
            # construct path to annotJSON and store
            annotFilePath = file.path(emuDBhandle$basePath, 
                                      paste0(bundleSession, session.suffix), 
                                      paste0(bundleName, bundle.dir.suffix), 
                                      paste0(bundleName, bundle.annotation.suffix, '.json'))
            
            json = jsonlite::toJSON(bundleData, 
                                    auto_unbox = TRUE, 
                                    force = TRUE, 
                                    pretty = TRUE)
            
            # use try mainly for permission problems on file system
            res = tryCatch(writeLines(json, annotFilePath, useBytes = TRUE), 
                           error = function(e) e)
            if(inherits(res,'error')){
              err = res
            }
            
            #### DBI ###
            # remove
            remove_bundleDBI(emuDBhandle, 
                             sessionName = bundleSession, 
                             name = bundleName)
            remove_bundleAnnotDBI(emuDBhandle, 
                                  sessionName = bundleSession, 
                                  bundleName = bundleName)
            # store
            # calculate MD5 sum of bundle annotJSON
            newMD5annotJSON = tools::md5sum(annotFilePath)
            names(newMD5annotJSON) = NULL
            
            bundleAnnotDFs = annotJSONcharToBundleAnnotDFs(as.character(json))
            add_bundleDBI(emuDBhandle, 
                          sessionName = bundleSession, 
                          name = bundleName, 
                          bundleAnnotDFs$annotates, 
                          bundleAnnotDFs$sampleRate, 
                          newMD5annotJSON)
            store_bundleAnnotDFsDBI(emuDBhandle, 
                                    bundleAnnotDFs, 
                                    sessionName = bundleSession, 
                                    bundleName = bundleName)
            
            # update bundlesDf and store as bundleList
            if(!is.null(bundleListName)){
              bl = read_bundleList(emuDBhandle, bundleListName)
              # print(jr[['data']][['comment']])
              bl[bl$session == bundleSession & 
                   bl$name == bundleName,]$comment = jr[['data']][['comment']]
              bl[bl$session == bundleSession & 
                          bl$name == bundleName,]$finishedEditing = jr[['data']][['finishedEditing']]
              
              write_bundleList(emuDBhandle, bundleListName, bl)

            }
          }
        }
        if(is.null(err)){
          responseBundle = list(status = list(type = 'SUCCESS'),
                                callbackID = jr$callbackID,
                                responseContent = 'status',
                                contentType = 'text/json')
        }else{
          m = err[['message']]
          cat('Error: ', m, "\n")
          responseBundle = list(status = list(type = 'ERROR', message = m),
                                callbackID = jr[['callbackID']],
                                responseContent = 'status',
                                contentType = 'text/json')
        }
        # response object to JSON 
        responseBundleJSON = jsonlite::toJSON(responseBundle,
                                              auto_unbox = TRUE,
                                              force = TRUE,
                                              pretty = TRUE)
        # send response
        result = ws$send(responseBundleJSON)
        # restore warn level
        options(warn = warnOptionSave)
        
        # reset error
        err = NULL
        
      }else if(jr[['type']] == 'DISCONNECTWARNING'){
        response = list(status = list(type = 'SUCCESS'), 
                        callbackID = jr[['callbackID']], 
                        responseContent = 'status', 
                        contentType = 'text/json')
        responseJSON = jsonlite::toJSON(response,
                                        auto_unbox = TRUE,
                                        force = TRUE,
                                        pretty = TRUE)
        result = ws$send(responseJSON)
        ws$close()
        cat("emuR websocket service closed by EMU-webApp\n")
      }
    }
    ws$onMessage(serverReceive)
    ws$onClose(serverClosed)
  }
  
  # stop all running servers
  httpuv::stopAllServers()
  
  # user messages
  cat("Navigate your browser to the EMU-webApp URL: https://ips-lmu.github.io/EMU-webApp/ (should happen automatically)\n")
  cat("Server connection URL: ws://localhost:", port, "\n", sep = '')
  cat("To stop the server either press the 'clear' button in the EMU-webApp, close/reload the webApp in your browser,\n")
  cat("or call the httpuv::stopAllServers() function\n")
  
  # build app list
  app = list(call = httpRequest, 
             onHeaders = onHeaders, 
             onWSOpen = serverEstablished)
  
  # start server
  httpuv::startServer(host = host, 
                      port = port, 
                      app = app)
  
  # either open browser of clone and host local EMU-webApp
  if(length(autoOpenURL) != 0 && autoOpenURL != ""){
    # open browser with EMU-webApp
    viewer <- getOption("viewer")
    if(useViewer & rstudioapi::isAvailable()){
      webApp_path = getOption("emuR.emuWebApp.dir")
      # TODO: can this be emulated? git clone --depth 1 -b gh-pages https://github.com/IPS-LMU/EMU-webApp
      #unlink(webApp_path, recursive = T)
      if(!dir.exists(webApp_path)){
        
        dir.create(webApp_path)
        # for devel 
        # file.copy(from = "~/Developer/EMU-webApp/dist/",
        #           to = tempdir(),
        #           recursive = T)
        # 
        # file.rename(from = file.path(tempdir(), "dist"),
        #             to = webApp_path)
        
        git2r::clone("https://github.com/IPS-LMU/EMU-webApp",
                     local_path = webApp_path,
                     branch = "gh-pages")
      }
      
      # replace <base href> tag because rstudio changes this 
      # in the web version and Angular needs it to be set
      if(rstudioapi::translateLocalUrl(paste0("http://localhost:", port, "/")) == paste0("http://localhost:", port, "/")){
        base_path = "/"
      } else {
        base_path = paste0("/", rstudioapi::translateLocalUrl(paste0("http://localhost:", port, "/")))
      }
      
      index_html = readr::read_file(file.path(webApp_path, "index.html"))
      index_html_new = stringr::str_replace(index_html, 
                                            pattern = "<base href=\"/EMU-webApp/\">",
                                            replacement = paste0("<base href=\"", base_path, "\">"))
      # remove manifest entry to avoid caching of local version
      index_html_new = stringr::str_replace(index_html_new, 
                                            pattern = "manifest=\"manifest.appcache\"",
                                            replacement = "")
      
      
      readr::write_file(x = index_html_new, 
                        path = file.path(webApp_path, "index.html"))
      
      if (!is.null(viewer)){
        # host in viewer
        viewer(paste0("http://127.0.0.1:", 
                      port, 
                      "/?autoConnect=true",
                      "&serverUrl=", 
                      stringr::str_replace(rstudioapi::translateLocalUrl(paste0("http://127.0.0.1:", port), absolute = TRUE),
                                                                         "http", 
                                                                         "ws")))
      }else{
        # host in browser
        utils::browseURL(paste0("http://127.0.0.1:", 
                                port, 
                                "/?autoConnect=true",
                                "&serverUrl=ws://127.0.0.1:", 
                                port),
                         browser = browser)
      }
      
    }else{
      # use online version
      utils::browseURL(autoOpenURL, browser = browser)
      cat("Unable to detect RStudio. Serving to online version.\n")
    }
  }
  
  
  
  return(invisible(TRUE))
  
}

## searches for all tracks needed by the EMUwebApp and
## returns their ssffTrackDefinitions
get_ssffTracksUsedByDBconfig <- function(DBconfig){
  allTracks = NULL
  
  # anagestConfig ssffTracks
  for(ld in DBconfig$levelDefinitions){
    allTracks = c(allTracks, 
                  ld$anagestConfig$verticalPosSsffTrackName, 
                  ld$anagestConfig$velocitySsffTrackName)
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
      # staticContours
      for(dot in tddd$staticContours){
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


# copy of servr:::fileinfo_table
fileinfo_table = function (info) {
  info = info[order(info$isdir, decreasing = TRUE), ]
  d = info$isdir
  i = !is.na(d)
  x1 = paste(basename(rownames(info)), ifelse(d & i, "/", ""), 
             sep = "")
  x1 = utils::URLencode(x1, reserved = T)
  x1[i] = sprintf("<a href=\"%s\">%s</a>", x1[i], x1[i])
  x2 = paste(format(info$size, scientific = FALSE, big.mark = ","), 
             "B")
  x2[is.na(info$size) | d] = ""
  x3 = as.character(info$mtime)
  x3[is.na(x3)] = ""
  c("<table>", "<thead><tr>", sprintf("<th>%s</th>", c("Name", 
                                                       "Size", "Date Modified")), "</tr></thead>", apply(cbind("<tr>", 
                                                                                                               sprintf("<td>%s</td>", x1), sprintf("<td align=\"right\">%s</td>", 
                                                                                                                                                   x2), sprintf("<td>%s</td>", x3), "</tr>"), 1, paste, 
                                                                                                         collapse = ""), "</table>")
}

# copy of servr:::guess_type
guess_type <- function (path) 
{
  mimetype = function(...) {
    system2("mimetype", c("-b", shQuote(path)), ...)
  }
  if (Sys.which("mimetype") == "" || mimetype(stdout = NULL) != 
      0) 
    return(mime::guess_type(path))
  mimetype(stdout = TRUE)
}

