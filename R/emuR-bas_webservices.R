##' Runs several BAS webservices, starting from an orthographic transcription
##'
##' This function calls the BAS webservices G2P, MAUS, Pho2Syl, MINNI and (if necessary) Chunker.
##' Starting from an orthographic transcription, it derives a tokenized orthographical word tier
##' using the G2P tool. It also derives canonical pronunciations (in SAMPA) for the words.
##' If at least one audio file is longer than 60 seconds, the function then calls the Chunker webservice
##' to presegment the recordings. Subsequently, the webservice MAUS is called to derive a phonetic
##' segmentation. A second, rough segmentation is created by running the phoneme decoder MINNI.
##' Finally, syllabification is performed by calling Pho2Syl. \strong{This function requires an internet connection.}
##'
##' All necessary level, attribute and link definitions are created in the process.
##' Note that this function will run all BAS webservices with default parameters, with four exceptions:
##' \itemize{
##' \item{Chunker: force=rescue}
##' \item{G2P: embed=maus}
##' \item{Pho2Syl: wsync=yes}
##' \item{MAUS: USETRN=[true if Chunker was called or transcription is a segment tier, false otherwise]}
##' }
##' If you wish to change parameters, you must use the individual runBASwebservices functions. This will also allow
##' you to carry out manual corrections in between the steps, or to use different languages for different webservices.
##'
##' @family BAS webservice functions
##'
##' @export
##' @param handle emuDB handle
##' @param transcriptionAttributeDefinitionName name of the attribute (not level!) containing an orthographic transcription.
##' @param language language(s) to be used. If you pass a single string (e.g. "deu-DE"), this language will be used for all bundles.
##' Alternatively, you can select the language for every bundle individually. To do so, you must pass a data frame with the columns
##' session, bundle, language. This data frame must contain one row for every bundle in your emuDB.
##' Up-to-date lists of the languages accepted by all webservices can be found here:
##' \url{https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/help}
##'
##' @param orthoAttributeDefinitionName attribute name for orthographic words
##' @param canoAttributeDefinitionName attribute name for canonical pronunciations of words
##' @param chunkAttributeDefinitionName attribute name for the chunk segmentation.
##' Please note that the chunk segmentation will only be generated if your emuDB contains
##' audio files beyond the one minute mark.
##' @param mausAttributeDefinitionName attribute name for the MAUS segmentation
##' @param minniAttributeDefinitionName attribute name for the MINNI segmentation
##' @param sylAttributeDefinitionName attribute name for syllable segmentation
##' @param canoSylAttributeDefinitionName attribute name for syllabified canonical pronunciations of words
##' @param runMINNI if set to \code{TRUE} (the default) the MINNI service is also run. As the MINNI service contains
##' less languages than the others it can be useful to turn this off.
##' @param patience If a web service call fails, it is repeated a further n times, with n being the value of patience.
##' Must be set to a value between 0 and 3.
##' @param verbose Display progress bars and other information
##' @param resume If a previous call to this function has failed (and you think you have fixed the issue
##' that caused the error), you can set resume=TRUE to recover any progress made up to that point. This
##' will only work if your R temporary directory has not been deleted or emptied in the meantime.

runBASwebservice_all <- function(handle,
                                 transcriptionAttributeDefinitionName,
                                 language,
                                 orthoAttributeDefinitionName = "ORT",
                                 canoAttributeDefinitionName = "KAN",
                                 mausAttributeDefinitionName = "MAU",
                                 minniAttributeDefinitionName = "MINNI",
                                 sylAttributeDefinitionName = "MAS",
                                 canoSylAttributeDefinitionName = "KAS",
                                 chunkAttributeDefinitionName = "TRN",
                                 runMINNI = TRUE,
                                 patience = 0,
                                 resume = FALSE,
                                 verbose = TRUE)
{
  check_emuDBhandle(handle)
  
  func = "all"
  transcriptionLevel = get_levelNameForAttributeName(handle, transcriptionAttributeDefinitionName)
  
  oldBasePath = handle$basePath
  
  if (is.null(transcriptionLevel)) {
    stop("Could not find a level for attribute ", transcriptionAttributeDefinitionName)
  }
  
  running_chunker = FALSE
  chunkLevel = NULL
  
  # if our transcription is a segment level, we assume it is a manual chunk segmentation
  if (get_levelDefinition(handle, transcriptionLevel)$type == "SEGMENT") {
    chunkLevel = transcriptionLevel # the transcription is the chunk segmentation
    usetrn = "true" # we use it for MAUS
  }
  
  # else, we check if we will need to perform automatic chunk segmentation
  else if (bas_long_enough_for_chunker(handle, oldBasePath)) {
    running_chunker = TRUE # we need to run the chunker
    chunkLevel = chunkAttributeDefinitionName
    usetrn = "true" # the to-be-created chunk segmentation will be used for MAUS
  }
  
  else
  {
    chunkAttributeDefinitionName = NULL
    usetrn = "false"
  }
  
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionAttributeDefinitionName = transcriptionAttributeDefinitionName,
    orthoAttributeDefinitionName = orthoAttributeDefinitionName,
    language = language,
    verbose = verbose,
    resume = resume,
    params = list(),
    func = func,
    patience = patience
  )
  
  bas_run_g2p_for_pronunciation_dbi(
    handle = handle,
    orthoAttributeDefinitionName = orthoAttributeDefinitionName,
    canoAttributeDefinitionName = canoAttributeDefinitionName,
    language = language,
    verbose = verbose,
    resume = resume,
    params = list(embed = "maus"),
    func = func,
    patience = patience
  )
  
  
  # if we previously decided to run automatic chunk segmentation
  if (running_chunker)
  {
    bas_run_chunker_dbi(
      handle = handle,
      canoAttributeDefinitionName = canoAttributeDefinitionName,
      chunkAttributeDefinitionName = chunkAttributeDefinitionName,
      orthoAttributeDefinitionName = orthoAttributeDefinitionName,
      rootLevel = transcriptionLevel,
      params = list(force = "rescue"),
      resume = resume,
      verbose = verbose,
      language = language,
      oldBasePath = oldBasePath,
      perspective = "default",
      func = func,
      patience = patience
    )
  }
  
  bas_run_maus_dbi(
    handle = handle,
    canoAttributeDefinitionName = canoAttributeDefinitionName,
    language = language,
    chunkLevel = chunkLevel,
    mausAttributeDefinitionName = mausAttributeDefinitionName,
    verbose = verbose,
    resume = resume,
    params = list(USETRN = usetrn),
    oldBasePath = oldBasePath,
    perspective = "default",
    turnChunkLevelIntoItemLevel = T,
    func = func,
    patience = patience
  )
  if(runMINNI){
    bas_run_minni_dbi(
      handle = handle,
      language = language,
      minniAttributeDefinitionName = minniAttributeDefinitionName,
      rootLevel = NULL,
      verbose = verbose,
      resume = resume,
      params = list(),
      oldBasePath = oldBasePath,
      perspective = "default",
      func = func,
      patience = patience
    )
  }
  
  bas_run_pho2syl_canonical_dbi(
    handle = handle,
    canoAttributeDefinitionName = canoAttributeDefinitionName,
    canoSylAttributeDefinitionName = canoSylAttributeDefinitionName,
    language = language,
    verbose = verbose,
    params = list(),
    resume = resume,
    func = func,
    patience = patience
  )
  
  orthoLevel = orthoAttributeDefinitionName
  
  bas_run_pho2syl_segmental_dbi(
    handle = handle,
    segmentAttributeDefinitionName = mausAttributeDefinitionName,
    language = language,
    sylAttributeDefinitionName = sylAttributeDefinitionName,
    superLevel = orthoLevel,
    resume = resume,
    params = list(wsync = "yes"),
    verbose = verbose,
    func = func,
    patience = patience
  )
  
  
  mausLevel = mausAttributeDefinitionName
  
  # remove the ORT -> MAU link as it is has been made redundant by the ORT -> MAS -> MAU path
  remove_linkDefinition(handle,
                        orthoLevel,
                        mausLevel,
                        force = T,
                        verbose = F)
  
  if (running_chunker)
  {
    # turn the chunk segmentation into an item level (as time information is now on the MAU tier)
    bas_segment_to_item_level(handle, chunkLevel)
    
    # remove the transcription -> ORT link
    # as it has been made redundant by the transcription -> TRN -> ORT path
    remove_linkDefinition(handle,
                          transcriptionLevel,
                          orthoAttributeDefinitionName,
                          force = T,
                          verbose = F)
  }
  
  handle = bas_clear(handle, oldBasePath, func)
  rewrite_annots(handle, verbose = verbose)
}

##' Runs MAUS webservice to create a phonetic segmentation
##'
##' This function calls the BAS webservice MAUS to generate a phonemic segmentation.
##' It requires a word-tokenized tier with a SAMPA pronunciation, which can be generated
##' by the function \link{runBASwebservice_g2pForPronunciation}.
##' \strong{This function requires an internet connection.}
##'
##' All necessary level, link and attribute definitions are created in the process.
##'
##' @family BAS webservice functions
##'
##' @export
##'
##' @param canoAttributeDefinitionName name of the attribute (not level!) containing the SAMPA word pronunciations.
##' If this attribute resides on a segment level, the segment time information is used as a presegmentation.
##' If it is an item level, no assumption is made about the temporal position of segments.
##' @param chunkLevel if you have a chunk segmentation level, you can provide it to improve the speed and accuracy
##' of MAUS. The chunk segmentation level must be a segment level, and it must link to the level of canoAttributeDefinitionName.
##' @param turnChunkLevelIntoItemLevel if TRUE, and if a chunk level is provided, the chunk level is converted into an ITEM level after segmentation
##' @param params named list of parameters to be passed on to the webservice. It is your own responsibility to
##' ensure that these parameters are compatible with the webservice API
##' (see \url{https://clarin.phonetik.uni-muenchen.de/BASWebServices/services/help}).
##' Some options accepted by the API (e.g. output format) cannot be set when calling a webservice from within emuR,
##' and will be overridden.
##' @param perspective the webApp perspective that the new level will be added to.
##' If NULL, the new level is not added to any perspectives.
##'
##' @inheritParams runBASwebservice_all

runBASwebservice_maus <- function(handle,
                                  canoAttributeDefinitionName,
                                  language,
                                  mausAttributeDefinitionName = "MAU",
                                  chunkLevel = NULL,
                                  turnChunkLevelIntoItemLevel = TRUE,
                                  params = NULL,
                                  perspective = "default",
                                  patience = 0,
                                  resume = FALSE,
                                  verbose = TRUE)
{
  check_emuDBhandle(handle)
  
  func = "maus"
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_maus_dbi(
    handle = handle,
    canoAttributeDefinitionName = canoAttributeDefinitionName,
    mausAttributeDefinitionName = mausAttributeDefinitionName,
    language = language,
    chunkLevel = chunkLevel,
    verbose = verbose,
    resume = resume,
    params = params,
    oldBasePath = oldBasePath,
    perspective = perspective,
    turnChunkLevelIntoItemLevel = turnChunkLevelIntoItemLevel,
    func = func,
    patience = patience
  )
  
  handle = bas_clear(handle, oldBasePath, func)
  
  rewrite_annots(handle, verbose = verbose)
}


#####################################################################
############################## G2P ##################################
#####################################################################


##' Tokenizes an orthographic transcription.
##'
##' This function calls the webservice G2P to break up a transcription into tokens, or words.
##' In addition to tokenization, G2P performs normalization of numbers and other special words.
##' A call to this function is usually followed by a call to \link{runBASwebservice_g2pForPronunciation}.
##' \strong{This function requires an internet connection.}
##'
##' All necessary level, link and attribute definitions are created in the process.
##'
##' @family BAS webservice functions
##'
##' @export
##' @inheritParams runBASwebservice_maus
##' @inheritParams runBASwebservice_all

runBASwebservice_g2pForTokenization <- function(handle,
                                                transcriptionAttributeDefinitionName,
                                                language,
                                                orthoAttributeDefinitionName = "ORT",
                                                params = list(),
                                                patience = 0,
                                                resume = FALSE,
                                                verbose = TRUE)
{
  check_emuDBhandle(handle)
  
  func = "g2p_tokenization"
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_g2p_for_tokenization_dbi(
    handle = handle,
    transcriptionAttributeDefinitionName = transcriptionAttributeDefinitionName,
    orthoAttributeDefinitionName = orthoAttributeDefinitionName,
    language = language,
    verbose = verbose,
    resume = resume,
    params = params,
    func = func,
    patience = patience
  )
  
  handle = bas_clear(handle, oldBasePath, func)
  
  rewrite_annots(handle, verbose = verbose)
}

##' Creates canonical pronunciation attributes for a tier of tokenized orthographical words.
##'
##' This function calls the G2P webservice to add canonical pronunciation attributes in SAMPA (default)
##' or IPA to a tier of tokenized orthographical words. It is usually called after tokenization
##' with \link{runBASwebservice_g2pForTokenization}. Its output can be used as input to
##' \link{runBASwebservice_maus} or \link{runBASwebservice_chunker}.
##' \strong{This function requires an internet connection.}
##'
##' By default, G2P is called in MAUS embed mode. This is important if you intend to use MAUS
##' afterwards. To disable MAUS embed mode, call this function with params=list(embed="no").
##' To derive IPA symbols, add outsym="ipa" to the parameter list.
##'
##' @family BAS webservice functions
##' @export
##'
##' @param orthoAttributeDefinitionName name of a attribute (not level!) containing orthographic words.
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus


runBASwebservice_g2pForPronunciation <- function(handle,
                                                 orthoAttributeDefinitionName,
                                                 language,
                                                 
                                                 canoAttributeDefinitionName = "KAN",
                                                 
                                                 params = list(embed = "maus"),
                                                 
                                                 patience = 0,
                                                 resume = FALSE,
                                                 verbose = TRUE)
{
  check_emuDBhandle(handle)
  func = "g2p_pronunciation"
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_g2p_for_pronunciation_dbi(
    handle = handle,
    orthoAttributeDefinitionName = orthoAttributeDefinitionName,
    language = language,
    canoAttributeDefinitionName = canoAttributeDefinitionName,
    verbose = verbose,
    resume = resume,
    params = params,
    func = func,
    patience = patience
  )
  
  handle = bas_clear(handle, oldBasePath, func)
  
  rewrite_annots(handle, verbose = verbose)
}



#####################################################################
########################### CHUNKER #################################
#####################################################################

##' Creates a chunk segmentation using the webservice Chunker.
##'
##' When audio input files are longer than approximately 10 minutes, alignment-based segmentation
##' tools such as MAUS will take a long time to run. In these cases, the Chunker pre-segments
##' the input into more digestable "chunks". As input, it requires a word tier with canonical
##' pronunciation attributes (which can be derived by \link{runBASwebservice_g2pForPronunciation}).
##' The resulting chunk level can be passed as input to \link{runBASwebservice_maus}.
##' \strong{This function requires an internet connection.}
##'
##' Please note that the chunker output is \strong{not} a semantically meaningful sentence
##' or turn segmentation, meaning that it cannot be used for analyses of sentence durations and the like.
##' By default, the chunker is called in force rescue mode. This means that the chunker is first run
##' in its normal mode, and switches to forced chunking mode only when it fails to find chunks that
##' are short enough for processing by MAUS. To disable the force mode completely, call this function with
##' params=list(force="false"). To skip the normal chunking mode and go directly into forced chunking
##' mode, use params=list(force="true").
##'
##' @family BAS webservice functions
##'
##' @export
##' @param canoAttributeDefinitionName name of the attribute (not level!) containing a canonical pronunciation of the words.
##' @param rootLevel if provided, the new level will be linked to the root level
##' @param orthoAttributeDefinitionName if provided, chunk attributes will contain orthographic instead of SAMPA strings.
##' Must be paired with the canonical pronunciation attributes in canoAttributeDefinitionName.
##' @param chunkAttributeDefinitionName attribute name for the chunk segmentation
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_chunker <- function(handle,
                                     canoAttributeDefinitionName,
                                     language,
                                     
                                     chunkAttributeDefinitionName = "TRN",
                                     rootLevel = NULL,
                                     orthoAttributeDefinitionName = NULL,
                                     
                                     params = list(force = "rescue"),
                                     
                                     perspective = "default",
                                     patience = 0,
                                     resume = FALSE,
                                     verbose = TRUE)
{
  check_emuDBhandle(handle)
  func = "chunker"
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_chunker_dbi(
    handle = handle,
    canoAttributeDefinitionName = canoAttributeDefinitionName,
    chunkAttributeDefinitionName = chunkAttributeDefinitionName,
    orthoAttributeDefinitionName = orthoAttributeDefinitionName,
    language = language,
    verbose = verbose,
    params = params,
    oldBasePath = oldBasePath,
    perspective = perspective,
    resume = resume,
    rootLevel = rootLevel,
    func = func,
    patience = patience
  )
  
  handle = bas_clear(handle, oldBasePath, func)
  
  rewrite_annots(handle, verbose = verbose)
}




#####################################################################
############################ MINNI ##################################
#####################################################################

##' Creates a rough phonetic segmentation by running the phoneme decoder webservice MINNI.
##'
##' The MINNI phoneme decoder performs phoneme-based decoding on the signal without input from
##' the transcription. Therefore, labelling quality is usually worse than that obtained from
##' MAUS (\link{runBASwebservice_maus}). Contrary to MAUS however, there is no need for a
##' pre-existing transcription.
##'
##' All necessary level, link and attribute definitions are created in the process.
##'
##' @family BAS webservice functions
##' @export
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus
##' @inheritParams runBASwebservice_chunker



runBASwebservice_minni <- function(handle,
                                   language,
                                   
                                   minniAttributeDefinitionName = "MINNI",
                                   rootLevel = NULL,
                                   
                                   params = list(),
                                   
                                   perspective = "default",
                                   patience = 0,
                                   resume = FALSE,
                                   verbose = TRUE)
{
  check_emuDBhandle(handle)
  func = "minni"
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_minni_dbi(
    handle = handle,
    language = language,
    minniAttributeDefinitionName = minniAttributeDefinitionName,
    verbose = verbose,
    rootLevel = rootLevel,
    resume = resume,
    params = params,
    oldBasePath = oldBasePath,
    perspective = perspective,
    func = func,
    patience = patience
  )
  
  handle = bas_clear(handle, oldBasePath, func)
  
  rewrite_annots(handle, verbose = verbose)
}


#####################################################################
########################### PHO2SYL #################################
#####################################################################

##' Adds syllabified word labels to a word level that already contains canonical pronunciations.
##'
##' This function calls the webservice Pho2Syl to add syllabified canonical pronunciation labels
##' to a word level that already contains unsyllabified canonical pronunciation labels (as can be
##' derived using \link{runBASwebservice_g2pForPronunciation}). \strong{This function requires an internet
##' connection.}
##'
##' @family BAS webservice functions
##' @export
##' @param canoAttributeDefinitionName name of the attribute (not level!) containing a canonical pronunciation of the words.
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_pho2sylCanonical <- function(handle,
                                              canoAttributeDefinitionName,
                                              language,
                                              canoSylAttributeDefinitionName = "KAS",
                                              params = list(),
                                              patience = 0,
                                              resume = FALSE,
                                              verbose = TRUE)
{
  check_emuDBhandle(handle)
  func = "pho2syl_canonical"
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_pho2syl_canonical_dbi(
    handle = handle,
    canoAttributeDefinitionName = canoAttributeDefinitionName,
    language = language,
    verbose = verbose,
    canoSylAttributeDefinitionName = canoSylAttributeDefinitionName,
    resume = resume,
    params = params,
    func = func,
    patience = patience
  )
  
  
  handle = bas_clear(handle, oldBasePath, func)
  
  rewrite_annots(handle, verbose = verbose)
}


##' Creates a syllable segmentation on the basis of a phonetic segmentation.
##'
##' This function calls the BAS webservice Pho2Syl to create a syllable segmentation on the basis
##' of a phonetic segmentation (created by, for example, \link{runBASwebservice_maus}).
##' You can provide the level of your word segmentation, or of any other hierarchically
##' dominant segmentation, via the superLevel parameter. This way, the new syllable
##' items can be linked up into the pre-existing hierarchy. If you do not provide
##' this input, the syllables will only be linked down to the segments.
##'
##' All necessary level, link and parameter definitions are created in the process.
##' By default, Pho2Syl is run in word synchronized mode. To override this, call this function
##' with the parameter params=list(wsync="no").
##'
##' @family BAS webservice functions
##' @export
##' @param segmentAttributeDefinitionName name of the attribute (not level!) containing a phonetic segmentation.
##' @param superLevel name of the segments' parent level (typically the word level).
##' If set to NULL, the syllable level cannot be linked up.
##'
##' @inheritParams runBASwebservice_all
##' @inheritParams runBASwebservice_maus

runBASwebservice_pho2sylSegmental <- function(handle,
                                              segmentAttributeDefinitionName,
                                              language,
                                              
                                              superLevel = NULL,
                                              
                                              sylAttributeDefinitionName = "MAS",
                                              
                                              params = list(wsync = "yes"),
                                              
                                              perspective = "default",
                                              patience = 0,
                                              resume = FALSE,
                                              verbose = TRUE)
{
  check_emuDBhandle(handle)
  func = "pho2syl_segmental"
  oldBasePath = handle$basePath
  handle = bas_prepare(handle, resume, verbose, func)
  
  bas_run_pho2syl_segmental_dbi(
    handle = handle,
    segmentAttributeDefinitionName = segmentAttributeDefinitionName,
    language = language,
    verbose = verbose,
    sylAttributeDefinitionName = sylAttributeDefinitionName,
    superLevel = superLevel,
    resume = resume,
    params = params,
    func = func,
    patience = patience
  )
  
  
  handle = bas_clear(handle, oldBasePath, func)
  
  rewrite_annots(handle, verbose = verbose)
}
