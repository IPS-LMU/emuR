#' add_signalViaMatlab
#' @md
#'
#' @description Use a Matlab function to derive an extra signal file for each
#' bundle of the Emu database. A new track definition will be added to the
#' database automatically.
#'
#' @details This function enables EMU-SDMS users you take advantage of tool boxes
#' and signal processing functions written in Matlab. The Matlab function must meet
#' certain requirements as detailed below, and it will always be run against the
#' entire database (either one bundle at a time or the whole database at a time,
#' but never only a part of the database).
#'
#' The Matlab function must:
#'
#' * Be defined in a file of its own.
#' * Accept named parameters.
#' * Accept at least the parameters `inputFilename` and `outputFilename`, both
#'   strings.
#' * Use the file at `inputFilename` and produce a new file `outputFilename`;
#'   the new file must be a `.mat` file containing the variables `data`,
#'   `sampleRate`, `startTime`, `units`, and `comment`.
#'
#' You can find examples of Matlab functions that meet these requirements by running
#' [create_emuRdemoData()] and then looking at the subdirectory `add_signal_files/matlab/`.
#'
#' The Matlab function can accept more parameters to influence the signal
#' processing. These parameters need not be the same values for the entire
#' database. They can be used, for example, to modify the signal processing
#' algorithms in a speaker-specific way.
#'
#' If `oneMatlabFunctionCallPerFile` is `TRUE`, the function will be called once
#' for every bundle of the database; in that case, all parameters
#' to the Matlab function will be 1x1 matrices. If `oneMatlabFunctionCallPerFile`
#' is `FALSE`, the Matlab function will only be called once for the entire database;
#' in that case, all parameters will be 1xN matrices with N equal to the number
#' of bundles in the database. `add_signalViaMatlab` will create a temporary `.m`
#' script. That script may, for example, contain code like this:
#'
#' ```matlab
#' demoSignalScalerForOneFile(inputFilename="msajc003.wav", outputFilename="/tmp/RtmpRwjz5Q/add_signalViaMatlab/0fc618dc-8980-414d-8c7a-144a649ce199/0000_ses/msajc003.mat", scalingFactor=1);
#' demoSignalScalerForOneFile(inputFilename="msajc010.wav", outputFilename="/tmp/RtmpRwjz5Q/add_signalViaMatlab/0fc618dc-8980-414d-8c7a-144a649ce199/0000_ses/msajc010.mat", scalingFactor=4);
#' ```
#' 
#' Or like this:
#' 
#' ```matlab
#' demoSignalScalerForManyFiles(inputFilename=["msajc003.wav", "msajc010.wav",], outputFilename=["/tmp/RtmpRwjz5Q/add_signalViaMatlab/0fc618dc-8980-414d-8c7a-144a649ce199/0000_ses/msajc003.mat", "/tmp/RtmpRwjz5Q/add_signalViaMatlab/0fc618dc-8980-414d-8c7a-144a649ce199/0000_ses/msajc010.mat], scalingFactor=[1, 4]);
#' ```
#' 
#' In both cases, `scalingFactor` is a parameter that `demoSignalScalerForOneFile`
#' and `demoSignalScalerForManyFiles` happen to accept. These are the demo functions
#' you can find by running [create_emuRdemoData].
#'
#' The input file will typically be the media file of the bundle, but can be one
#' of the other files stored in the bundle. If you need that, use the `inputFileExtension`
#' parameter.
#'
#' The output `.mat` files that need to be written by the Matlab function will
#' be converted – by `emuR` – to `.Rda` files and saved in each bundle folder with
#' the file extension `outputFileExtension`.
#'
#' The working directory of the Matlab function will be the same as that of the
#' current R session, see [base::getwd()].
#'
#' You need a working and licensed Matlab instance on your computer. It will be
#' called via [matlabr::run_matlab_code()].
#'
#' Matlab is a trademark of The MathWorks, Inc.
#'
#'
#' @param emuDBhandle The Emu database to work on.
#' @param matlabFunctionName Name of a Matlab function to use for signal processing.
#'        Must be available on Matlab’s search path; see `paths_to_add`.
#' @param outputFileExtension The file extension for the new derived signal file
#'        to be created within each bundle.
#' @param trackName The name of the new track that will be created automatically.
#' @param trackColumn The column of data to be used from the result files generated
#'        by Matlab. Should usually start with `data[` or `data$`.
#' @param oneMatlabFunctionCallPerFile Whether to call `matlabFunctionName` once
#'        per file (TRUE) or once for the entire database (FALSE). `FALSE` will
#'        be necessary if you want Matlab to process bundles in parallel.
#' @param inputFileExtension The file extension of the files to operate on. Defaults
#'        to the standard media file extension of the current Emu database.
#' @param matlabFunctionParameters Data frame with parameters for `matlabFunctionName`.
#'        Needs to contain the columns `session` and `bundle` plus one column for
#'        each function parameter. The column names will be used as parameter names.
#'        Must contain *one row for every bundle, without exception*.
#' @param paths_to_add List of paths where Matlab will look for functions. This
#'        is usually handled by [matlabr::run_matlab_code], but it adds the paths
#'        *after* the code, so we need to handle it in `emuR`.
#' @param ... Other parameters are passed on to [matlabr::run_matlab_code].
#'
#' @export
add_signalViaMatlab = function(emuDBhandle,
                               matlabFunctionName,
                               outputFileExtension,
                               trackName,
                               trackColumn,
                               oneMatlabFunctionCallPerFile = TRUE,
                               inputFileExtension = NULL,
                               matlabFunctionParameters = NULL,
                               paths_to_add = NULL,
                               ...) {
  # By "using" these variables, we make them required arguments in terms of R UI:
  emuDBhandle
  matlabFunctionName
  outputFileExtension
  trackName
  trackColumn

  if (is.null(inputFileExtension)) {
    DBconfig = load_DBconfig(emuDBhandle)
    inputFileExtension = DBconfig$mediafileExtension
  }

  listOfFiles =
    listOfFilesForExternalSignalProcessing("add_signalViaMatlab",
                                           emuDBhandle,
                                           inputFileExtension,
                                           outputFileExtension)

  filenameParameters =
    listOfFiles %>%
    select(session, bundle, inputFilename, outputFilename = intermediateFilename) %>%
    encodeStringsAsMatlabLiterals()

  if (is.null(matlabFunctionParameters)) {
    matlabFunctionParameters = tibble::tibble(session = character(), bundle = character())
  } else {
    matlabFunctionParameters =
      matlabFunctionParameters %>%
      encodeStringsAsMatlabLiterals()
  }

  matlabCommands =
    left_join(filenameParameters,
              matlabFunctionParameters,
              by = c("session", "bundle")) %>%
    select(-session, -bundle)

  if (oneMatlabFunctionCallPerFile) {
    matlabCommands =
      matlabCommands %>%
      makeKeyValuePairs() %>%
      unite("allParameters",
            everything(),
            sep = ", ") %>%
      mutate(command = paste0(matlabFunctionName,
                              "(",
                              allParameters,
                              ")"))
  } else {
    matlabCommands =
      matlabCommands %>%
      summarise(across(everything(),
                       ~ paste0(.x, collapse = ", ")),
                across(everything(),
                       ~ paste0("[", .x, "]"))) %>%
      makeKeyValuePairs() %>%
      unite("allParameters",
            everything(),
            sep = ", ") %>%
      mutate(command = paste0(matlabFunctionName,
                              "(",
                              allParameters,
                              ")"))
  }

  # This is a workaround: I could just pass paths_to_add on to run_matlab_code,
  # but it adds the paths *after* the code, which cannot work.
  code = matlabCommands$command
  if (!is.null(paths_to_add)) {
    paths_to_add = matlabr::add_path(paths_to_add)
    code = c(paths_to_add, code)
  }
  
  matlabr::run_matlab_code(code,
                           endlines = TRUE,
                           ...)

  convertMatlabIntermediateFilesToRda(listOfFiles)

  add_ssffTrackDefinition(emuDBhandle = emuDBhandle,
                          name = trackName,
                          columnName = trackColumn,
                          fileExtension = outputFileExtension,
                          fileFormat = "Rda")
}


listOfFilesForExternalSignalProcessing = function(functionName,
                                                  emuDBhandle,
                                                  inputFileExtension,
                                                  outputFileExtension) {
  listOfFiles =
    list_files(emuDBhandle, inputFileExtension) %>%
    mutate(inputFilename        = absolute_file_path,
           outputFilename       = file.path(emuDBhandle$basePath,
                                            paste0(session, session.suffix),
                                            paste0(bundle,  bundle.dir.suffix),
                                            paste0(bundle,  ".", outputFileExtension)),
           intermediateDir      = file.path(tempdir(),
                                            functionName,
                                            emuDBhandle$UUID,
                                            paste0(session, session.suffix)),
           intermediateFilename = file.path(intermediateDir,
                                            paste0(bundle, ".mat")))

  fs::dir_create(path = listOfFiles$intermediateDir, recurse = TRUE)

  return(listOfFiles)
}


# Converts .mat files as stored by external Matlab code to .Rda files.
#
# The argument files must be a data frame with the columns `intermediateFilename`
# and `outputFilename`. The `.mat` file must exist at the intermediate path, and
# the `.Rda` file will be created at the output path.
#
convertMatlabIntermediateFilesToRda = function(files) {
  for (i in rownames(files)) {
    currentFile = files[i,]

    rawData    = R.matlab::readMat(currentFile$intermediateFilename)

    data       = rawData$data
    units      = rawData$units
    sampleRate = rawData$sampleRate[1,1]
    startTime  = rawData$startTime[1,1]
    comment    = rawData$comment[1,1]

    save(data,
         units,
         sampleRate,
         startTime,
         comment,
         file = currentFile$outputFilename)
  }
}


# Expects a data frame and transforms all character-typed columns except those
# named "session" or "bundle".
#
# The transformation includes:
# - escaping all double quotes (") in the values as two double quotes ("")
# - adding one double quote to the beginning and end of each value
#
encodeStringsAsMatlabLiterals = function(matlabFunctionParameters) {
  for (column in colnames(matlabFunctionParameters)) {
    if (column == "session" || column == "bundle") {
      next
    }

    columnType = typeof(pull(matlabFunctionParameters, column))

    if (columnType == "character") {
      matlabFunctionParameters[column] =
        str_replace_all(pull(matlabFunctionParameters, column),
                        '"',
                        '""')

      matlabFunctionParameters[column] =
        paste0('"',
               pull(matlabFunctionParameters, column),
               '"')
    }
  }

  return(matlabFunctionParameters)
}


# Expects a data frame and transforms all columns except those named "session"
# or "bundle".
#
# The transformation consists of prepending each value with "columnName=".
#
makeKeyValuePairs = function(data) {
  for (column in colnames(data)) {
    if (column == "session" || column == "bundle") {
      next
    }

    data[column] = paste0(column,
                          "=",
                          pull(data, column))
  }

  return(data)
}
