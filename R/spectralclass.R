##' Function to test whether the object is of class "spectral"
##' 
##' Returns \code{TRUE} or \code{FALSE} depending on whether the object is of class "spectral".
##' 
##' 
##' @param obj An R object
##' @return A value, \code{TRUE} or \code{FALSE}
##' @author Jonathan Harrington
##' @seealso \code{\link{as.spectral}}
##' @keywords attribute
##' @examples
##' 
##' 
##' is.spectral(vowlax.dft.5)
##' is.spectral(fric.dft)
##' is.spectral(fric.dft$data)
##' is.spectral(vowlax.dft.5[1,])
##' is.spectral(fric.dft[1,1])
##' 
##' 
##' 
##' @export 
is.spectral <- function(obj)
{
#  if(!is.trackdata(dat))
    return("spectral" %in% class(obj) && !is.null(attr(obj,"fs")))
#  else
#    return("spectral" %in% class(dat$data) && !is.null(attr(dat,"fs")))
}


##' Converts a matrix into an object of class 'spectral'.
##' 
##' 
##' The supplied matrix is assumed to have one row per segment, and one column per sampling frequency. The user may specify either a sampling frequency for the spectral object in the \code{fs} argument (a single numeric value) or a vector of numbers equl in length to the number of columns in \code{mat}. 
##' 
##' @export
##' 
##' @param mat A matrix object
##' @param fs Either a single element numeric vector, or a numeric vector of
##' the same length as the number of columns in \code{mat}.
##' @param single.segment This argument only matters if an array/vector (one dimensional matrix) is supplied to the function. If \code{single.segment=TRUE} (the default), it will be assumed that the spectral data contained in the vector was obtained from a single segment, and the spectral object will then have one row and as many columns as the length of the vector. If \code{single.segment=FALSE} the vector will instead be converted into a 1 column matrix, which is essentially a 1 dimensional spectral result from each segment. 
##' @return An object of class 'spectral' with values from each sample frequency in columns.
##' @author Jonathan Harrington
##' @seealso \code{\link{is.spectral}} \code{\link{plot.spectral}}
##' @keywords attribute
##' @examples
##' 
##' vec = 1:10
##' as.spectral(vec, 2000)
##' mat = rbind(1:10, 1:10)
##' as.spectral(mat)


as.spectral.matrix <- function(mat, fs=NULL,single.segment=TRUE)
{
  if (!is.matrix(mat)){
    stop("The supplied object is not a matrix.")
  }
  if(is.spectral(mat)) {
    warning("The matrix is already of class spectral")
    return(mat)
  }
  N <- ncol(mat)
  if(N == 1 && single.segment){
    mat <- t(mat)
  }
  if(is.null(fs))
    fs <- 0:(ncol(mat)-1)
  else{
    if(length(fs)==1)
    {
      fs <- fs/2
      fs <- seq(0, fs, length.out=N)
    }
  }
  attr(mat,"fs") <- fs
  class(mat) <- c(class(mat), "spectral")
  
  return(mat)
}


# ##' Converts a trackdata object into an object of class 'spectral'.
# ##' 
# ##' 
# ##' 
# ##' 
# ##' 
# ##' @param track A trackdata object.
# ##' @param fs Either a single element numeric vector, or a numeric vector of
# ##' the same length as the length of trackdata if trackdata is a vector, or of
# ##' the same number of rows as trackdata
# ##' @return The same object but of class 'spectral'.
# ##' @author Jonathan Harrington
# ##' @seealso \code{\link{is.spectral}} \code{\link{plot.spectral}}
# ##' @keywords attribute
# ##' @examples
# ##' # turn a new spectral trackdata object into a trackdata object
# ##' tr = as.trackdata(rbind(fric.dft$data), fric.dft$index, fric.dft$ftime)
# ##' # turn it into a spectral trackdata object with sampling freq 16 kHz
# ##' tr = as.spectral(tr, 16000)
# ##' # list the frequencies
# ##' trackfreq(tr)
# ##' # Notice that only the $data is made into a spectral matrix,
# ##' # not the entire trackdata object
# ##' # so this is trackdata
# ##' class(tr)
# ##' # this is a spectral matrix
# ##' class(tr$data)
# ##' 
# 
# 
# as.spectral.trackdata <- function(track,fs=NULL)
# {
#   if(is.spectral(track) || is.spectral(track$data)) {
#     warning("The trackdata object is already of class spectral")
#     return(trackdata)
#   }
#   N <- ncol(track$data)
#   if(is.null(fs))
#     fs <- 0:(ncol(track$data)-1)
#   else{
#     if(length(fs)==1)
#     {
#       fs <- fs/2
#       fs <- seq(0, fs, length.out=N)
#     }
#   }
#   attr(track$data, "fs") <- fs
#   class(track$data) <- c(class(track$data), "spectral")
#   return(track)
# }



as.spectral <- function(x) {
  UseMethod("as.spectral")
}


##' Plot spectra from EMU spectral objects
##' 
##' The function plots spectrum of any EMU spectral object.
##' 
##' This function is implemented when a spectral trackdata object is called
##' with the 'plot' function.
##' 
##' @param x An EMU object of class 'spectral'
##' @param labs An optional vector character labels. Must be the same length as
##' specdata
##' @param ylim A two-element numeric vector for the y-axis range (see 'par')
##' @param xlim A two-element numeric vector for the x-axis range (see 'par')
##' @param col Specify a color - see 'mu.colour')
##' @param lty Specify a linetype - see 'mu.colour'
##' @param lwd Specify line thickness - see 'mu.colour'
##' @param fun An R function name e.g., mean, var, sum, etc. The function is
##' applied separately to each category type specified in labs
##' @param freq A numeric vector the same length as the number of columns in
##' specdata specifying the frequencies at which the spectral data is to be
##' plotted. If not supplied, defaults to trackfreq(specdata)
##' @param type A single element character vector for the linetype
##' @param power Logical. If T, then specdata (or specdata\$data if specdata is
##' a trackdata object, is converted to a *
##' specdata\eqn{\mbox{\textasciicircum}}{^}b, where a and b have the values
##' given in powcoeffs. This operation is applied before b
##' @param powcoeffs A two-element numeric vector. Defaults to c(10, 10)
##' @param dbnorm Logical. If T, apply dB-level normalization per spectrum as
##' defined by dbcoeffs below. Defaults to F.
##' @param dbcoeffs A two element numeric vector (x, y). The spectra are
##' normalised in such a way that the values of each spectrum at a frequency of
##' y are set to a dB level of x. For example, to normalise the spectrum to 10
##' dB at 2000 Hz, set dbnorm to T and dbcoeffs to c(2000, 10)
##' @param legend Parameters for defining the legend. See 'mu.legend' for
##' further details
##' @param axes A logical vector indicating whether the axes should be plotted
##' @param \dots Further graphical parameters may be supplied.
##' @note To plot spectral data from a spectral trackdata object, then call the
##' function explicitly with 'plot/spectral' rather than with just 'plot'
##' @export
##' @author Jonathan Harrington
##' @seealso \code{\link{plot}} \code{\link{plot.trackdata}}
##' \code{\link{as.spectral}}
##' @keywords dplot
##' @examples
##' \dontrun{
##' 
##' plot(vowlax.dft.5[1,])
##' 
##' # with label types
##' plot(vowlax.dft.5[1:20,], vowlax.l[1:20])
##' 
##' # As above but averaged after converting to power ratios.
##' plot(vowlax.dft.5[1:20,], vowlax.l[1:20], fun=mean, power=TRUE)
##' 
##' # All the spectra of one segment in a trackdata object
##' plot(fric.dft[1,])
##' 
##' }
##' 
"plot.spectral" <- function (x, labs, ylim, xlim,  col, lty, 
                             lwd, fun, freq, type = "l", 
                             power = FALSE, powcoeffs = c(10, 10), 
                             dbnorm = FALSE, dbcoeffs = c(0, 0), 
                             legend = TRUE, axes=TRUE,  ...) 
{
  specdata = x
  if (is.trackdata(specdata)) 
    specdata <- specdata$data
  if (!is.spectral(specdata)) 
    stop("specdata must be of class spectral")
  if (dbnorm) 
    specdata <- dbnorm(specdata, dbcoeffs[1], dbcoeffs[2])
  if (missing(freq)) 
    f <- trackfreq(specdata)
  else f <- freq
  if (is.matrix(specdata)) 
    N <- nrow(specdata)
  else {
    N <- 1
    specdata <- rbind(specdata)
  }
  if (missing(labs)) 
    labs <- rep(".", N)
  if (!missing(fun)) {
    if (power) 
      specdata <- dbtopower(specdata, powcoeffs[1], powcoeffs[2])
    mat <- list(NULL)
    for (j in unique(labs)) {
      temp <- labs == j
      v <- apply(specdata[temp, ], 2, fun)
      mat$fn <- rbind(mat$fn, v)
      mat$l <- c(mat$l, j)
    }
    dimnames(mat$fn) <- list(mat$l, dimnames(specdata)[[2]])
    specdata <- mat$fn
    if (power) 
      specdata <- dbtopower(specdata, powcoeffs[1], powcoeffs[2], 
                            inv = TRUE)
    if (length(unique(labs)) > 1) 
      labs <- dimnames(specdata)[[1]]
    else {
      labs <- unique(labs)
      specdata <- rbind(specdata)
    }
  }
  if (missing(ylim)) 
    ylim <- range(specdata)
  if (missing(xlim)) 
    xlim <- range(f)
  if (missing(col)) 
    col <- TRUE
  if (missing(lty)) 
    lty <- FALSE
  if (missing(lwd)) 
    lwd <- NULL
  cols <- mu.colour(labs, col, lty, lwd)
  for (j in 1:nrow(specdata)) {
    graphics::plot(f, specdata[j, ], type = type, col = cols$colour[j], 
         lty = cols$linetype[j], lwd = cols$lwd[j], xlim = xlim, 
         ylim = ylim, xlab = "", ylab = "", axes = FALSE)
    graphics::par(new = TRUE)
  }
  if (is.logical(legend)) {
    if (legend & length(unique(labs)) > 1) {
      legend <- "topright"
      legend(legend, NULL, cols$legend$lab, col = cols$legend$col, 
             lty = as.numeric(cols$legend$lty), lwd = as.numeric(cols$legend$lwd))
    }
  }
  else legend(legend, NULL, cols$legend$lab, col = cols$legend$col, 
              lty = as.numeric(cols$legend$lty), lwd = as.numeric(cols$legend$lwd))
  if(axes)
  {
    graphics::axis(side = 1)
    graphics::axis(side = 2)
  }
  graphics::title(...)
  graphics::box(...)
}


##' @export
"bark.spectral" <- function (f, ...) 
{
  specobject = f
  if (!is.trackdata(specobject)) {
    if (!is.matrix(specobject)) 
      specobject <- as.spectral(rbind(specobject), attr(specobject, 
                                                        "fs"))
  }
  f <- trackfreq(specobject)
  b <- bark(f)
  temp <- b < 0
  if (any(temp)) 
    specobject <- specobject[, !temp]
  f <- trackfreq(specobject)
  b <- bark(f)
  N <- length(b)
  ba <- seq(min(b), max(b), length = N)
  if (is.trackdata(specobject)) 
    spec <- specobject$data
  else if (is.matrix(specobject)) 
    spec <- specobject
  else spec <- as.spectral(rbind(specobject), attr(specobject,"fs"))
  res <- NULL
  for (j in 1:nrow(spec)) {
    v = approx(b, c(spec[j, ]), ba)
    if(j == 1){ # preallocate result matrix
      res = matrix(nrow = nrow(spec), ncol = length(v$y))
    }
    res[j, ] <- v$y
  }
  if (is.trackdata(specobject)) {
    specobject$data <- res
    if (!is.null(tracktimes(spec))) 
      rownames(specobject$data) <- tracktimes(spec)
    specobject <- as.spectral(specobject, ba)
  }
  else {
    specobject <- res
    specobject <- as.spectral(specobject, ba)
  }
  specobject
}

##' @export
"mel.spectral" <- function (a) 
{
  specobject = a
  if (!is.trackdata(specobject)) {
    if (!is.matrix(specobject)) 
      specobject <- as.spectral(rbind(specobject), attr(specobject, "fs"))
  }
  f <- trackfreq(specobject)
  b <- mel(f)
  N <- length(b)
  ba <- seq(min(b), max(b), length = N)
  if (is.trackdata(specobject)) 
    spec <- specobject$data
  else if (is.matrix(specobject)) 
    spec <- specobject
  else spec <- as.spectral(rbind(specobject), attr(specobject, 
                                                   "fs"))
  res <- NULL
  for (j in 1:nrow(spec)) {
    v = approx(b, c(spec[j, ]), ba)
    if(j == 1){ # preallocate result matrix
      res = matrix(nrow = nrow(spec), ncol = length(v$y))
    }
    res[j, ] <- v$y
  }
  if (is.trackdata(specobject)) {
    specobject$data <- res
    if (!is.null(tracktimes(spec))) 
      rownames(specobject$data) <- tracktimes(spec)
    specobject <- as.spectral(specobject, ba)
  }
  else {
    specobject <- res
    specobject <- as.spectral(specobject, ba)
  }
  specobject
}
