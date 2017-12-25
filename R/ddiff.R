##' Differentiation of tracks
##' 
##' Differentiates a list, as returned by track, to the nth order, readjusting
##' the index and ftime values each time.
##' 
##' 
##' @aliases ddiff ddiff.sub
##' @param dataset track data object - a list as returned by track
##' @param n the order of differentiation
##' @param smoothing if TRUE track is smoothed
##' @author Jonathan Harrington
##' @keywords math
##' @export ddiff
##' 
ddiff <- function(dataset, n = 1, smoothing = TRUE)
{
  ## differentiates a list, as returned by track, to the nth
  ## order, readjusting the index and ftime values each time
  ## dataset: a list as returned by track
  ## n: the order of differentiation
  
  ## now we apply the function to the data using dapply
  outdat <- dapply(dataset, ddiff.sub, n = n)
  if(smoothing)
    dsmooth(outdat)
  else outdat
}



#' A function to be called by ddiff to do differentiation of a data track.
#'
#' @param data a data matrix
#' @param ftime a start-end pair
#' @param n number of times to differentiate
#'
#' @return a list of \code{data} values differentiated and \code{ftime} values adjusted accordingly. Values in \code{data} that are returned are per millisecond
#'
#' @examples
ddiff.sub <- function(data, ftime, n)
{

  if(is.matrix(data)) lval <- nrow(data) else lval <- length(data
  )
  if(lval < 1) stop("not enough data points in ddiff")	
  ## compute the time between samples
  interval <- (ftime[2] - ftime[1])/lval	
  ## do the differentiation
  data <- diff(data, differences = n)
  if(is.matrix(data))
    lval <- nrow(data)
  else lval <- length(data)
  timefactor <- (n * interval)/2
  ftime[1] <- ftime[1] + timefactor
  ftime[2] <- ftime[2] - timefactor	
  ## smooth the data as appropriate
  data <- data/interval	
  ## and return the data in the required format
  list(data = data, ftime = ftime)
}
