##' Find the time and segment corresponding to an index within a trackdata object.
##' 
##' Trackdata objects have a \code{data} component and two index components
##' (\code{index} and \code{ftime}). This function can be used to find out which
##' segment and timepoint (\code{ftime}) a particular row of \code{data} corresponds to.
##' 
##' @param track A trackdata object, as returned by \code{track} or \code{frames}.
##' @param datanum An integer, an index into the \code{data} component of
##' \code{track}.
##' @return A list with the following components
##' \item{segnum}{The index of the segment which the value found at index \code{datanum} belongs to.}
##' \item{time}{The time in the \code{ftime} field in the trackdata object that is found at the \code{datanum} index}
##' @examples 
##' coutts.epg
##' frames.time(coutts.epg,1)
##' @seealso track, frames
##' @keywords misc
##' @export
##' 

frames.time <- function(track, datanum)
{
  ## return the time and the number of the segment element
  ## that the datanum refers to
  if(is.matrix(track$ftime) == FALSE) track$ftime <- rbind(track$ftime)
  if(is.matrix(track$index) == FALSE)  track$index <- rbind(track$index)
  nums <- seq(1, nrow(track$ftime))
  incl <- track$index[, 1] <= datanum & track$index[, 2] >= datanum
  retv <- NULL
  segnum <- nums[incl]
  percent <- (datanum - track$index[segnum, 1])/
             (track$index[segnum, 2] - track$index[segnum, 1])
  retv$segnum <- segnum
  retv$time <- track$ftime[segnum, 1] + 
               percent * (track$ftime[segnum, 2] - track$ftime[segnum, 1])
  retv
}







##' Get data for a given time
##' 
##' Gets data for a given time
##' 
##' 
##' @param timeval A time in milliseconds
##' @param dataset A trackdata object as returned by \code{track}.
##' @return The element number of \code{trackdata$data} corresponding to
##' \code{time}
##' @seealso track, frames
##' @keywords misc
##' @export get.time.element
get.time.element <- function(timeval, dataset)
{
  ## timeval: a time in milliseconds
  ## dataset: a data structure consisting of $data, $ftime, $index
  ## returns the element number of dataset$data corresponding to timeval
  numrows <- nrow(dataset$ftime)
  left <- dataset$ftime[1, 1]
  right <- dataset$ftime[numrows, 2]
  left.i <- dataset$index[1, 1]
  right.i <- dataset$index[numrows, 2]
  round(((timeval - left)/(right - left)) * (right.i - left.i)) + 1
}
