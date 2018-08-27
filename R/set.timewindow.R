#' Indexing Function
#'
#' This function uses specified start and/or end trial messages to correctly index the trial number. This makes it easier for pre-processing and later analyses
#' the 'dplyr' package is a dependency
#' @param x vector
#' @param start Message string that identifies the onset of a trial
#' @param duration Duration of time window in milliseconds
#' @keywords set time window
#' @export
#' @examples
#' set.trial(x, start = "Fixation", duration = 2000)

set.timewindow <- function(x, start = "", duration = 2000){
  duration <- (duration/1000)*x$Hz[1]
  onsetTimes <- grep(start, x$Message)
  x <- dplyr::mutate(x, TimeWindow = NA)
  for (i in 1:length(onsetTimes)){
    x <- dplyr::mutate(x, TimeWindow = ifelse(row_number()>=(onsetTimes[i]) & row_number()<(onsetTimes[i]+duration),i,Trial))
  }
  return(x)
}
