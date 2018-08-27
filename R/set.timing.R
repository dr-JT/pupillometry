#' Pupil Data Function
#'
#' This sets the timing variable relative to the onset of each trial
#' @param x dataframe
#' @param start.trial Message string that marks the start of a trial
#' @param ms.conversion Conversion factor to convert timing to milliseconds
#' @param pretrial.duration Duration of pre-trial baseline period in milliseconds
#' @keywords set timing
#' @export
#' @examples
#' set.timing(x, start.trial = "Fixation", ms.conversion = 1000)

set.timing <- function(x, start.trial = "", ms.conversion = 1, pretrial.duration = 0){
  pretrial.duration <- abs(pretrial.duration)*-1
  start.timestamps <- x$Time[(grep(start.trial, x$Message))]
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, Time = (Time - start.timestamps[Trial])/ms.conversion,
                     PreTrial = ifelse(Time>=pretrial.duration & Time<0,1,ifelse(Time>=0,0,NA)))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x, Trial = ifelse(Time>=pretrial.duration,Trial,0))
  x <- dplyr::filter(x, Trial!=0)
  return(x)
}
