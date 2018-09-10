#' Pupil Data Function
#'
#' This sets the timing variable relative to the onset of each trial
#' @param x dataframe
#' @param trialonset.message Message string that marks the start of a trial
#' @param ms.conversion Conversion factor to convert timing to milliseconds
#' @param pretrial.duration Duration of pre-trial baseline period in milliseconds
#' @keywords set timing
#' @export
#' @examples
#' set.timing(x, start.trial = "Fixation", ms.conversion = 1000)

set.timing <- function(x, trialonset.message = "", ms.conversion = 1, pretrial.duration = 0){
  pretrial.duration <- abs(pretrial.duration)*-1
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     trialonset.time = ifelse(Message==trialonset.message, Time, NA),
                     trialonset.time.min = min(trialonset.time, na.rm = TRUE),
                     trialonset.time = ifelse(trialonset.time!=trialonset.time.min,NA,trialonset.time),
                     trialonset.time = zoo::na.locf(trialonset.time),
                     Time = ifelse(!is.na(trialonset.time), (Time - trialstart.time)/ms.conversion, NA,
                     PreTrial = ifelse(Time>=pretrial.duration & Time<0,1,ifelse(Time>=0,0,NA)))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x, Trial = ifelse(Time>=pretrial.duration,Trial,0))
  x <- dplyr::filter(x, Trial!=0, !is.na(Trial))
  x <- dplyr::select(x, -trialstart.time)
  return(x)
}
