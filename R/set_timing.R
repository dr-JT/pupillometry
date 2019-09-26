#' Set timing relative to trial-onset
#'
#' This sets the timing variable relative to the onset of each trial
#' @param x dataframe
#' @param trialonset.message Message string that marks the start of a trial
#' @param ms.conversion Conversion factor to convert timing to milliseconds
#' @param pretrial.duration Duration of pre-trial baseline period in milliseconds
#' @param match Should the message string be an "exact" match or a "pattern" match?
#' @keywords set timing
#' @export
#' @examples
#' set_timing(x, start.trial = "Fixation", ms.conversion = 1000)

set_timing <- function(x, trialonset.message = NULL, ms.conversion = 1,
                       pretrial.duration = 0, match = "exact"){
  x <- dplyr::select(x, -ms_conversion)
  pretrial.duration <- abs(pretrial.duration)*-1

  x <- dplyr::group_by(x, Trial)
  if (match == "exact"){
    x <- dplyr::mutate(x, trialonset.time = ifelse(Message == trialonset.message,
                                                   Time, NA))
  } else if (match == "pattern"){
    x <- dplyr::mutate(x, trialonset.time =
                         ifelse(stringr::str_detect(Message, trialonset.message),
                                Time, NA))
  }
  x <- dplyr::mutate(x,
                     min = min(trialonset.time, na.rm = TRUE),
                     trialonset.time = ifelse(is.na(trialonset.time) |
                                                trialonset.time != min,
                                              NA, trialonset.time),
                     trialonset.time = zoo::na.locf(trialonset.time,
                                                    na.rm = FALSE),
                     trialonset.time = zoo::na.locf(trialonset.time,
                                                    na.rm = FALSE,
                                                    fromLast = TRUE),
                     Time = (Time - trialonset.time) / ms.conversion,
                     PreTrial = ifelse(Time >= pretrial.duration & Time < 0, 1,
                                       ifelse(Time >= 0, 0, NA)))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x, Trial = ifelse(Time >= pretrial.duration,Trial,0))
  x <- dplyr::filter(x, Trial != 0, !is.na(Trial))
  x <- dplyr::select(x, -trialonset.time, -min)
  x <- dplyr::distinct(x, Trial, Time, .keep_all = TRUE)
  x <- dplyr::filter(x, !is.na(Subject))

  col_order <- c("Subject", "Trial", "PreTrial", "Time", "Message",
                 "Message_Inserted", "Pupil_Diameter.mm", "Pupils.r",
                 "Event", "Gaze_Position.x", "Gaze_Position.y",
                 "Gaze.quality", "Head_Dist.cm")

  col_order <- colnames(data)[order(match(colnames(data), col_order))]

  data <- data[,col_order]

  return(x)
}
