#' Set timing relative to trial-onset
#'
#' This sets the timing variable relative to the onset of each trial
#' @param x dataframe
#' @param trial_onset.message Message string that marks the start of a trial
#' @param match Is message string an "exact" match or a "pattern" match?
#' @param pretrial.duration deprecated
#' @param trialonset.message deprecated
#' @param pre_trial.duration deprecated
#' @export
#'

set_timing <- function(x, trial_onset.message = NULL, pretrial.duration = 0,
                       match = "exact", trialonset.message = NULL,
                       pre_trial.duration = NULL){
  if (!is.null(trialonset.message)) {
    trial_onset.message <- trialonset.message
  }
  if (!is.null(pre_trial.duration)) {
    pretrial.duration <- pre_trial.duration
  }
  pretrial.duration <- abs(pretrial.duration) * -1

  if ("Stimulus" %in% colnames(x)) {
    x <- dplyr::group_by(x, Trial, Stimulus)
    x <- dplyr::mutate(x,
                       trialonset.time = ifelse(Stimulus == trial_onset.message,
                                                min(Time, na.rm = TRUE), NA))
    x <- dplyr::group_by(x, Trial)
    x <- dplyr::fill(x, trialonset.time, .direction = "updown")
    x <- dplyr::mutate(x, Time = Time - trialonset.time)
  } else if ("Message" %in% colnames(x)) {
    x <- dplyr::group_by(x, Trial)
    if (match == "exact"){
      x <- dplyr::mutate(x,
                         trialonset.time = ifelse(Message == trial_onset.message,
                                                  Time, NA))
    } else if (match == "pattern"){
      x <- dplyr::mutate(x,
                         trialonset.time =
                           ifelse(stringr::str_detect(Message,
                                                      trial_onset.message),
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
                       Time = Time - trialonset.time)
    x <- dplyr::select(x, -min)
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -trialonset.time)
  x <- dplyr::distinct(x, Trial, Time, .keep_all = TRUE)
  x <- dplyr::filter(x, !is.na(Subject))

  return(x)
}
