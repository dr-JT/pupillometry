#' Set timing relative to trial-onset
#'
#' This sets the timing variable relative to the onset of each trial
#' @param x dataframe
#' @param trial_onset.message Message string that marks the start of a trial
#' @param pretrial.duration Duration of pre-trial period in milliseconds
#' @param match Is message string an "exact" match or a "pattern" match?
#' @param trialonset.message See trial_onset.message
#' @param pre_trial.duration See pretrial.duration
#' @export
#'

set_timing <- function(x, trial_onset.message = NULL, ms.conversion = 1,
                       pretrial.duration = 0, match = "exact",
                       trialonset.message = NULL, pre_trial.duration = NULL){
  if (!is.null(trial_onset.message)) {
    trial_onset.message <- trialonset.message
  }
  if (!is.null(pre_trial.duration)) {
    pretrial.duration <- pre_trial.duration
  }
  pretrial.duration <- abs(pretrial.duration) * -1

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
                     Time = Time - trialonset.time,
                     Trial_Phase =
                       ifelse(Time >= pretrial.duration & Time < 0,
                              "PreTrial", ifelse(Time >= 0, "Trial", NA)))
  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x, Trial = ifelse(Time >= pretrial.duration, Trial, 0))
  x <- dplyr::filter(x, Trial != 0, !is.na(Trial))
  x <- dplyr::select(x, -trialonset.time, -min)
  x <- dplyr::distinct(x, Trial, Time, .keep_all = TRUE)
  x <- dplyr::filter(x, !is.na(Subject))

  return(x)
}
