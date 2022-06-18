#' Set timing
#'
#' Sets the timing variable relative to the onset of an event. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Changes the values in the Time column.
#'
#' @section Relative Timing:
#'
#' Raw eye tracker data will provide timing values in absolute values from the
#' start of recording. More useful, are timing values that are relative to
#' the onset of an event.
#'
#' This function allows you to set the timing variable relative to the onset of
#' an event in the Stimulus or Message columns with the `onset_message`
#' argument call. For instance, you can set the timing relative to the onset
#' of a fixation or the onset of the first stimulus in the trial. You can use
#' this function on the data as many times as you like.
#'
#' @param x dataframe.
#' @param onset_message Message string that marks the start of the event.
#' @param match Is message string an "exact" match or a "pattern" match?
#' @param trial_onset.message deprecated. Use onset_message.
#' @param pretrial.duration deprecated.
#' @param trialonset.message deprecated.
#' @param pre_trial.duration deprecated.
#' @export
#'

set_timing <- function(x, onset_message = NULL, match = "exact",
                       trial_onset.message = NULL, pretrial.duration = 0,
                       trialonset.message = NULL, pre_trial.duration = NULL){

  x <- dtplyr::lazy_dt(x)

  if (!is.null(trialonset.message)) {
    trial_onset.message <- trialonset.message
  }
  if (!is.null(pre_trial.duration)) {
    pretrial.duration <- pre_trial.duration
  }
  pretrial.duration <- abs(pretrial.duration) * -1
  if (!is.null(trial_onset.message)) {
    onset_message <- trial_onset.message
  }

  if ("Stimulus" %in% colnames(x)) {
    x <- dplyr::group_by(x, Trial, Stimulus)
    x <- dplyr::mutate(x,
                       onset.time = ifelse(Stimulus == onset_message,
                                                min(Time, na.rm = TRUE), NA))
    x <- dplyr::group_by(x, Trial)
    x <- tidyr::fill(x, onset.time, .direction = "updown")
    x <- dplyr::mutate(x, Time = Time - onset.time)
  } else if ("Message" %in% colnames(x)) {
    x <- dplyr::group_by(x, Trial)
    if (match == "exact") {
      x <- dplyr::mutate(x,
                         onset.time = ifelse(Message == onset_message,
                                                  Time, NA))
    } else if (match == "pattern") {
      x <- dplyr::mutate(x,
                         onset.time =
                           ifelse(stringr::str_detect(Message, onset_message),
                                  Time, NA))
    }
    x <- dplyr::mutate(x,
                       min = min(onset.time, na.rm = TRUE),
                       onset.time = ifelse(is.na(onset.time) |
                                                  onset.time != min,
                                                NA, onset.time),
                       onset.time = zoo::na.locf(onset.time,
                                                      na.rm = FALSE),
                       onset.time = zoo::na.locf(onset.time,
                                                      na.rm = FALSE,
                                                      fromLast = TRUE),
                       Time = Time - onset.time)
    x <- dplyr::select(x, -min)
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -onset.time)
  x <- dplyr::distinct(x, Trial, Time, .keep_all = TRUE)
  x <- dplyr::filter(x, !is.na(Subject))

  x <- dplyr::as_tibble(x)
  return(x)
}
