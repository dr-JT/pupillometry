#' Deblink
#'
#' Deblink pupil data with the option to extend blinks before and after
#' blink detection. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Changes values in column containing pupil data.
#'
#' @section Plot inspection:
#'
#' To inspect how the preprocesing step changed pupil size values,
#' use `plot = TRUE`.
#'
#' Warning: this will create a separate plot for every trial and therefore can
#' be time consuming and overwhelming. The plot argument is meant for initial
#' exploratory steps to determine the appropriate preprocessing parameters.
#'
#' @param x dataframe.
#' @param extend How many milliseconds to extend blinks
#'     before and after blink detection.
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_trial what trial(s) to plot. default = "all"
#' @import data.table
#' @export
#'

pupil_deblink <- function(x, extend = 0, plot = FALSE, plot_trial = "all") {

  x_before <- x

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_eye_name <- eye
    colnames(x)[which(colnames(x) == real_eye_name)] <- "pupil_val"
    real_event_name <- dplyr::case_when(
      stringr::str_detect(real_eye_name, "L_") ~ "L_Eye_Event",
      stringr::str_detect(real_eye_name, "R_") ~ "R_Eye_Event",
      TRUE ~ "Eye_Event")
    colnames(x)[which(colnames(x) == real_event_name)] <- "eye_event"

    #### Define blink + extension samples ####
    x <- dtplyr::lazy_dt(x)
    x <- dplyr::mutate(x, .by = Trial,
                       blink =
                         ifelse(!is.na(eye_event) & eye_event == "Blink", 1,
                                ifelse(is.na(pupil_val), 1, 0)),
                       blink.lag = dplyr::lag(blink),
                       blink.lead = dplyr::lead(blink),
                       blink.start =
                         ifelse(blink == 1 & !is.na(blink.lag) & blink.lag == 0,
                                Time, as.numeric(NA)),
                       blink.start = zoo::na.locf(blink.start, na.rm = FALSE,
                                                  fromLast = TRUE),
                       blink.end =
                         ifelse(blink == 1 & !is.na(blink.lead) & blink.lead == 0,
                                Time, as.numeric(NA)),
                       blink.end = zoo::na.locf(blink.end, na.rm = FALSE),
                       blink =
                         ifelse(!is.na(blink.start) &
                                  Time >= blink.start - extend &
                                  Time <= blink.start, 1, blink),
                       blink = ifelse(!is.na(blink.end) &
                                        Time <= blink.end + extend &
                                        Time >= blink.end, 1, blink))

    x <- dplyr::select(x, -blink.lag, -blink.lead,
                       -blink.start, -blink.end)
    ##########################################

    x <- dplyr::mutate(x,
                       pupil_val = ifelse(pupil_val == 0 |
                                            blink == 1,
                                          as.numeric(NA), pupil_val))
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_eye_name
    colnames(x)[which(colnames(x) == "eye_event")] <- real_event_name
  }

  if (plot == TRUE) pupil_plot(x_before, x, trial = plot_trial,
                               sub_title =
                                 paste("pupil_deblink(extend = ", extend, ")",
                                       sep = ""))

  x <- dplyr::select(x, -blink)

  return(x)
}
