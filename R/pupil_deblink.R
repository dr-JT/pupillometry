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
#' @param min_blink_duration Minimum duration of a blink in milliseconds.
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_trial what trial(s) to plot. default = "all"
#' @import data.table
#' @export
#'

pupil_deblink <- function(x, extend = 0,
                          min_blink_duration = 0,
                          plot = FALSE, plot_trial = "all") {

  x_before <- dplyr::as_tibble(x)

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
    x <- x |>
      dplyr::mutate(
        pupil_missing = is.na(pupil_val),

        missing_run = cumsum(
          pupil_missing !=
            dplyr::lag(pupil_missing, default = FALSE)
        ),

        .by = Trial
      ) |>
      dplyr::mutate(
        missing_duration = dplyr::if_else(
          dplyr::first(pupil_missing),
          max(Time, na.rm = TRUE) - min(Time, na.rm = TRUE),
          0
        ),

        .by = c(Trial, missing_run)
      ) |>
      dplyr::mutate(.by = Trial,
                       blink = dplyr::if_else(
                          (!is.na(eye_event) & eye_event == "Blink") |
                            (
                              pupil_missing &
                                missing_duration >= min_blink_duration
                            ),
                          1L,
                          0L
                        ),
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
                       -blink.start, -blink.end,
                       -pupil_missing, -missing_run, -missing_duration)
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
