#' Deblink
#'
#' Deblink pupil data with the option to extend blinks before and after
#' blink detection.
#'
#' See https://dr-jt.github.io/pupillometry/index.html for more information.
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
#' @export
#'

pupil_deblink <- function(x, extend = 0, plot = FALSE){

  x_before <- x

  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                       "Pupil_Diameter.mm", "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  x <- dplyr::mutate(x,
                     blink =
                       ifelse(!is.na(Eye_Event) & Eye_Event == "Blink", 1,
                              ifelse(is.na(pupil_val), 1, 0)),
                     blink.lag = dplyr::lag(blink),
                     blink.lead = dplyr::lead(blink),
                     blink.start = ifelse(blink == 1 &
                                            !is.na(blink.lag) &
                                            blink.lag == 0, Time, NA),
                     blink.start = zoo::na.locf(blink.start, na.rm = FALSE,
                                                fromLast = TRUE),
                     blink.end = ifelse(blink == 1 &
                                          !is.na(blink.lead) &
                                          blink.lead == 0, Time, NA),
                     blink.end = zoo::na.locf(blink.end, na.rm = FALSE),
                     blink =
                       ifelse(!is.na(blink.start) &
                                Time >= blink.start - extend &
                                Time <= blink.start, 1, blink),
                     blink = ifelse(!is.na(blink.end) &
                                      Time <= blink.end + extend &
                                      Time >= blink.end, 1, blink),
                     pupil_val = ifelse(pupil_val == 0 |
                                          blink == 1,
                                        NA, pupil_val))

  x <- dplyr::select(x, -blink, -blink.lag, -blink.lead,
                     -blink.start, -blink.end)

  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name

  if (plot == TRUE) pupil_plot(x_before, x)

  return(x)
}
