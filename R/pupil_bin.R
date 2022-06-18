#' Bin pupil data
#'
#' Bin pupil data into smaller time-bins. This is useful to perform time-series
#' analyses or to just reduce the size of the data. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Reduces the number of rows into discrete time-bins. Changes the values in
#' Time and pupil columns.
#'
#' @param x dataframe.
#' @param bin_length Length of bins in milliseconds.
#' @export
#'

pupil_bin <- function(x, bin_length = NULL){

  x <- dtplyr::lazy_dt(x)

  bin <- function(x, bin_length) {
    x <- dplyr::group_by(x, Subject, Trial, add = TRUE)
    x <- dplyr::mutate(x,
                       TimeBin = trunc(Time / bin_length),
                       TimeBin = ifelse(Time < 0, TimeBin - 1, TimeBin),
                       Time = TimeBin * bin_length)
    x <- dplyr::group_by(x, TimeBin, add = TRUE)
    x <- dplyr::mutate(x, pupil_val = mean(pupil_val, na.rm = TRUE))
    x <- dplyr::ungroup(x)
    x <- dplyr::select(x, -TimeBin)
    x <- dplyr::distinct(x, Subject, Trial, Time, .keep_all = TRUE)
  }

  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- bin(x, bin_length)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  x <- dplyr::as_tibble(x)
  return(x)
}
