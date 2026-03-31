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
#' @param x data frame.
#' @param bin_length Length of bins in milliseconds.
#' @param bin_position Position of bins. "leading" places the time at the
#' beginning of the bin, "centered" places the time at the center of the bin.
#' @param na_allow Proportion of missing data allowed in each bin. If the proportion
#' of missing data in a bin exceeds this value, the pupil value for that bin
#' will be set to NA. Default is 0.5 (50%).
#'
#' @description
#' This function bins the pupil data into smaller time-bins. This is useful to
#' perform time-series analyses or to just reduce the size of the data.
#'
#' The function works by averaging the pupil values within each bin. The
#' `bin_length` argument specifies the length of the bins in milliseconds.
#'
#' The `bin_position` argument specifies whether the position of the bin in which
#' pupil values are averaged should begin at the corresponding Time value and
#' extend forward by `bin_length` milliseconds ("leading") or whether the bin
#' should be centered around the corresponding Time value ("centered").
#'
#' @import data.table
#' @export
#'

pupil_bin <- function(x,
                      bin_length = NULL,
                      bin_position = c("leading", "centered"),
                      na_allow = .5) {

  bin_position <- match.arg(bin_position)

  bin <- function(x, bin_length, bin_position, na_allow) {
    x <- dplyr::group_by(x, Subject, Trial, add = TRUE)

    if (bin_position == "leading") {
      x <- dplyr::mutate(x,
                         TimeBin = trunc(Time / bin_length),
                         TimeBin = ifelse(Time < 0, TimeBin - 1, TimeBin))
    }

    if (bin_position == "centered") {
      x <- dplyr::mutate(x,
                         TimeBin = (Time - (bin_length / 2))/ bin_length,
                         TimeBin = ifelse(TimeBin > 0, TimeBin + 1, TimeBin),
                         TimeBin = trunc(TimeBin))
    }

    x <- dplyr::mutate(x,
                       Time = TimeBin * bin_length)
    x <- dplyr::group_by(x, TimeBin, add = TRUE)
    x <- dplyr::mutate(x,
                       pupil_val = mean(pupil_val, na.rm = TRUE),
                       missing = sum(!is.na(pupil_val)),
                       pupil_val = ifelse(missing / bin_length > na_allow,
                                          NA, pupil_val))
    x <- dplyr::ungroup(x)
    x <- dplyr::select(x, -TimeBin)
    x <- dplyr::distinct(x, Subject, Trial, Time, .keep_all = TRUE)
  }

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- dtplyr::lazy_dt(x)
    x <- bin(x, bin_length, bin_position, na_allow)
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  return(x)
}
