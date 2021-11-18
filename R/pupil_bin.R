#' Bin pupil data
#'
#' Bin pupil data into smaller time-bins. This is useful to perform time-series
#' analyses or to just reduce the size of the data.
#'
#' See https://dr-jt.github.io/pupillometry/index.html for more information.
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

  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  x <- dplyr::group_by(x, Subject, Trial, add = TRUE)
  x <- dplyr::mutate(x,
                     TimeBin = trunc(Time / bin_length),
                     TimeBin = ifelse(Time < 0, TimeBin - 1, TimeBin),
                     Time = TimeBin * bin_length)
  x <- dplyr::group_by(x, TimeBin, add = TRUE)
  x <- dplyr::mutate(x, pupil_val = mean(pupil_val, na.rm = TRUE))

  bc <- "Pupil_Diameter_bc.mm" %in% colnames(x) |
    "Pupil_Diameter_bc.px" %in% colnames(x)
  if (bc == TRUE){
    real_name_bc <- ifelse("Pupil_Diameter_bc.mm" %in% colnames(x),
                           "Pupil_Diameter_bc.mm", "Pupil_Diameter_bc.px")
    colnames(x)[which(colnames(x) == real_name_bc)] <- "pupil_val_bc"

    x <- dplyr::mutate(x, pupil_val_bc = mean(pupil_val_bc, na.rm = TRUE))

    colnames(x)[which(colnames(x) == "pupil_val_bc")] <- real_name_bc
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -TimeBin)
  x <- dplyr::distinct(x, Subject, Trial, Time, .keep_all = TRUE)

  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name

  return(x)
}
