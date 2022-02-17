#' Trial removal
#'
#' This function will remove trials that have too much missing data. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Removes rows for trials with too much missing data. Adds a `Pupil_Missing`
#' or `Pupil_Missing_1` and `Pupil_Missing_2` columns.
#'
#' @section Usage:
#'
#' This can be used:
#'
#' 1) after artifact removal and before smoothing / interpolation
#'
#' 2) after smoothing / interpolation
#'
#' or
#'
#' 3) both
#'
#' @param x dataframe.
#' @param missing_allowed What proportion of missing data is allowed, per trial?
#' @export
#'

pupil_missing <- function(x, missing_allowed = 1){

  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  if ("Pupil_Missing" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Pupil_Missing")] <- "Pupil_Missing_1"
  }

  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     Pupil_Missing = ifelse(is.na(pupil_val), 1, 0),
                     Pupil_Missing =
                       sum(Pupil_Missing, na.rm = TRUE) / dplyr::n())
  x <- dplyr::ungroup(x)
  x <- dplyr::filter(x, Pupil_Missing <= missing_allowed)

  if ("Pupil_Missing_1" %in% colnames(x)) {
    colnames(x)[which(colnames(x) == "Pupil_Missing")] <- "Pupil_Missing_2"
  }
  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name

  return(x)
}
