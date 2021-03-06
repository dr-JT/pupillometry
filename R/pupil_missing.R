#' Remove trials with too much missing data
#'
#' This function will remove trials that have too much missing data
#' @param x dataframe
#' @param missing_allowed What proportion of missing data is allowed, per trial?
#' @export
#'

pupil_missing <- function(x, missing_allowed = 1, velocity = "", margin = ""){

  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")

  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     Missing_Trial = ifelse(is.na(pupil_val), 1, 0),
                     Missing_Trial =
                       sum(Missing_Trial, na.rm = TRUE) / dplyr::n())
  x <- dplyr::ungroup(x)
  x <- dplyr::filter(x, Missing_Trial <= missing_allowed)
  x <- dplyr::select(x, -Missing_Trial)
  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name

  return(x)
}
