#' Evaluate the amount of missing data
#'
#' This function is for preprocessing baseline pupil x. It takes as input a x file that has been processed
#' with the .convert() functions. The output of this function is a preprocessed x file. Preprocessing steps can
#' include interpolation and smoothing
#' @param x dataframe
#' @param missing.allowed What proportion of missing data is allowed, per trial?
#' @param velocity The velocity threshold for Blink detection
#' @param margin The margin before and after Blink onset and offset
#' @keywords preprocess
#' @export
#'

pupil_missing <- function(x, missing.allowed = 1, velocity = "", margin = ""){
  if ("Pupil_Diameter.mm" %in% colnames(x)) {
    real_name <- "Pupil_Diameter.mm"
  } else {
    real_name <- "Pupil_Diameter.px"
  }

  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     Missing.Total = ifelse(is.na(pupil_val), 1, 0),
                     Missing.Total =
                       sum(Missing.Total, na.rm = TRUE) / dplyr::n())
  x <- dplyr::ungroup(x)
  x <- dplyr::filter(x, Missing.Total <= missing.allowed)
  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name

  return(x)
}
