#' Preprocessnig Function
#'
#' This function is for preprocessing baseline pupil x. It takes as input a x file that has been processed
#' with the .convert() functions. The output of this function is a preprocessed x file. Preprocessing steps can
#' include interpolation and smoothing
#' @param x xframe
#' @param missing.allowed What proportion of missing data is allowed, per trial?
#' @param velocity The velocity threshold for Blink detection
#' @param margin The margin before and after Blink onset and offset
#' @keywords preprocess
#' @export
#' @examples
#' pupil.missing(x)

pupil.missing <- function(x, missing.allowed = 1, velocity = "", margin = ""){
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     Pupil_Diameter.mm = ifelse(Event=="Blink"|is.na(Event)|Pupil_Diameter.mm==0, NA, Pupil_Diameter.mm),
                     Missing.Total = ifelse(is.na(Pupil_Diameter.mm), 1, NA),
                     Missing.Total = sum(Missing.Total, na.rm = TRUE)/n())
  x <- dplyr::ungroup(x)
  x <- dplyr::filter(x, Missing.Total<=missing.allowed)
  return(x)
}
