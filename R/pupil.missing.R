#' Preprocessnig Function
#'
#' This function is for preprocessing baseline pupil x. It takes as input a x file that has been processed
#' with the .convert() functions. The output of this function is a preprocessed x file. Preprocessing steps can
#' include interpolation and smoothing
#' @param x xframe
#' @param eye.recorded Is there pupil x for the left, right, or both eyes?
#' @param velocity The velocity threshold for Blink detection
#' @param margin The margin before and after Blink onset and offset
#' @keywords preprocess
#' @export
#' @examples
#' pupil.missing(x)

pupil.missing <- function(x, eye.recorded = "", velocity = "", margin = ""){
  x <- dplyr::group_by(x, Trial)
  if (eye.recorded=="both"){
    x <- dplyr::mutate(x,
                       L_Pupil_Diameter.mm = ifelse(L_Event=="Blink"|is.na(L_Event)|L_Pupil_Diameter.mm==0, NA, L_Pupil_Diameter.mm),
                       L_Missing.Total = ifelse(is.na(L_Pupil_Diameter.mm), 1, NA),
                       L_Missing.Total = sum(L_Missing.Total, na.rm = TRUE)/n(),
                       R_Pupil_Diameter.mm = ifelse(R_Event=="Blink"|is.na(R_Event)|R_Pupil_Diameter.mm==0, NA, R_Pupil_Diameter.mm),
                       R_Missing.Total = ifelse(is.na(R_Pupil_Diameter.mm), 1, NA),
                       R_Missing.Total = sum(R_Missing.Total, na.rm = TRUE)/n())

  } else if (eye.recorded=="left"|eye.recorded=="right"){
    x <- dplyr::mutate(x,
                       Pupil_Diameter.mm = ifelse(Event=="Blink"|is.na(Event)|Pupil_Diameter.mm==0, NA, Pupil_Diameter.mm),
                       Missing.Total = ifelse(is.na(Pupil_Diameter.mm), 1, NA),
                       Missing.Total = sum(Missing.Total, na.rm = TRUE)/n())
  }
  x <- dplyr::ungroup(x)
  x <- dplyr::filter(x, Missing.Total!=1)
  return(x)
}
