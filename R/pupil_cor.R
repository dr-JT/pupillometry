#' Add column that is the correlation between left and rigth eyes
#'
#' This function is for preprocessing baseline pupil data. It takes as input a data file that has been processed
#' with the .convert() functions. The output of this function is a preprocessed data file. Preprocessing steps can
#' include interpolation and smoothing
#' @param x dataframe
#' @keywords eye
#' @export
#'

pupil_cor <- function(x){
  x <- dplyr::mutate(x, Pupils.r = stats::cor(L_Pupil_Diameter.mm,
                                              R_Pupil_Diameter.mm,
                                              use = "pairwise.complete.obs"),
                     Pupils.r = ifelse(is.na(Pupils.r), 0, Pupils.r))
  x <- dplyr::select(x, Subject, Trial, Time, Message, dplyr::contains("mm"),
                     Pupils.r, dplyr::contains("Event"),
                     dplyr::contains("Gaze"), Head_Dist.cm, ms_conversion,
                     Message_Inserted)
  return(x)
}
