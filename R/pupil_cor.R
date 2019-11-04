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
  if ("L_Pupil_Diameter.mm" %in% colnames(x)) {
    L_pupil_colname <- "L_Pupil_Diameter.mm"
    R_pupil_colname <- "R_Pupil_Diameter.mm"
  } else {
    L_pupil_colname <- "L_Pupil_Diameter.px"
    R_pupil_colname <- "R_Pupil_Diameter.px"
  }

  x <- dplyr::mutate(x, Pupils.r = stats::cor(get(L_pupil_colname),
                                              get(R_pupil_colname),
                                              use = "pairwise.complete.obs"),
                     Pupils.r = ifelse(is.na(Pupils.r), 0, Pupils.r))

  col_order <- c("Subject", "Trial", "Time", "Message",
                 "L_Pupil_Diameter.mm", "R_Pupil_Diameter.mm",
                 "L_Pupil_Diameter.px", "R_Pupil_Diameter.px", "L_Event",
                 "R_Event", "L_Gaze_Position.x", "L_Gaze_Position.y",
                 "R_Gaze_Position.x", "R_Gaze_Position.y", "Gaze.quality",
                 "Head_Dist.cm", "ms_conversion", "Message_Inserted")

  col_order <- colnames(x)[order(match(colnames(x), col_order))]

  x <- x[,col_order]

  return(x)
}
