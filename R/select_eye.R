#' Select eye to keep for analysis
#'
#' If both eyes were recorded from, then this function will correlate the timeseries pupil data from both eyes
#' and select only one eye data to keep for further preprocessing and output.
#' This function will also remove trials with too much missing data, specified by `missing.allowed`.
#' @param x xframe
#' @param eye.use Which eye to use? Left or right
#' @keywords select
#' @export


select_eye <- function(x, eye.use = ""){
  left.recorded <- "L_Pupil_Diameter.mm" %in% colnames(x)
  right.recorded <- "R_Pupil_Diameter.mm" %in% colnames(x)
  ## Correlate and select Eyes
  if (left.recorded == TRUE & right.recorded == TRUE){
    # remove either left or right eye
    if (eye.use == "left"){
      x <- dplyr::rename(x,
                         Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                         Event = L_Event,
                         Gaze_Position.x = L_Gaze_Position.x,
                         Gaze_Position.y = L_Gaze_Position.y)
      x <- dplyr::select(x, -R_Pupil_Diameter.mm, -R_Event,
                         -R_Gaze_Position.x, -R_Gaze_Position.y)
    } else if (eye.use == "right"){
      x <- dplyr::rename(x,
                         Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                         Event = R_Event,
                         Gaze_Position.x = R_Gaze_Position.x,
                         Gaze_Position.y = R_Gaze_Position.y)
      x <- dplyr::select(x, -L_Pupil_Diameter.mm, -L_Event,
                         -L_Gaze_Position.x, -L_Gaze_Position.y)
    }
  }
  return(x)
}
