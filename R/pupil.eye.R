#' Preprocessnig Function
#'
#' If both eyes were recorded from, then this function will correlate the timeseries pupil data from both eyes
#' and select only one eye data to keep for further preprocessing and output.
#' This function will also remove trials with too much missing data, specified by `missing.allowed`.
#' @param x xframe
#' @param eye.recorded Is there pupil x for the left, right, or both eyes?
#' @param eye.use Which eye to use? Left or right
#' @param gazedata.include Logical. Include columns for x and y coordinates of eye gaze? (Default: FALSE)
#' @keywords pupil
#' @export
#' @examples
#'

pupil.eye <- function(x, eye.recorded = "", eye.use = "", gazedata.include = FALSE){

  ## Correlate and select Eyes
  if (eye.recorded == "both"){
    # correlate eyes
    x <- pupil.cor(x)
    # remove either left or right eye
    if (eye.use=="left"){
      x <- dplyr::rename(x,
                         Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                         Event = L_Event)
      x <- dplyr::select(x, -R_Pupil_Diameter.mm, -R_Event)
      if (gazedata.include==TRUE){
        x <- dplyr::rename(x,
                           Gaze_Position.x = L_Gaze_Position.x,
                           Gaze_Position.y = L_Gaze_Position.y)
        x <- dplyr::select(x, -R_Gaze_Position.x, -R_Gaze_Position.y)
      }
    } else if (eye.use=="right"){
      x <- dplyr::rename(x,
                         Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                         Event = R_Event)
      x <- dplyr::select(x, -L_Pupil_Diameter.mm, -L_Event)
      if (gazedata.include==TRUE){
        x <- dplyr::rename(x,
                           Gaze_Position.x = R_Gaze_Position.x,
                           Gaze_Position.y = R_Gaze_Position.y)
        x <- dplyr::select(x, -L_Gaze_Position.x, -L_Gaze_Position.y)
      }
    }
  } else if (eye.recorded=="left"){
    x <- dplyr::rename(x, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                       Event = L_Event)
    if (gazedata.include==TRUE){
      x <- dplyr::rename(x,
                         Gaze_Position.x = L_Gaze_Position.x,
                         Gaze_Position.y = L_Gaze_Position.y)
    }
  } else if (eye.recorded=="right"){
    x <- dplyr::rename(x, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                       Event = R_Event)
    if (gazedata.include==TRUE){
      x <- dplyr::rename(x,
                         Gaze_Position.x = R_Gaze_Position.x,
                         Gaze_Position.y = R_Gaze_Position.y)
    }
  }
  return(x)
}
