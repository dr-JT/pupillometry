#' Select eye to keep for analysis
#'
#' If both eyes were recorded from, then this function will correlate the
#' timeseries pupil data from both eyes and select only one eye data to keep
#' for further preprocessing and output. This function will also remove
#' trials with too much missing data, specified by `missing_allowed`.
#' @param x xframe
#' @param eye_use Which eye to use? Left or right
#' @export
#'


select_eye <- function(x, eye_use = ""){

  l_pupil <- ifelse("L_Pupil_Diameter.mm" %in% colnames(data),
                    "L_Pupil_Diameter.mm", "L_Pupil_Diameter.px")
  r_pupil <- ifelse("R_Pupil_Diameter.mm" %in% colnames(data),
                    "R_Pupil_Diameter.mm", "R_Pupil_Diameter.px")

  if (eye_use == "left") {
    x <- dplyr::rename(x,
                       Pupil_Diameter.mm = get(l_pupil),
                       Eye_Event = L_Eye_Event)
    if ("L_Gaze_Position.x" %in% colnames(x)) {
      x <- dplyr::rename(x,
                         Gaze_Position.x = L_Gaze_Position.x,
                         Gaze_Position.y = L_Gaze_Position.y)
    }
    x <- dplyr::select(x, -tidyselect::any_of(r_pupil),
                       -tidyselect::any_of(c("R_Gaze_Position.x",
                                             "R_Gaze_Position.y")),
                       -tidyselect::any_of("R_Eye_Event"))
  } else if (eye_use == "right") {
    x <- dplyr::rename(x,
                       Pupil_Diameter.mm = get(r_pupil),
                       Eye_Event = R_Eye_Event)
    if ("R_Gaze_Position.x" %in% colnames(x)) {
      x <- dplyr::rename(x,
                         Gaze_Position.x = R_Gaze_Position.x,
                         Gaze_Position.y = R_Gaze_Position.y)
    }
    x <- dplyr::select(x, -tidyselect::any_of(l_pupil),
                       -tidyselect::any_of(c("L_Gaze_Position.x",
                                             "L_Gaze_Position.y")),
                       -tidyselect::any_of("L_Eye_Event"))
  }
  return(x)
}
