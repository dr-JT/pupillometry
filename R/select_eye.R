#' Select eye
#'
#' Choose which eye to use for analysis. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' This function removes columns related to the non-selected eye and renames
#' columns of the selected eye by removing the L_ or R_ prefix. It also adds
#' a column `Pupil.r`.
#'
#' If both eyes were recorded from, then this function will correlate the
#' pupil data from both eyes and select only one eye data to keep
#' for further preprocessing and output.
#'
#' @param x dataframe.
#' @param eye_use Which eye to use? left or right?
#' @export
#'


select_eye <- function(x, eye_use = ""){

  l_pupil <- ifelse("L_Pupil_Diameter.mm" %in% colnames(x),
                    "L_Pupil_Diameter.mm", "L_Pupil_Diameter.px")
  r_pupil <- ifelse("R_Pupil_Diameter.mm" %in% colnames(x),
                    "R_Pupil_Diameter.mm", "R_Pupil_Diameter.px")

  if (eye_use == "left") {
    colnames(x)[which(colnames(x) == l_pupil)] <- stringr::str_remove(l_pupil,
                                                                      "L_")
    if ("L_Eye_Event" %in% colnames(x)) {
      x <- dplyr::rename(x,
                         Eye_Event = L_Eye_Event)
    }
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
    colnames(x)[which(colnames(x) == r_pupil)] <- stringr::str_remove(r_pupil,
                                                                      "R_")
    if ("R_Eye_Event" %in% colnames(x)) {
      x <- dplyr::rename(x,
                         Eye_Event = R_Eye_Event)
    }
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
