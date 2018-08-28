#' Pupil Preprocessing Function
#'
#' This function performs a linear interpolation over missing values.
#' The 'zoo' package is a dependency
#' @param x dataframe
#' @param type What type of interpolation to use? (default: cubic-spline)
#' @param eye.recorded Is there pupil data for the left, right, or both eyes?
#' @keywords interpolate
#' @export
#' @examples
#' pupil.interpolate(x, type = "cubic-spline", eye.recorded = "both")

pupil.interpolate <- function(x, type = "cubic-spline", eye.recorded = ""){
  if (eye.recorded=="both"){
    data <- dplyr::mutate(data,
                          L_Pupil_Diameter.mm = ifelse((L_Event=="Blink"|is.na(L_Event)), NA, L_Pupil_Diameter.mm),
                          R_Pupil_Diameter.mm = ifelse((R_Event=="Blink"|is.na(R_Event)), NA, R_Pupil_Diameter.mm))
    if (type=="cubic-spline"){
      data <- dplyr::group_by(data, Trial)
      data <- dplyr::mutate(data,
                            index_left = ifelse(is.na(L_Pupil_Diameter.mm), NA, row_number()),
                            index_left = zoo::na.approx(index_left, na.rm = FALSE),
                            index_right = ifelse(is.na(R_Pupil_Diameter.mm), NA, row_number()),
                            index_right = zoo::na.approx(index_right, na.rm = FALSE),
                            L_Pupil_Diameter.mm = zoo::na.spline(L_Pupil_Diameter.mm, na.rm = FALSE, x = index_left),
                            R_Pupil_Diameter.mm = zoo::na.spline(R_Pupil_Diameter.mm, na.rm = FALSE, x = index_right))
      data <- dplyr::select(data, -index_left, -index_right)
    } else if (type=="linear"){
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = zoo::na.approx(L_Pupil_Diameter.mm, na.rm = FALSE),
                            R_Pupil_Diameter.mm = zoo::na.approx(R_Pupil_Diameter.mm, na.rm = FALSE))
    }

  } else if (eye.recorded=="left"){
    data <- dplyr::mutate(data,
                          L_Pupil_Diameter.mm = ifelse((L_Event=="Blink"|is.na(L_Event)), NA, L_Pupil_Diameter.mm))
    if (type=="cubic-spline"){
      data <- dplyr::group_by(data, Trial)
      data <- dplyr::mutate(data,
                            index_left = ifelse(is.na(L_Pupil_Diameter.mm), NA, row_number()),
                            index_left = zoo::na.approx(index_left, na.rm = FALSE),
                            L_Pupil_Diameter.mm = zoo::na.spline(L_Pupil_Diameter.mm, na.rm = FALSE, x = index_left))
      data <- dplyr::select(data, -index_left)
    } else if (type=="linear"){
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = zoo::na.approx(L_Pupil_Diameter.mm, na.rm = FALSE))
    }

  } else if (eye.recorded=="right"){
    data <- dplyr::mutate(data,
                          R_Pupil_Diameter.mm = ifelse((R_Event=="Blink"|is.na(R_Event)), NA, R_Pupil_Diameter.mm))
    if (type=="cubic-spline"){
      data <- dplyr::group_by(data, Trial)
      data <- dplyr::mutate(data,
                            index_right = ifelse(is.na(R_Pupil_Diameter.mm), NA, row_number()),
                            index_right = zoo::na.approx(index_right, na.rm = FALSE),
                            R_Pupil_Diameter.mm = zoo::na.spline(R_Pupil_Diameter.mm, na.rm = FALSE, x = index_right))
      data <- dplyr::select(data, -index_right)
    } else if (type=="linear"){
      data <- dplyr::mutate(data,
                            R_Pupil_Diameter.mm = zoo::na.approx(R_Pupil_Diameter.mm, na.rm = FALSE))
    }

  }
  return(data)
}
