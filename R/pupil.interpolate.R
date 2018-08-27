#' Pupil Preprocessing Function
#'
#' This function performs a linear interpolation over missing values.
#' The 'zoo' package is a dependency
#' @param x dataframe
#' @param eye.recorded Is there pupil data for the left, right, or both eyes?
#' @keywords interpolate
#' @export
#' @examples
#' pupil.interpolate(x)

pupil.interpolate <- function(x, eye.recorded = ""){
  data.list <- data.frame()
  for (i in unique(x$Trial)[which(!is.na(unique(x$Trial)))]){
    ## Reduce baseline pupil data
    data <- dplyr::filter(x, Trial==i)

    ## Preprocessing steps
    if (eye.recorded=="both"){
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = ifelse((L_Event=="Blink"|is.na(L_Event)), NA, L_Pupil_Diameter.mm),
                            R_Pupil_Diameter.mm = ifelse((R_Event=="Blink"|is.na(R_Event)), NA, R_Pupil_Diameter.mm))

      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = zoo::na.approx(L_Pupil_Diameter.mm, na.rm = FALSE),
                            L_Pupil_Diameter.mm = ifelse(is.na(L_Pupil_Diameter.mm),NA,L_Pupil_Diameter.mm),
                            R_Pupil_Diameter.mm = zoo::na.approx(R_Pupil_Diameter.mm, na.rm = FALSE),
                            R_Pupil_Diameter.mm = ifelse(is.na(R_Pupil_Diameter.mm),NA,R_Pupil_Diameter.mm))
    } else if (eye.recorded=="left"){
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = ifelse((L_Event=="Blink"|is.na(L_Event)), NA, L_Pupil_Diameter.mm),
                            Eye = "Left")

      data <- dplyr::mutate(data,
                            Pupil_Diameter.mm = zoo::na.approx(L_Pupil_Diameter.mm, na.rm = FALSE),
                            Pupil_Diameter.mm = ifelse(is.na(L_Pupil_Diameter.mm),NA,L_Pupil_Diameter.mm))
    } else if (eye.recorded=="right"){
      data <- dplyr::mutate(data,
                            R_Pupil_Diameter.mm = ifelse((R_Event=="Blink"|is.na(R_Event)), NA, R_Pupil_Diameter.mm),
                            Eye = "Right")

      data <- dplyr::mutate(data,
                            R_Pupil_Diameter.mm = zoo::na.approx(R_Pupil_Diameter.mm, na.rm = FALSE),
                            R_Pupil_Diameter.mm = ifelse(is.na(R_Pupil_Diameter.mm),NA,R_Pupil_Diameter.mm))
    }
    data.list <- rbind(data.list,data.frame(data))
  }
  return(data.list)
}
