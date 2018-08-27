#' Pupil Preprocessing Function
#'
#' This function performs a smoothing function over an array of data
#' The 'zoo' package is a dependency
#' @param x dataframe
#' @param eye.recorded Is there pupil data for the left, right, or both eyes?
#' @param window The size of the smoothing window (default = 5)
#' @keywords smooth
#' @export
#' @examples
#' pupil.smooth(x)

pupil.smooth <- function(x, eye.recorded = "", window = 5){
  data.list <- data.frame()
  for (i in unique(x$Trial)[which(!is.na(unique(x$Trial)))]){
    ## Reduce baseline pupil data
    data <- dplyr::filter(x, Trial==i)

    ## Preprocessing steps
    if (eye.recorded=="both"){
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = zoo::rollapply(L_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                            L_Pupil_Diameter.mm = ifelse(is.na(L_Pupil_Diameter.mm),NA,L_Pupil_Diameter.mm),
                            R_Pupil_Diameter.mm = zoo::rollapply(R_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                            R_Pupil_Diameter.mm = ifelse(is.na(R_Pupil_Diameter.mm),NA,R_Pupil_Diameter.mm))

    } else if (eye.recorded=="left"){
      data <- dplyr::mutate(data,
                            Pupil_Diameter.mm = zoo::rollapply(L_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                            Pupil_Diameter.mm = ifelse(is.na(L_Pupil_Diameter.mm),NA,L_Pupil_Diameter.mm))

    } else if (eye.recorded=="right"){
      data <- dplyr::mutate(data,
                            R_Pupil_Diameter.mm = zoo::rollapply(R_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                            R_Pupil_Diameter.mm = ifelse(is.na(R_Pupil_Diameter.mm),NA,R_Pupil_Diameter.mm))
    }
    data.list <- rbind(data.list,data.frame(data))

  }
  return(data.list)
}
