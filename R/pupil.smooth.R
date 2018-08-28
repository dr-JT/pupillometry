#' Pupil Preprocessing Function
#'
#' This function performs a smoothing function over an array of data
#' The 'zoo' package is a dependency
#' @param x dataframe
#' @param type The type of smoothing function to apply. Hann or moving window average
#' @param window The size of the smoothing window (default = 5). Value is in milliseconds
#' @param hz The recording frequency (used to calculate window size)
#' @param eye.recorded Is there pupil data for the left, right, or both eyes?
#' @keywords smooth
#' @export
#' @examples
#' pupil.smooth(x, type = "hann", window = 11, eye.recorded = "both")

pupil.smooth <- function(x, type = "hann", window = 5, hz = "", eye.recorded = ""){
  if (hz==""){
    hz <- x$hz[1]
  }
  window <- window/(hz/1000)
  data.list <- data.frame()
  for (i in unique(x$Trial)[which(!is.na(unique(x$Trial)))]){
    ## Reduce baseline pupil data
    data <- dplyr::filter(x, Trial==i)

    ## Preprocessing steps
    if (eye.recorded=="both"){
      if (type=="hann"){
        data <- dplyr::mutate(data,
                              L_Pupil_Diameter.mm = dplR::hanning(L_Pupil_Diameter.mm, n = window),
                              R_Pupil_Diameter.mm = dplR::hanning(R_Pupil_Diameter.mm, n = window))
      } else if (type=="mwa"){
        data <- dplyr::mutate(data,
                              L_Pupil_Diameter.mm = zoo::rollapply(L_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                              L_Pupil_Diameter.mm = ifelse(is.na(L_Pupil_Diameter.mm),NA,L_Pupil_Diameter.mm),
                              R_Pupil_Diameter.mm = zoo::rollapply(R_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                              R_Pupil_Diameter.mm = ifelse(is.na(R_Pupil_Diameter.mm),NA,R_Pupil_Diameter.mm))
      }

    } else if (eye.recorded=="left"){
      if (type=="hann"){
        data <- dplyr::mutate(data, dplR::hanning(L_Pupil_Diameter.mm, n = window))
      } else if (type=="mwa"){
        data <- dplyr::mutate(data,
                              Pupil_Diameter.mm = zoo::rollapply(L_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                              Pupil_Diameter.mm = ifelse(is.na(L_Pupil_Diameter.mm),NA,L_Pupil_Diameter.mm))
      }

    } else if (eye.recorded=="right"){
      if (type=="hann"){
        data <- dplyr::mutate(data, R_Pupil_Diameter.mm = dplR::hanning(R_Pupil_Diameter.mm, n = window))
      } else if (type=="mwa"){
        data <- dplyr::mutate(data,
                              R_Pupil_Diameter.mm = zoo::rollapply(R_Pupil_Diameter.mm, width = window, FUN = mean, partial = TRUE, na.rm = TRUE),
                              R_Pupil_Diameter.mm = ifelse(is.na(R_Pupil_Diameter.mm),NA,R_Pupil_Diameter.mm))
      }

    }
    data.list <- rbind(data.list,data.frame(data))

  }
  return(data.list)
}
