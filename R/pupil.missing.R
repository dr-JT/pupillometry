#' Preprocessnig Function
#'
#' This function is for preprocessing baseline pupil data. It takes as input a data file that has been processed
#' with the .convert() functions. The output of this function is a preprocessed data file. Preprocessing steps can
#' include interpolation and smoothing
#' @param x dataframe
#' @param eye.recorded Is there pupil data for the left, right, or both eyes?
#' @keywords preprocess
#' @export
#' @examples
#' pupil.missing(x)

pupil.missing <- function(x, eye.recorded = ""){
  data.list <- data.frame()
  for (i in unique(x$Trial)[which(!is.na(unique(x$Trial)))]){
    ## Reduce baseline pupil data
    data <- dplyr::filter(x, Trial==i)

    if (eye.recorded=="both"){
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = ifelse(L_Pupil_Diameter.mm==0, NA, L_Pupil_Diameter.mm),
                            R_Pupil_Diameter.mm = ifelse(R_Pupil_Diameter.mm==0, NA, R_Pupil_Diameter.mm))
      left.missing <- length(is.na(data$L_Pupil_Diameter.mm)[is.na(data$L_Pupil_Diameter.mm)==TRUE])/length(is.na(data$L_Pupil_Diameter.mm))
      right.missing <- length(is.na(data$R_Pupil_Diameter.mm)[is.na(data$R_Pupil_Diameter.mm)==TRUE])/length(is.na(data$R_Pupil_Diameter.mm))
      data <- dplyr::mutate(data, L_Missing.Total = left.missing, R_Missing.Total = right.missing)

    } else if (eye.recorded=="left"){
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm = ifelse(L_Pupil_Diameter.mm==0, NA, L_Pupil_Diameter.mm),
                            Eye = "Left")
      left.missing <- length(is.na(data$L_Pupil_Diameter.mm)[is.na(data$L_Pupil_Diameter.mm)==TRUE])/length(is.na(data$L_Pupil_Diameter.mm))
      data <- dplyr::mutate(data, L_Missing.Total = left.missing)
    } else if (eye.recorded=="right"){
      data <- dplyr::mutate(data,
                            R_Pupil_Diameter.mm = ifelse(R_Pupil_Diameter.mm==0, NA, R_Pupil_Diameter.mm),
                            Eye = "Right")
      right.missing <- length(is.na(data$R_Pupil_Diameter.mm)[is.na(data$R_Pupil_Diameter.mm)==TRUE])/length(is.na(data$R_Pupil_Diameter.mm))
      data <- dplyr::mutate(data, R_Missing.Total = right.missing)
    }
    data.list <- rbind(data.list, data)
  }
  return(data.list)
}
