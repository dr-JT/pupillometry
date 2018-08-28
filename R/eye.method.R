#' Preprocessnig Function
#'
#' This function is for preprocessing baseline pupil data. It takes as input a data file that has been processed
#' with the .convert() functions. The output of this function is a preprocessed data file. Preprocessing steps can
#' include interpolation and smoothing
#' @param x dataframe
#' @param eye.recorded Is there pupil data for the left, right, or both eyes?
#' @param method Which method of using left and right eye data? Average, missing, left, or right.
#' @param cor.criteria If using the average method, then what is the criteria of how highly the two eyes need to correlate?
#' @keywords eye
#' @export
#' @examples
#' eye.method(x)

eye.method <- function(x, eye.recorded = "", method = "average", cor.criteria = .9){
  data.list <- data.frame()
  for (i in unique(x$Trial)[which(!is.na(unique(x$Trial)))]){
    ## Reduce baseline pupil data
    data <- dplyr::filter(x, Trial==i)

    ## Preprocessing steps
    if (method=="average"){
      left.missing <- data$L_Missing.Total[1]
      right.missing <- data$R_Missing.Total[1]

      eyes.corr <- stats::cor(data$L_Pupil_Diameter.mm,
                              data$R_Pupil_Diameter.mm,
                              use = "pairwise.complete.obs")
      if (is.na(eyes.corr)){
        eyes.corr <- 0
      }
      if (eyes.corr>=cor.criteria){
        data <- dplyr::mutate(data, Pupil_Diameter.mm = rowMeans(dplyr::select(data, L_Pupil_Diameter.mm, R_Pupil_Diameter.mm), na.rm = TRUE),
                              Missing.Total = rowMeans(dplyr::select(data, L_Missing.Total, R_Missing.Total), na.rm = TRUE),
                              Eye = "Average", Eyes.r = eyes.corr)
        data <- dplyr::select(data, -L_Pupil_Diameter.mm, -R_Pupil_Diameter.mm,
                              -L_Missing.Total, -R_Missing.Total)
      } else {
        if (left.missing<=right.missing){
          data <- dplyr::mutate(data, Eye = "Left", Eyes.r = eyes.corr)
          data <- dplyr::select(data, -R_Pupil_Diameter.mm, -R_Missing.Total)
          data <- dplyr::rename(data, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                                Missing.Total = L_Missing.Total)
        } else {
          data <- dplyr::mutate(data, Eye = "Right", Eyes.r = eyes.corr)
          data <- dplyr::select(data, -L_Pupil_Diameter.mm, -L_Missing.Total)
          data <- dplyr::rename(data, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                                Missing.Total = R_Missing.Total)
        }
      }

    } else if (method=="missing"){
      left.missing <- data$L_Missing.Total[1]
      right.missing <- data$R_Missing.Total[1]

      if (left.missing<=right.missing){
        data <- dplyr::mutate(data, Eye = "Left")
        data <- dplyr::rename(data, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                              Missing.Total = L_Missing.Total)
      } else {
        data <- dplyr::mutate(data, Eye = "Right")
        data <- dplyr::rename(data, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                              Missing.Total = R_Missing.Total)
      }

    } else if (method=="left"){
      data <- dplyr::mutate(data, Eye = "Left")
      data <- dplyr::rename(data, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                            Missing.Total = L_Missing.Total)
    } else if (method=="right"){
      data <- dplyr::mutate(data, Eye = "Right")
      data <- dplyr::rename(data, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                            Missing.Total = R_Missing.Total)
    }
    data.list <- rbind(data.list,data.frame(data))
  }
  return(data.list)
}
