#' Pupil Preprocessing Function
#'
#' This function will reduce the sampling fequency to a specified Hz
#' @param x dataframe
#' @param Hz The sampling frequency to reduce pupil data to
#' @keywords downsample
#' @export
#' @examples
#' downsample(x, Hz = 100)

downsample <- function(x, Hz = ""){
  int <- 1000/Hz
  x <- dplyr::mutate(x, Time = (trunc((Time+int)/int,0)-1)*int)
  sumx <- dplyr::group_by(x, Subject, Trial, Time)
  sumx <- dplyr::summarise(sumx, Pupil_Diameter.mm = mean(Pupil_Diameter.mm, na.rm = TRUE))
  sumx <- dplyr::ungroup(sumx)
  x <- dplyr::select(x, -Pupil_Diameter.mm)
  x <- merge(sumx, x, by = c("Subject", "Trial", "Time"))
  x <- dplyr::arrange(x, Subject, Trial, Time)
  x <- x[!duplicated(x[c("Subject", "Trial", "Time")]),]
  if ("Missing.Total"%in%colnames(x)){
    x <- dplyr::filter(x, !is.na(Missing.Total))
  }
  return(x)
}
