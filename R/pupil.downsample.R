#' Pupil Preprocessing Function
#'
#' This function will reduce the sampling fequency
#' @param x dataframe
#' @param bin.length Length of bins to average
#' @param bc Logical. Was baseline correction done on the data? Is there a column labeled Pupil_Diameter_bc.mm?
#' @keywords downsample
#' @export
#' @examples
#' pupil.downsample(x, bin.length = 100)

pupil.downsample <- function(x, bin.length = "", bc = FALSE){
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, TimeBin = ifelse(min(Time)>=0, Time - min(Time), Time),
                     TimeBin = trunc((Time - min(Time))/bin.length),
                     Time = TimeBin*bin.length)
  x <- dplyr::group_by(x, Trial, TimeBin)
  x <- dplyr::mutate(x, Pupil_Diameter.mm = mean(Pupil_Diameter.mm, na.rm = TRUE))
  if (bc==TRUE){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = mean(Pupil_Diameter_bc.mm, na.rm = TRUE))
  }
  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -TimeBin)
  x <- dplyr::distinct(x, Trial, Time, .keep_all = TRUE)
  return(x)
}
