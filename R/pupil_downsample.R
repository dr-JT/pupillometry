#' Downsample pupil data
#'
#' This function will reduce the sampling fequency
#' @param x dataframe
#' @param bin.length Length of bins to average
#' @param bc Logical. Was baseline correction done on the data? Is there a column labeled Pupil_Diameter_bc.mm?
#' @keywords downsample
#' @export
#' @examples
#' pupil_downsample(x, bin.length = 100)

pupil_downsample <- function(x, bin.length = NULL, bc = FALSE){
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     TimeBin = trunc(Time/bin.length),
                     TimeBin = ifelse(Time<0, TimeBin - 1, TimeBin),
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