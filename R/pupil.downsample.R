#' Pupil Preprocessing Function
#'
#' This function will reduce the sampling fequency
#' @param x dataframe
#' @param bin.length Length of bins to average
#' @keywords downsample
#' @export
#' @examples
#' pupil.downsample(x, bin.length = 100)

pupil.downsample <- function(x, bin.length = ""){
  x <- dplyr::group_by(x, Subject, Trial)
  x <- dplyr::mutate(x, TimeBin = ifelse(min(Time)>=0, Time - min(Time), Time),
                     TimeBin = trunc((Time - min(Time))/bin.length))
  x <- dplyr::group_by(x, Subject, Trial, TimeBin)
  x <- dplyr::mutate(x, Pupil_Diameter.mm = mean(Pupil_Diameter.mm, na.rm = TRUE))
  x <- dplyr::ungroup(x)
  return(x)
}
