#' Downsample pupil data
#'
#' This function will reduce the sampling fequency
#' @param x dataframe
#' @param bin.length Length of bins to average
#' @param id Column name that identifies subject number
#' @keywords downsample
#' @export
#' @examples
#' pupil_downsample(x, bin.length = 100)

pupil_downsample <- function(x, bin.length = NULL, id = "Subjet"){
  colnames(x)[which(colnames(x)==id)] <- "Subject"
  x <- dplyr::group_by(x, Subject, Trial, add = TRUE)
  x <- dplyr::mutate(x,
                     TimeBin = trunc(Time/bin.length),
                     TimeBin = ifelse(Time<0, TimeBin - 1, TimeBin),
                     Time = TimeBin*bin.length)
  x <- dplyr::group_by(x, TimeBin, add = TRUE)
  x <- dplyr::mutate(x, Pupil_Diameter.mm = mean(Pupil_Diameter.mm, na.rm = TRUE))
  bc <- "Pupil_Diameter_bc.mm" %in% colnames(x)
  if (bc == TRUE){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = mean(Pupil_Diameter_bc.mm, na.rm = TRUE))
  }
  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -TimeBin)
  x <- dplyr::distinct(x, Subject, Trial, Time, .keep_all = TRUE)
  colnames(x)[which(colnames(x)=="Subject")] <- id
  return(x)
}
