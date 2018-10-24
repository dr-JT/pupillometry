#' Downsample pupil data
#'
#' This function will reduce the sampling fequency
#' @param x dataframe
#' @param bin.length Length of bins to average
#' @param bc Logical. Was baseline correction done on the data? Is there a column labeled Pupil_Diameter_bc.mm?
#' @param id Column that identifies Subjects
#' @param trial.col Trial column name
#' @param time.col Time column name
#' @param pupil.col Trial containing pupil data to be downsampled
#' @keywords downsample
#' @export
#' @examples
#' pupil.downsample(x, bin.length = 100)

pupil.downsample <- function(x, bin.length = NULL, bc = FALSE, id = "Subject", trial.col = "Trial",
                             time.col = "Time", pupil.col = "Pupil_Diameter.mm"){
  colnames(x)[which(colnames(x)==id)] <- "Subject"
  colnames(x)[which(colnames(x)==trial.col)] <- "Trial"
  colnames(x)[which(colnames(x)==time.col)] <- "Time"
  colnames(x)[which(colnames(x)==pupil.col)] <- "Pupil"
  x <- dplyr::group_by(x, Subject, Trial)
  x <- dplyr::mutate(x,
                     TimeBin = trunc(Time/bin.length),
                     TimeBin = ifelse(Time<0, TimeBin - 1, TimeBin),
                     Time = TimeBin*bin.length)
  x <- dplyr::group_by(x, Subject, Trial, TimeBin)
  x <- dplyr::mutate(x, Pupil = mean(Pupil, na.rm = TRUE))
  if (bc==TRUE){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = mean(Pupil_Diameter_bc.mm, na.rm = TRUE))
  }
  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -TimeBin, -Message)
  x <- dplyr::distinct(x, Trial, Time, .keep_all = TRUE)
  colnames(x)[which(colnames(x)=="Subject")] <- id
  colnames(x)[which(colnames(x)=="Trial")] <- trial.col
  colnames(x)[which(colnames(x)=="Time")] <- time.col
  colnames(x)[which(colnames(x)=="Pupil")] <- pupil.col
  return(x)
}
