#' Downsample aoi data
#'
#' This function will reduce the sampling fequency
#' @param x dataframe
#' @param bin.length Length of bins to average
#' @param aoi_col Column names that contain aoi measures
#' @keywords downsample
#' @export
#' @examples
#' aoi.downsample(x, bin.length = 100)

aoi.downsample <- function(x, bin.length = NULL, aoi_col = c()){
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     TimeBin = trunc(Time/bin.length),
                     TimeBin = ifelse(Time<0, TimeBin - 1, TimeBin),
                     Time = TimeBin*bin.length)
  x <- dplyr::group_by(x, Trial, TimeBin)

  for (aoi in aoi_col){
    colnames(x)[which(colnames(x)==aoi)] <- "placeholder"
    x <- dplyr::mutate(x, placeholder = mean(placeholder, na.rm = TRUE))
    colnames(x)[which(colnames(x)=="placeholder")] <- aoi
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -TimeBin, -Message)
  x <- dplyr::distinct(x, Trial, Time, .keep_all = TRUE)
  return(x)
}
