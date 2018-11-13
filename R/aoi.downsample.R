#' Downsample aoi data
#'
#' This function will reduce the sampling fequency
#' @title aoi.downsample
#' @param x dataframe
#' @param bin.length Length of bins to average
#' @param aoi_col Column names that contain aoi measures
#'
#' @templateVar fun aoi.downsample
#' @template template-depr_fun
NULL

#' @templateVar old aoi.downsample
#' @templateVar new aoi_downsample
#' @template template-depr_pkg
#'
#' @keywords downsample
#' @export
#' @examples
#' aoi.downsample(x, bin.length = 100)

aoi.downsample <- function(x, bin.length = NULL, aoi_col = c()){
  .Deprecated("aoi_downsample")
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
