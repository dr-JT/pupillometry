#' Pupil DV Calculation
#'
#' This function returns the distance between max pupil dilation values
#' @param x vector
#' @keywords peaks
#' @export
#' @examples
#' peaks.dist(x)

peaks.dist <- function(x){
  which(x %in% max(x, na.rm = TRUE))[length(which(x %in% max(x, na.rm = TRUE)))] - which(x %in% max(x, na.rm = TRUE))[1]
}
