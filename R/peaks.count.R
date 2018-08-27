#' Pupil DV Calculation
#'
#' This function returns the number of max pupil dilation values
#' @param x vector
#' @keywords peaks
#' @export
#' @examples
#' peaks.count(x)

peaks.count <- function(x){
  length(which(x %in% max(x, na.rm = TRUE)))
}
