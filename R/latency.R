#' Pupil DV Calculation
#'
#' This function returns the latency of max pupil dilation
#' @param x vector
#' @keywords latency
#' @export
#' @examples
#' latency(x)

latency <- function(x){
  match(max(x,na.rm = TRUE), x)
}
