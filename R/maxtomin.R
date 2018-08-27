#' Pupil DV Calculation
#'
#' This function returns the difference between max and min values
#' @param x vector
#' @keywords max
#' @export
#' @examples
#' maxtomin(x)

maxtomin <- function(x){
  max(x, na.rom = TRUE) - min(x, na.rm = TRUE)
}
