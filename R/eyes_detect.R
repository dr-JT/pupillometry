#' Detect Eyes
#'
#' Detect which eyes are in the data
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' This function returns the number of eyes recorded in the data
#'
#' @param x dataframe
#' @export
#'


eyes_detect <- function(x) {

  mm_conversion <- colnames(x)[which(stringr::str_detect(colnames(x), ".mm"))]
  mm_conversion <- !identical(mm_conversion, character(0))
  eyes <- colnames(x)[which(stringr::str_detect(colnames(x), "_Diameter"))]
  if (mm_conversion == TRUE) {
    eyes <- eyes[which(stringr::str_detect(eyes), ".mm")]
  }

  return(eyes)
}
