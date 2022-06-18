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

  x <- dtplyr::lazy_dt(x)

  mm_conversion <- x[["vars"]][which(stringr::str_detect(x[["vars"]], ".mm"))]
  mm_conversion <- !identical(mm_conversion, character(0))
  eyes <- x[["vars"]][which(stringr::str_detect(x[["vars"]], "_Diameter"))]
  eyes <- eyes[which(!stringr::str_detect(eyes, "bc"))]
  if (mm_conversion == TRUE) {
    eyes <- eyes[which(stringr::str_detect(eyes, ".mm"))]
  }

  return(eyes)
}
