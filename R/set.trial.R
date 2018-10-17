#' Set correct trial indexing
#'
#' This sets the trials relative to a message string in the data
#' @param x dataframe
#' @param startrecording.message Message string that marks the start of a recorind
#' @param match Should the message string be an "exact" match or a "pattern" match?
#' @keywords set timing
#' @export
#' @examples
#' set.trial(x, start.trial = "Fixation", ms.conversion = 1000)

set.trial <- function(x, startrecording.message = "", match = "exact"){
  if (match=="exact"){
    x <- dplyr::mutate(x, startrecording.time = ifelse(Message==startrecording.message, Time, NA))
  } else if (match=="pattern"){
    x <- dplyr::mutate(x, startrecording.time = ifelse(stringr::str_detect(Message, startrecording.message), Time, NA))
  }
  x <- dplyr::mutate(x,
                     startrecording.time = zoo::na.locf(startrecording.time, na.rm = FALSE),
                     Trial = dplyr::dense_rank(startrecording.time))
  x <- dplyr::select(x, -startrecording.time)
  return(x)
}
