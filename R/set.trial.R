#' Pupil Data Function
#'
#' This sets the trials relative to a message string in the data
#' @param x dataframe
#' @param startrecroding.message Message string that marks the start of a recorind
#' @keywords set timing
#' @export
#' @examples
#' set.trial(x, start.trial = "Fixation", ms.conversion = 1000)

set.trial <- function(x, startrecroding.message = ""){
  x <- dplyr::mutate(x,
                     startrecording.time = ifelse(Message==startrecroding.message, Time, NA),
                     startrecording.time = ifelse(startrecording.time!=min(startrecording.time, na.rm = TRUE), NA, startrecording.time),
                     startrecording.time = zoo::na.locf(startrecording.time),
                     Trial = dplyr::dense_rank(startrecording.time))
  return(x)
}
