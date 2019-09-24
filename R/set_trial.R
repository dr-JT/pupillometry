#' Set correct trial indexing
#'
#' This sets the trials relative to a message string in the data
#' @param x dataframe
#' @param startrecording.start Message string that marks the start of a recorind
#' @param match Should the message string be an "exact" match or a "pattern" match?
#' @keywords set timing
#' @export
#' @examples
#' set_trial(x, start.trial = "Fixation", ms.conversion = 1000)

set_trial <- function(x, startrecording.start = "", match = "exact"){
  if (stringr::str_detect(timing.file, "csv")) {
    timing_data <- read_csv(timing.file)
  }

  if (match=="exact"){
    x <- dplyr::mutate(x, startrecording.time = ifelse(Message==startrecording.start, Time, NA))
  } else if (match=="pattern"){
    x <- dplyr::mutate(x, startrecording.time = ifelse(stringr::str_detect(Message, startrecording.start), Time, NA))
  }
  x <- dplyr::mutate(x,
                     startrecording.time = zoo::na.locf(startrecording.time, na.rm = FALSE),
                     Trial = dplyr::dense_rank(startrecording.time))
  x <- dplyr::select(x, -startrecording.time)
  return(x)
}
