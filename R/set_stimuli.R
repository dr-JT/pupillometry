#' Create a column of current stimulus period
#'
#' This function will create a column that indexes the stimuli within each trial
#' @param x dataframe
#' @keywords set stimuli
#' @export
#' @examples
#' set_stimuli(x)

set_stimuli <- function(x){
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, Stimulus = zoo::na.locf(Message, na.rm = FALSE))
  x <- dplyr::ungroup(x)

  x <- dplyr::filter(x, Message_Inserted == 0)

  col_order <- c("Subject", "Trial", "PreTrial", "Time", "Stimulus",
                 "Pupil_Diameter.mm", "Pupils.r", "Event", "Gaze_Position.x",
                 "Gaze_Position.y", "Gaze.quality", "Head_Dist.cm")

  col_order <- colnames(data)[order(match(colnames(data), col_order))]

  data <- data[,col_order]

  return(x)
}
