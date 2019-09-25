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

  if ("Message_Inserted" %in% colnames(x)) {
    x <- dplyr::filter(x, Message_Inserted == 0)
    x <- dplyr::select(x, -Message_Inserted)
  }
  return(x)
}
