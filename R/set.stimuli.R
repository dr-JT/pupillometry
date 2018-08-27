#' Indexing Function
#'
#' This function will create a column that indexes the stimuli within each trial
#' @param x dataframe
#' @keywords set stimuli
#' @export
#' @examples
#' set.stimuli(x)

set.stimuli <- function(x){
  data <- data.frame()
  for (trial in unique(x$Trial)){
    data_trial <- dplyr::filter(x, Trial==trial)
    stim <- unique(data_trial$Message[which(!is.na(data_trial$Message))])
    stim.time <- list()
    data_trial <- dplyr::mutate(data_trial, Stimulus = NA)
    for (i in 1:length(stim)){
      stim.time[[i]] <- match(stim[i], data_trial$Message)
    }
    for (i in 1:length(stim)){
      if (i==length(stim)){
        data_trial <- dplyr::mutate(data_trial, Stimulus = ifelse(row_number()>=stim.time[[i]] & row_number()<=length(data_trial$Message), stim[i], Stimulus))
      } else {
        data_trial <- dplyr::mutate(data_trial, Stimulus = ifelse(row_number()>=stim.time[[i]] & row_number()<stim.time[[i+1]], stim[i], Stimulus))
      }
    }
    data <- rbind(data, data.frame(data_trial))
  }
  return(data)
}

