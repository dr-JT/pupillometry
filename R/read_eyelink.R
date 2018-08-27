#' Convert SMI BeGaze for Analysis
#'
#' This function takes a raw data export file from the SMI BeGaze software and converts it into a format that is easy to analyze
#' @param file A file path to the raw data export file
#' @param subset Which columns in the raw data export file do you want to keep
#' @param trial.start Message used in SMI experiment to mark StartTracking inline
#' @param eye Do you want to inclue the "left", "right', or "both" eyes?
#' @keywords read eyelink
#' @export
#' @examples
#' read_eyelink(file = "path/filename.txt", subset = c(), track.start = "# Message: StartTracking.bmp", eye = "both", Hz = 1000)

read_eyelink <- function(file = "", subset = "Time", trial.start = "TRIALID", eye = "both"){

  ## Import and grab subject number ####
  data <- readr::read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE, na = ".")
  subj <- as.numeric(gsub(gsub('[0-9]', "", data$RECORDING_SESSION_LABEL[1]),
                          "", data$RECORDING_SESSION_LABEL[1]))
  ###################
  ## Add subject, Hz, and event info. rename and select subset of data ####
  if (eye=="both"){
    data <- dplyr::mutate(data, Subject = subj, Trial = NA,
                          L_Event = ifelse(LEFT_IN_BLINK==1,"Blink",
                                           ifelse(LEFT_IN_SACCADE==1,"Saccade",
                                                  ifelse(!is.na(LEFT_FIX_INDEX),"Fixation",NA))),
                          R_Event = ifelse(RIGHT_IN_BLINK==1,"Blink",
                                           ifelse(RIGHT_IN_SACCADE==1,"Saccade",
                                                  ifelse(!is.na(RIGHT_FIX_INDEX),"Fixation",NA))))
    data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE, L_Pupil_Diameter.mm = LEFT_PUPIL_SIZE,
                          R_Pupil_Diameter.mm = RIGHT_PUPIL_SIZE)
    data <- dplyr::select(data, Subject, Time, Trial, subset, Message, L_Pupil_Diameter.mm,
                          L_Event, R_Pupil_Diameter.mm, R_Event)
  } else if (eye=="left"){
    data <- dplyr::mutate(data, Subject = subj, Trial = NA,
                          L_Event = ifelse(LEFT_IN_BLINK==1,"Blink",
                                           ifelse(LEFT_IN_SACCADE==1,"Saccade",
                                                  ifelse(!is.na(LEFT_FIX_INDEX),"Fixation",NA))))
    data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE, L_Pupil_Diameter.mm = LEFT_PUPIL_SIZE)
    data <- dplyr::select(data, Subject, Time, Trial, subset, Message, L_Pupil_Diameter.mm, L_Event)
  } else if (eye=="right"){
    data <- dplyr::mutate(data, Subject = subj, Trial = NA,
                          R_Event = ifelse(RIGHT_IN_BLINK==1,"Blink",
                                           ifelse(RIGHT_IN_SACCADE==1,"Saccade",
                                                  ifelse(!is.na(RIGHT_FIX_INDEX),"Fixation",NA))))
    data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE, R_Pupil_Diameter.mm = RIGHT_PUPIL_SIZE)
    data <- dplyr::select(data, Subject, Time, Trial, subset, Message, R_Pupil_Diameter.mm, R_Event)
  }
  ###################
  ## Correctly set trial index ####
  onsetTimes <- grep(trial.start, data$Message)
  data <- dplyr::mutate(data, Trial = NA)
  for (i in 1:length(onsetTimes)){
    if (i==length(onsetTimes)){
      data <- dplyr::mutate(data, Trial = ifelse(row_number()>=(onsetTimes[i]) & row_number()<=length(data$Message),i,Trial))
    } else {
      data <- dplyr::mutate(data, Trial = ifelse(row_number()>=(onsetTimes[i]) & row_number()<onsetTimes[i+1],i,Trial))
    }
  }
  ###################
  return(data)
}
