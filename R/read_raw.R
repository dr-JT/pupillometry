#' Messy to Tidy Raw Eye-Tracker Data File
#'
#' This function converts a "messy" raw eye-tracker data file to a "tidy" raw data file.
#' Different eye-trackers will save the raw data file using completely different organizations and naming conventions
#' This function will create a "tidy" raw data file that has a standard organization and naming convention, regardless of which eye-tracker was used.
#' Supported Eye-Tracker Systems: Sensomotoric Instruments ("smi") and SR-Research EyeLink ("eyelink")
#' @param file A file path to the raw data export file
#' @param eyetracker Which eye-tracker system was used to record data?
#' @param trial.start Message used in SMI experiment to mark StartTracking inline
#' @param eye Do you want to inclue the "left", "right', or "both" eyes?
#' @param subj.prefix The prefix that comes before the subject number in the data file (including "-")
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @keywords read smi
#' @export
#' @examples
#' read_smi(file = "path/filename.txt", subset = c(), message.column = "columnName", track.start = "# Message: StartTracking.bmp", eye = "both")

read_smi <- function(file, eyetracker = "", trial.start = "", eye = "", subj.prefix = "", trial.exclude = c()){
  ## Import and grab data from the header ####
  header <- readr::read_table(file, col_names = FALSE)
  Hz <- as.numeric(strsplit(header$X1[6], "\t")[[1]][2])
  samples.total <- as.numeric(strsplit(header$X1[10], "\t")[[1]][2])
  subj <- as.numeric(strsplit(strsplit(gsub(gsub("-$", "",
                                                 gsub('[0-9]', "",
                                                      strsplit(header$X1[13], "\t")[[1]][2])), "",
                                            strsplit(header$X1[13], "\t")[[1]][2]), subj.prefix)[[1]][1], "-")[[1]][1])
  head.distance <- as.numeric(strsplit(header$X1[24], "\t")[[1]][2])/10
  ###################
  ## Import data and replace [ ] in column names ####
  # Find where data starts
  found <- NA
  checkrow <- 0
  while (is.na(found)){
    checkrow <- checkrow + 1
    found <- match("Time", strsplit(header[checkrow,][[1]], "\t")[[1]][1])
    datastart <- checkrow
  }
  data <- readr::read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE, skip = datastart-1)
  names(data) <- gsub(" ", "_", gsub("\\[mm\\]", "mm", gsub("\\[px\\]", "px", names(data))))
  message.column <- names(data[4])
  ###################
  ## Add info from header, rename, set missing values and select subset of data ####
  if (eye=="both"){
    data <- dplyr::mutate(data, Subject = subj, Hz = Hz, Head_Dist.cm = head.distance,
                          Message = ifelse(get(message.column)>=0,NA,get(message.column)),
                          L_Event_Info = ifelse((L_Event_Info=="-"|is.na(L_Event_Info)),NA, L_Event_Info),
                          R_Event_Info = ifelse((R_Event_Info=="-"|is.na(R_Event_Info)),NA, R_Event_Info))
    data <- dplyr::rename(data, L_Pupil_Diameter.mm = L_Pupil_Diameter_mm, L_Event = L_Event_Info,
                          R_Pupil_Diameter.mm = R_Pupil_Diameter_mm, R_Event = R_Event_Info)
    data <- dplyr::select(data, Subject, Hz, Head_Dist.cm, Time, Trial, Message, L_Pupil_Diameter.mm,
                          L_Event, R_Pupil_Diameter.mm, R_Event)
  } else if (eye=="left"){
    data <- dplyr::mutate(data, Subject = subj, Hz = Hz, Head_Dist.cm = head.distance,
                          Message = ifelse(get(message.column)>=0,NA,get(message.column)),
                          L_Event_Info = ifelse((L_Event_Info=="-"|is.na(L_Event_Info)),NA, L_Event_Info))
    data <- dplyr::rename(data, L_Pupil_Diameter.mm = L_Pupil_Diameter_mm, L_Event = L_Event_Info)
    data <- dplyr::select(data, Subject, Hz, Head_Dist.cm, Time, Trial, Message, L_Pupil_Diameter.mm,
                          L_Event)
  } else if (eye=="right"){
    data <- dplyr::mutate(data, Subject = subj, Hz = Hz, Head_Dist.cm = head.distance,
                          Message = ifelse(get(message.column)>=0,NA,get(message.column)),
                          R_Event_Info = ifelse((R_Event_Info=="-"|is.na(R_Event_Info)),NA, R_Event_Info))
    data <- dplyr::rename(data, R_Pupil_Diameter.mm = R_Pupil_Diameter_mm, R_Event = R_Event_Info)
    data <- dplyr::select(data, Subject, Hz, Head_Dist.cm, Time, Trial, Message, R_Pupil_Diameter.mm,
                          R_Event)
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

  if (!is.null(trial.exclude)){
    data <- dplyr::filter(data, !(Trial %in% trial.exclude))
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
  }
  ###################
  ## Remove "# Message: " from message string ####
  data$Message <- gsub("# Message: ", "", data$Message)
  ###################
  return(data)
}
