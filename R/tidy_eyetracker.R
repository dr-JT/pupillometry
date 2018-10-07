#' Messy to Tidy Raw Eye-Tracker Data File
#'
#' This function converts a "messy" raw eye-tracker data file to a "tidy" raw data file.
#' Different eye-trackers will save the raw data file using completely different organizations and naming conventions
#' This function will create a "tidy" raw data file that has a standard organization and naming convention, regardless of which eye-tracker was used.
#' Supported Eye-Tracker Systems: Sensomotoric Instruments ("smi") and SR-Research EyeLink ("eyelink")
#' @param file A file path to the raw data export file
#' @param eyetracker Which eye-tracker system was used to record data?
#' @param startrecording.message Message used in SMI experiment to mark StartTracking inline
#' @param startrecording.match Should the message string be an "exact" match or a "pattern" match?
#' @param eye.recorded Do you want to inclue the "left", "right', or "both" eyes?
#' @param subj.prefix The prefix that comes before the subject number in the data file (including "-")
#' @param subset Which columns in the raw data export file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @keywords tidy
#' @export
#' @examples
#' tidy_eyetracker(file = "path/filename.txt", subset = c(), message.column = "columnName", track.start = "# Message: StartTracking.bmp", eye = "both")

tidy_eyetracker <- function(file, eyetracker = "", startrecording.message = "default", startrecording.match = "exact",
                            eye.recorded = "", subj.prefix = "default", subset = "default", trial.exclude = c()){

  #### ----- SMI ----- ####
  if (eyetracker=="smi") {
    if (startrecording.message=="default"){
      startrecording.message <- "# Message: StartTracking.bmp"
    }
    if (subj.prefix=="default") {
      subj.prefix <- "-"
    }
  }

  if (eyetracker=="eyelink") {
    if (startrecording.message=="default"){
      startrecording.message <- "TRIALID"
    }
    if (subset=="default"){
      subset <- "Time"
    }
  }
  ##################################

  #### ----- SMI ----- ####
  if (eyetracker=="smi") {
    ## Import and grab data from the header ####
    header <- readr::read_table(file, col_names = FALSE)
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
    if (eye.recorded=="both"){
      data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                            Message = ifelse(get(message.column)>=0,NA,get(message.column)),
                            L_Event_Info = ifelse((L_Event_Info=="-"|is.na(L_Event_Info)),NA, L_Event_Info),
                            R_Event_Info = ifelse((R_Event_Info=="-"|is.na(R_Event_Info)),NA, R_Event_Info))
      data <- dplyr::rename(data, L_Pupil_Diameter.mm = L_Pupil_Diameter_mm, L_Event = L_Event_Info,
                            R_Pupil_Diameter.mm = R_Pupil_Diameter_mm, R_Event = R_Event_Info)
      data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message, L_Pupil_Diameter.mm,
                            L_Event, R_Pupil_Diameter.mm, R_Event)
    } else if (eye.recorded=="left"){
      data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                            Message = ifelse(get(message.column)>=0,NA,get(message.column)),
                            L_Event_Info = ifelse((L_Event_Info=="-"|is.na(L_Event_Info)),NA, L_Event_Info))
      data <- dplyr::rename(data, Pupil_Diameter.mm = L_Pupil_Diameter_mm, Event = L_Event_Info)
      data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message, Pupil_Diameter.mm,
                            Event)
    } else if (eye.recorded=="right"){
      data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                            Message = ifelse(get(message.column)>=0,NA,get(message.column)),
                            R_Event_Info = ifelse((R_Event_Info=="-"|is.na(R_Event_Info)),NA, R_Event_Info))
      data <- dplyr::rename(data, Pupil_Diameter.mm = R_Pupil_Diameter_mm, Event = R_Event_Info)
      data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message, Pupil_Diameter.mm,
                            Event)
    }
    ###################
  }
  #########################

  #### ----- EyeLink ----- ####
  if (eyetracker=="eyelink") {
    ## Import and grab subject number ####
    data <- readr::read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE, na = ".")
    subj <- as.numeric(gsub(gsub('[0-9]', "", data$RECORDING_SESSION_LABEL[1]),
                            "", data$RECORDING_SESSION_LABEL[1]))
    ###################
    ## Add subject, Hz, and event info. rename and select subset of data ####
    if (eye.recorded=="both"){
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
    } else if (eye.recorded=="left"){
      data <- dplyr::mutate(data, Subject = subj, Trial = NA,
                            Event = ifelse(LEFT_IN_BLINK==1,"Blink",
                                             ifelse(LEFT_IN_SACCADE==1,"Saccade",
                                                    ifelse(!is.na(LEFT_FIX_INDEX),"Fixation",NA))))
      data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE, Pupil_Diameter.mm = LEFT_PUPIL_SIZE)
      data <- dplyr::select(data, Subject, Time, Trial, subset, Message, Pupil_Diameter.mm, Event)
    } else if (eye.recorded=="right"){
      data <- dplyr::mutate(data, Subject = subj, Trial = NA,
                            Event = ifelse(RIGHT_IN_BLINK==1,"Blink",
                                             ifelse(RIGHT_IN_SACCADE==1,"Saccade",
                                                    ifelse(!is.na(RIGHT_FIX_INDEX),"Fixation",NA))))
      data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE, Pupil_Diameter.mm = RIGHT_PUPIL_SIZE)
      data <- dplyr::select(data, Subject, Time, Trial, subset, Message, Pupil_Diameter.mm, Event)
    }
    ###################
  }
  #############################

  if (!is.null(trial.exclude)){
    data <- dplyr::filter(data, !(Trial %in% trial.exclude))
  }

  ## Correctly set trial index ####
  data <- set.trial(data, startrecording.message = startrecording.message, match = startrecording.match)
  ##################

  ## Remove "# Message: " from message string ####
  data$Message <- gsub("# Message: ", "", data$Message)
  return(data)
}
