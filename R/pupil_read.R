#' Convert "messy" raw data file to "tidy" raw data file
#'
#' This function converts a "messy" raw eye-tracker data file
#' to a "tidy" raw data file.
#'
#' Different eye-trackers will save the raw data file using
#' completely different organizations and naming conventions
#' This function will create a "tidy" raw data file that has
#' a standard organization and naming convention,
#' regardless of which eye-tracker was used.
#' Supported Eye-Tracker Systems: Sensomotoric Instruments ("smi")
#' and SR-Research EyeLink ("eyelink")
#' @param file A file path to the raw data export file
#' @param eyetracker Which eye-tracker system was used to record data?
#' @param startrecording.message Message used in SMI experiment to
#'     mark StartTracking inline
#' @param startrecording.match Should the message string be an
#'     "exact" match or a "pattern" match?
#' @param subj.prefix The unique pattern prefix (letter(s)
#'     and/or symbol(s)) that comes before the subject number in the data file
#' @param subj.suffix The unique pattern suffix (letter(s) or
#'     symbol(s)) that comes after the subject number in the data file
#' @param subset Which columns in the raw data export file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @export

pupil_read <- function(file, eyetracker = "",
                       startrecording.message = "default",
                       startrecording.match = "exact",
                       subj.prefix = NULL, subj.suffix = NULL,
                       subset = "default", trial.exclude = c()){

  subj.extract <- function(x, prefix, suffix){
    x <- stringr::str_split(x, "/")[[1]]
    x <- x[length(x)]
    if (!is.null(prefix)){
      if (!is.null(suffix)){
        pattern.prefix <- paste(prefix, "(?=\\d)", sep = "")
        pattern.suffix <- paste("(?<=\\d)", suffix, sep = "")

        subj <- stringr::str_split(x, pattern.prefix)[[1]][2]
        subj <- stringr::str_split(subj, pattern.suffix)[[1]][1]
        subj <- as.numeric(subj)
      } else {
        pattern.prefix <- paste(prefix, "(?=\\d)", sep = "")

        subj <- stringr::str_split(x, pattern.prefix)[[1]][2]
        subj <- as.numeric(subj)
      }
    } else if (!is.null(suffix)){
      pattern.suffix <- paste("(?<=\\d)", suffix, sep = "")

      subj <- stringr::str_split(x, pattern.suffix)[[1]][1]
      subj <- as.numeric(subj)
    } else {
      subj <- x
    }
    return(subj)
  }

  if (eyetracker == "smi") {
    if (startrecording.message == "default"){
      startrecording.message <- "StartTracking.bmp"
    }
  }

  if (eyetracker == "eyelink") {
    if (startrecording.message == "default"){
      startrecording.message <- "TRIALID"
    }
    if (subset == "default"){
      subset <- "Time"
    }
  }

  if (eyetracker == "smi") {

    header <- readr::read_table(file, col_names = FALSE)
    if (ncol(header) == 1) {
      samples.total <- as.numeric(strsplit(header$X1[10], "\t")[[1]][2])
      subj <- subj.extract(file, prefix = subj.prefix, suffix = subj.suffix)
      head.distance <- as.numeric(strsplit(header$X1[24], "\t")[[1]][2])/10

      found <- NA
      checkrow <- 0
      while (is.na(found)){
        checkrow <- checkrow + 1
        found <- match("Time", strsplit(header[checkrow,][[1]], "\t")[[1]][1])
        datastart <- checkrow
      }

      data <- readr::read_delim(file, "\t", escape_double = FALSE,
                                trim_ws = TRUE, skip = datastart-1,
                                guess_max = 100000)
    } else {
      data <- readr::read_delim(file, "\t", escape_double = FALSE,
                                trim_ws = TRUE, guess_max = 100000)
    }

    names(data) <- gsub(" ", "_", gsub("\\[mm\\]", "mm",
                                       gsub("\\[px\\]", "px", names(data))))

    if ("Time" %in% colnames(data)) {
      model <- "Red250m"
    } else if ("Video_Time_[h:m:s:ms]" %in% colnames(data)) {
      model <- "glasses"
    }

    if (model == "Red250m") {
      message.column <- names(data[4])
      left.recorded <- "L_Pupil_Diameter_mm" %in% colnames(data)
      right.recorded <- "R_Pupil_Diameter_mm" %in% colnames(data)
      left.gaze <- "L_POR_X_px" %in% colnames(data)
      right.gaze <- "R_POR_X_px" %in% colnames(data)
      if (left.recorded == TRUE & right.recorded == TRUE){
        data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                              Message = ifelse(get(message.column) >= 0,
                                               NA,get(message.column)),
                              L_Event_Info = ifelse((L_Event_Info == "-" |
                                                       is.na(L_Event_Info)),
                                                    NA, L_Event_Info),
                              R_Event_Info = ifelse((R_Event_Info == "-" |
                                                       is.na(R_Event_Info)),
                                                    NA, R_Event_Info))
        data <- dplyr::rename(data,
                              L_Pupil_Diameter.mm = L_Pupil_Diameter_mm,
                              L_Event = L_Event_Info,
                              R_Pupil_Diameter.mm = R_Pupil_Diameter_mm,
                              R_Event = R_Event_Info)

        if (left.gaze == TRUE & right.gaze == TRUE){
          data <- dplyr::rename(data,
                                L_Gaze_Position.x = L_POR_X_px,
                                L_Gaze_Position.y = L_POR_Y_px,
                                R_Gaze_Position.x = R_POR_X_px,
                                R_Gaze_Position.y = R_POR_Y_px,
                                Gaze.quality = Timing)
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                L_Pupil_Diameter.mm,
                                L_Event, R_Pupil_Diameter.mm, R_Event,
                                L_Gaze_Position.x, L_Gaze_Position.y,
                                R_Gaze_Position.x, R_Gaze_Position.y,
                                Gaze.quality)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                L_Pupil_Diameter.mm,
                                L_Event, R_Pupil_Diameter.mm, R_Event)
        }
      } else if (left.recorded == TRUE){
        data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                              Message = ifelse(get(message.column) >= 0,
                                               NA,get(message.column)),
                              L_Event_Info = ifelse((L_Event_Info == "-" |
                                                       is.na(L_Event_Info)),
                                                    NA, L_Event_Info))
        data <- dplyr::rename(data,
                              Pupil_Diameter.mm = L_Pupil_Diameter_mm,
                              Event = L_Event_Info)
        if (left.gaze == TRUE){
          data <- dplyr::rename(data,
                                Gaze_Position.x = L_POR_X_px,
                                Gaze_Position.y = L_POR_Y_px,
                                Gaze.quality = Timing)
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm,
                                Event, Gaze_Position.x, Gaze_Position.y,
                                Gaze.quality)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm,
                                Event)
        }

      } else if (right.recorded == TRUE){
        data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                              Message = ifelse(get(message.column) >= 0,
                                               NA,get(message.column)),
                              R_Event_Info = ifelse((R_Event_Info == "-" |
                                                       is.na(R_Event_Info)),
                                                    NA, R_Event_Info))
        data <- dplyr::rename(data,
                              Pupil_Diameter.mm = R_Pupil_Diameter_mm,
                              Event = R_Event_Info)
        if (right.gaze == TRUE){
          data <- dplyr::rename(data,
                                Gaze_Position.x = R_POR_X_px,
                                Gaze_Position.y = R_POR_Y_px,
                                Gaze.quality = Timing)
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm,
                                Event, Gaze_Position.x, Gaze_Position.y,
                                Gaze.quality)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm,
                                Event)
        }
      }
    }

    if (model == "glasses") {
      data <- dplyr::mutate(data,
                            Subject = Participant,
                            Event = dplyr::case_when(Category_Binocular ==
                                                       "Annotation Interval Start" ~ "-",
                                                     Category_Binocular ==
                                                       "Annotation Interval End" ~ "-",
                                                     Category_Binocular ==
                                                       "Visual Intake" ~ "Fixation",
                                                     TRUE ~ Category_Binocular),
                            L_Event = Event,
                            R_Event = Event,
                            Message = ifelse(Annotation_Name == "-", NA, Annotation_Name),
                            L_Pupil_Diameter.mm =
                              as.numeric(stringr::str_replace(Pupil_Diameter_Left_mm,
                                                              ",", ".")),
                            R_Pupil_Diameter.mm =
                              as.numeric(stringr::str_replace(Pupil_Diameter_Right_mm,
                                                              ",", ".")),
                            Gaze_Position.x =
                              as.numeric(stringr::str_replace(Point_of_Regard_Binocular_X_px,
                                                              ",", ".")),
                            Gaze_Position.y =
                              as.numeric(stringr::str_replace(Point_of_Regard_Binocular_Y_px,
                                                              ",", ".")),
                            Time = as.numeric(stringr::str_replace(`RecordingTime_[ms]`,
                                                                   ",", "")),
                            L_Gaze_Position.x = Gaze_Position.x,
                            R_Gaze_Position.x = Gaze_Position.x,
                            L_Gaze_Position.y = Gaze_Position.y,
                            R_Gaze_Position.y = Gaze_Position.y)
      data <- dplyr::select(data,
                            Subject = Participant, Time,
                            Trial, Message, L_Pupil_Diameter.mm,
                            L_Event, R_Pupil_Diameter.mm, R_Event,
                            L_Gaze_Position.x, L_Gaze_Position.y,
                            R_Gaze_Position.x, R_Gaze_Position.y)
    }
  }

  if (eyetracker=="eyelink") {

    data <- readr::read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE, na = ".")
    subj <- subj.extract(file, prefix = subj.prefix, suffix = subj.suffix)

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
  }

  data$Message <- gsub("# Message: ", "", data$Message)

  data <- set_trial(data, startrecording.message = startrecording.message,
                    match = startrecording.match)

  if (!is.null(trial.exclude)){
    data <- dplyr::filter(data, !(Trial %in% trial.exclude))
  }
  return(data)
}
