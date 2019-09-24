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
#' @param starttracking.message Message used in SMI experiment to
#'     mark StartTracking inline
#' @param starttracking.match Should the message string be an
#'     "exact" match or a "pattern" match?
#' @param subj.prefix The unique pattern prefix (letter(s)
#'     and/or symbol(s)) that comes before the subject number in the data file
#' @param subj.suffix The unique pattern suffix (letter(s) or
#'     symbol(s)) that comes after the subject number in the data file
#' @param timing.file File location and name that contains timing
#'     information for message markers
#' @param subset Which columns in the raw data export file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @export
#'

pupil_read <- function(file, eyetracker = "",
                       starttracking.message = "default",
                       starttracking.match = "exact",
                       subj.prefix = NULL, subj.suffix = NULL,
                       timing.file = NULL, subset = "default",
                       trial.exclude = c()){

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
    if (starttracking.message == "default"){
      starttracking.message <- "StartTracking.bmp"
    }
  }

  if (eyetracker == "eyelink") {
    if (starttracking.message == "default"){
      starttracking.message <- "TRIALID"
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
    } else {
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
                                                    NA, R_Event_Info),
                              ms_conversion = 1000)
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
                                Gaze.quality, ms_conversion)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                L_Pupil_Diameter.mm,
                                L_Event, R_Pupil_Diameter.mm, R_Event, ms_conversion)
        }
      } else if (left.recorded == TRUE){
        data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                              Message = ifelse(get(message.column) >= 0,
                                               NA,get(message.column)),
                              L_Event_Info = ifelse((L_Event_Info == "-" |
                                                       is.na(L_Event_Info)),
                                                    NA, L_Event_Info),
                              ms_conversion = 1000)
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
                                Gaze.quality, ms_conversion)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm,
                                Event, ms_conversion)
        }

      } else if (right.recorded == TRUE){
        data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                              Message = ifelse(get(message.column) >= 0,
                                               NA,get(message.column)),
                              R_Event_Info = ifelse((R_Event_Info == "-" |
                                                       is.na(R_Event_Info)),
                                                    NA, R_Event_Info),
                              ms_conversion = 1000)
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
                                Gaze.quality, ms_conversion)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm,
                                Event, ms_conversion)
        }
      }
    }

    if (model == "glasses") {
      data <- dplyr::mutate(data,
                            Subject = Participant,
                            Event =
                              dplyr::case_when(Category_Binocular ==
                                                 "Annotation Interval Start"
                                               ~ "-",
                                               Category_Binocular ==
                                                 "Annotation Interval End"
                                               ~ "-",
                                               Category_Binocular ==
                                                 "Visual Intake" ~ "Fixation",
                                                     TRUE ~ Category_Binocular),
                            L_Event = Event,
                            R_Event = Event,
                            Message = ifelse(Annotation_Name == "-", NA,
                                             Annotation_Name),
                            Time = `RecordingTime_[ms]`,
                            Time = zoo::na.locf(Time, na.rm = FALSE,
                                                fromLast = TRUE),
                            L_Pupil_Diameter.mm =
                              ifelse(Pupil_Diameter_Left_mm == "-", NA,
                                     Pupil_Diameter_Left_mm),
                            R_Pupil_Diameter.mm =
                              ifelse(Pupil_Diameter_Right_mm == "-", NA,
                                     Pupil_Diameter_Right_mm),
                            L_Pupil_Diameter.mm = as.numeric(L_Pupil_Diameter.mm),
                            R_Pupil_Diameter.mm = as.numeric(R_Pupil_Diameter.mm),
                            L_Gaze_Position.x = Point_of_Regard_Binocular_X_px,
                            R_Gaze_Position.x = Point_of_Regard_Binocular_X_px,
                            L_Gaze_Position.y = Point_of_Regard_Binocular_Y_px,
                            R_Gaze_Position.y = Point_of_Regard_Binocular_Y_px,
                            ms_conversion = 1)
      data <- dplyr::select(data,
                            Subject = Participant, Time,
                            Trial, Message, L_Pupil_Diameter.mm,
                            L_Event, R_Pupil_Diameter.mm, R_Event,
                            L_Gaze_Position.x, L_Gaze_Position.y,
                            R_Gaze_Position.x, R_Gaze_Position.y,
                            ms_conversion)
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
                                                    ifelse(!is.na(RIGHT_FIX_INDEX),"Fixation",NA))),
                            ms_conversion = 1)
      data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE, Pupil_Diameter.mm = RIGHT_PUPIL_SIZE)
      data <- dplyr::select(data, Subject, Time, Trial, subset, Message, Pupil_Diameter.mm, Event)
    }
  }

  data$Message <- gsub("# Message: ", "", data$Message)

  ## Set Trial at starttracking.message
  if (is.null(timing.file)) {
    timing_data <- data.frame()
  } else if (stringr::str_detect(timing.file, "csv")) {
    timing_data <- readr::read_csv(timing.file)
  }
  if ("Subject" %in% colnames(timing_data)) {
    subj <- data$Subject[1]
    timing_data <- dplyr::filter(timing_data, Subject == subj)
  }
  if (ncol(timing_data) > 0) {
    starttrack_timing <- dplyr::mutate(timing_data,
                                       Time = get(starttracking.message),
                                       Message = starttracking.message)
    starttrack_timing <- dplyr::select(starttrack_timing, Trial, Time, Message)
    data <- dplyr::full_join(data, starttrack_timing, by = "Trial")
    data <- dplyr::mutate(data, Time = dplyr::coalesce(Time.x, Time.y))
    data <- dplyr::select(data, -Time.x, -Time.y)
    data <- dplyr::arrange(data, Subject, Trial, Time)
  }
  if (starttracking.match == "exact"){
    data <- dplyr::mutate(data,
                          starttracking.time =
                            ifelse(Message == starttracking.message, Time, NA))
  } else if (starttracking.match == "pattern"){
    data <- dplyr::mutate(data,
                          starttracking.time =
                            ifelse(stringr::str_detect(Message,
                                                       starttracking.message),
                                   Time, NA))
  }
  data <- dplyr::mutate(data,
                        starttracking.time = zoo::na.locf(starttracking.time,
                                                           na.rm = FALSE),
                        Trial = dplyr::dense_rank(starttracking.time))

  if (ncol(timing_data) > 0) {
    ## Set other trial markers based on timing.file
    message_markers <- stringr::str_subset(colnames(timing_data), "Subject",
                                           negate=TRUE)
    message_markers <- stringr::str_subset(message_markers,
                                           "Trial", negate = TRUE)
    message_markers <- stringr::str_subset(message_markers,
                                           starttracking.message, negate = TRUE)
    for (message in message_markers) {
      message_start <- dplyr::filter(data, Message == starttracking.message)
      message_start <- dplyr::select(message_start, Trial, Time,
                                     Message, starttracking.time)
      message_start <- merge(timing_data, message_start, by = "Trial")
      message_start <- dplyr::mutate(message_start,
                                     check = ifelse(get(message) - starttracking.time > 0,
                                                    "abs", "rel"),
                                     Time = ifelse(check == "abs",
                                                   get(message),
                                                   Time + get(message)),
                                     Message = message)
      message_start <- dplyr::select(message_start, Trial, Time, Message)
      data <- dplyr::full_join(data, message_start, by = "Trial")
      data <- dplyr::mutate(data, Time = dplyr::coalesce(Time.x, Time.y))
      data <- dplyr::select(data, -Time.x, -Time.y)
      data <- dplyr::arrange(data, Subject, Trial, Time)
    }
  }

  data <- dplyr::select(data, -starttracking.time)
  data <- dplyr::mutate(data,
                        Subject = zoo::na.locf(Subject, na.rm = FALSE),
                        Trial = zoo::na.locf(Trial, na.rm = FALSE))

  if (!is.null(trial.exclude)){
    data <- dplyr::filter(data, !(Trial %in% trial.exclude))
  }
  return(data)
}
