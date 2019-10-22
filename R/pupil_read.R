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
#' @param start_tracking.message Message used in SMI experiment to
#'     mark StartTracking inline
#' @param start_tracking.match Should the message string be an
#'     "exact" match or a "pattern" match?
#' @param subj.prefix The unique pattern prefix (letter(s)
#'     and/or symbol(s)) that comes before the subject number in the data file
#' @param subj.suffix The unique pattern suffix (letter(s) or
#'     symbol(s)) that comes after the subject number in the data file
#' @param px_to_mm.conversion The conversion factor to go from
#'     px pupil diameter to mm pupil diameter
#' @param starttracking.message See start_tracking.message
#' @param starttracking.match See start_tracking.match
#' @param timing.file File location and name that contains timing
#'     information for message markers
#' @param subset Which columns in the raw data export file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @export
#'

pupil_read <- function(file, eyetracker = "", px_to_mm.conversion = NULL,
                       start_tracking.message = "default",
                       start_tracking.match = "exact", subj.prefix = NULL,
                       subj.suffix = NULL, timing.file = NULL,
                       subset = "default", trial.exclude = c(),
                       starttracking.message = NULL,
                       starttracking.match = NULL){
  if (!is.null(starttracking.message)) {
    start_tracking.message <- starttracking.message
  }
  if (!is.null(starttracking.match)) {
    start_tracking.match <- starttracking.match
  }

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
    if (start_tracking.message == "default"){
      start_tracking.message <- "StartTracking.bmp"
    }
  }

  if (eyetracker == "eyelink") {
    if (start_tracking.message == "default"){
      start_tracking.message <- "TRIALID"
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
      data <- dplyr::mutate(data, Subject = subj, Head_Dist.cm = head.distance,
                            Message = ifelse(get(message.column) >= 0,
                                             NA, get(message.column)),
                            ms_conversion = 1000)
      left.recorded <- "L_Pupil_Diameter_mm" %in% colnames(data)
      right.recorded <- "R_Pupil_Diameter_mm" %in% colnames(data)
      left.gaze <- "L_POR_X_px" %in% colnames(data)
      right.gaze <- "R_POR_X_px" %in% colnames(data)
      if (left.recorded == TRUE & right.recorded == TRUE){
        data <- dplyr::mutate(data,
                              L_Event = ifelse((L_Event_Info == "-" |
                                                  is.na(L_Event_Info)),
                                               NA, L_Event_Info),
                              R_Event = ifelse((R_Event_Info == "-" |
                                                  is.na(R_Event_Info)),
                                               NA, R_Event_Info))
        data <- dplyr::rename(data,
                              L_Pupil_Diameter.mm = L_Pupil_Diameter_mm,
                              R_Pupil_Diameter.mm = R_Pupil_Diameter_mm)

        if (left.gaze == TRUE & right.gaze == TRUE){
          data <- dplyr::rename(data,
                                L_Gaze_Position.x = L_POR_X_px,
                                L_Gaze_Position.y = L_POR_Y_px,
                                R_Gaze_Position.x = R_POR_X_px,
                                R_Gaze_Position.y = R_POR_Y_px,
                                Gaze.quality = Timing)
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                L_Pupil_Diameter.mm, L_Event,
                                R_Pupil_Diameter.mm, R_Event,
                                L_Gaze_Position.x, L_Gaze_Position.y,
                                R_Gaze_Position.x, R_Gaze_Position.y,
                                Gaze.quality, ms_conversion)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                L_Pupil_Diameter.mm, L_Event,
                                R_Pupil_Diameter.mm, R_Event, ms_conversion)
        }
      } else if (left.recorded == TRUE){
        data <- dplyr::mutate(data,
                              Event = ifelse((L_Event_Info == "-" |
                                                     is.na(L_Event_Info)),
                                                  NA, L_Event_Info))
        data <- dplyr::rename(data,
                              Pupil_Diameter.mm = L_Pupil_Diameter_mm)
        if (left.gaze == TRUE){
          data <- dplyr::rename(data,
                                Gaze_Position.x = L_POR_X_px,
                                Gaze_Position.y = L_POR_Y_px,
                                Gaze.quality = Timing)
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm, Event,
                                Gaze_Position.x, Gaze_Position.y,
                                Gaze.quality, ms_conversion)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm, Event, ms_conversion)
        }

      } else if (right.recorded == TRUE){
        data <- dplyr::mutate(data,
                              Event = ifelse((R_Event_Info == "-" |
                                                is.na(R_Event_Info)),
                                             NA, R_Event_Info))
        data <- dplyr::rename(data,
                              Pupil_Diameter.mm = R_Pupil_Diameter_mm)
        if (right.gaze == TRUE){
          data <- dplyr::rename(data,
                                Gaze_Position.x = R_POR_X_px,
                                Gaze_Position.y = R_POR_Y_px,
                                Gaze.quality = Timing)
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm, Event,
                                Gaze_Position.x, Gaze_Position.y,
                                Gaze.quality, ms_conversion)
        } else {
          data <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message,
                                Pupil_Diameter.mm, Event, ms_conversion)
        }
      }
    }

    if (model == "glasses") {
      left.recorded == TRUE
      right.recorded == TRUE
      subj <- subj.extract(data$Participant[1],
                           prefix = subj.prefix, suffix = subj.suffix)
      if ("Point_of_Regard_Binocular_X_px" %in% colnames(data)) {
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
                              Message_Inserted = ifelse(is.na(Time), 1, 0),
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
                              Subject, Time,
                              Trial, Message, L_Pupil_Diameter.mm,
                              L_Event, R_Pupil_Diameter.mm, R_Event,
                              L_Gaze_Position.x, L_Gaze_Position.y,
                              R_Gaze_Position.x, R_Gaze_Position.y,
                              ms_conversion)
      } else {
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
                              Message_Inserted = ifelse(is.na(Time), 1, 0),
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
                              ms_conversion = 1)
        data <- dplyr::select(data,
                              Subject, Time,
                              Trial, Message, L_Pupil_Diameter.mm,
                              L_Event, R_Pupil_Diameter.mm, R_Event,
                              ms_conversion)
      }
      if (!is.null(subj.prefix)) {
        data <- dplyr::mutate(data,
                              Subject =
                                stringr::str_split(Subject, subj.prefix)[[1]][2])
      }
      if (!is.null(subj.prefix)) {
        data <- dplyr::mutate(data,
                              Subject =
                                stringr::str_split(Subject, subj.suffix)[[1]][1])
      }
    }
  }

  if (eyetracker=="eyelink") {

    data <- readr::read_delim(file, "\t", escape_double = FALSE,
                              trim_ws = TRUE, na = ".", guess_max = 100000)
    subj <- subj.extract(data$RECORDING_SESSION_LABEL[1],
                         prefix = subj.prefix, suffix = subj.suffix)
    data <- dplyr::mutate(data, Subject = subj, Trial = NA, ms_conversion = 1)
    data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE)

    left.recorded <- "LEFT_PUPIL_SIZE" %in% colnames(data)
    right.recorded <- "RIGHT_PUPIL_SIZE" %in% colnames(data)

    if (left.recorded == TRUE & right.recorded == TRUE) {
      data <- dplyr::mutate(data,
                            L_Event = ifelse(LEFT_IN_BLINK == 1, "Blink", "Fixation/Saccade"),
                            R_Event = ifelse(RIGHT_IN_BLINK == 1, "Blink", "Fixation/Saccade"))
      if ("LEFT_IN_SACCADE" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              L_Event = ifelse(LEFT_IN_SACCADE == 1,
                                               "Saccade", L_Event))
      }
      if ("LEFT_FIX_INDEX" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              L_Event = ifelse(!is.na(LEFT_FIX_INDEX),
                                               "Fixation", L_Event))
      }
      if ("RIGHT_IN_SACCADE" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              R_Event = ifelse(RIGHT_IN_SACCADE == 1,
                                               "Saccade", R_Event))
      }
      if ("RIGHT_FIX_INDEX" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              R_Event = ifelse(!is.na(RIGHT_FIX_INDEX),
                                               "Fixation", R_Event))
      }
      if (!is.null(px_to_mm.conversion)) {
        data <- dplyr::mutate(data,
                              L_Pupil_Diameter.mm =
                                px_to_mm.conversion * LEFT_PUPIL_SIZE,
                              R_Pupil_Diameter.mm =
                                px_to_mm.conversion * RIGHT_PUPIL_SIZE)
        data <- dplyr::select(data, Subject, Time, Trial, Message,
                              L_Pupil_Diameter.mm, L_Event,
                              R_Pupil_Diameter.mm, R_Event,
                              ms_conversion, subset)
      } else {
        data <- dplyr::select(data, Subject, Time, Trial, Message,
                              L_Pupil_Diameter.px = LEFT_PUPIL_SIZE, L_Event,
                              R_Pupil_Diameter.px = RIGHT_PUPIL_SIZE, R_Event,
                              ms_conversion, subset)
      }

    } else if (left.recorded == TRUE) {
      data <- dplyr::mutate(data,
                            L_Event = ifelse(LEFT_IN_BLINK == 1, "Blink", "Fixation/Saccade"))
      if ("LEFT_IN_SACCADE" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              L_Event = ifelse(LEFT_IN_SACCADE == 1,
                                               "Saccade", L_Event))
      }
      if ("LEFT_FIX_INDEX" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              L_Event = ifelse(!is.na(LEFT_FIX_INDEX),
                                               "Fixation", L_Event))
      }
      if (!is.null(px_to_mm.conversion)) {
        data <- dplyr::mutate(data,
                              Pupil_Diameter.mm =
                                px_to_mm.conversion * LEFT_PUPIL_SIZE)
        data <- dplyr::select(data, Subject, Time, Trial, Message,
                              Pupil_Diameter.mm, Event = L_Event,
                              ms_conversion, subset)
      } else {
        data <- dplyr::select(data, Subject, Time, Trial, Message,
                              L_Pupil_Diameter.px = LEFT_PUPIL_SIZE,
                              Event = L_Event, ms_conversion, subset)
      }
    } else if (right.recorded == TRUE) {
      data <- dplyr::mutate(data,
                            R_Event = ifelse(RIGHT_IN_BLINK == 1, "Blink", "Fixation/Saccade"))
      if ("RIGHT_IN_SACCADE" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              R_Event = ifelse(RIGHT_IN_SACCADE == 1,
                                               "Saccade", R_Event))
      }
      if ("RIGHT_FIX_INDEX" %in% colnames(data)) {
        data <- dplyr::mutate(data,
                              R_Event = ifelse(!is.na(RIGHT_FIX_INDEX),
                                               "Fixation", R_Event))
      }

      if (!is.null(px_to_mm.conversion)) {
        data <- dplyr::mutate(data,
                              Pupil_Diameter.mm =
                                px_to_mm.conversion * RIGHT_PUPIL_SIZE)
        data <- dplyr::select(data, Subject, Time, Trial, Message,
                              Pupil_Diameter.mm, Event = R_Event,
                              ms_conversion, subset)
      } else {
        data <- dplyr::select(data, Subject, Time, Trial, Message,
                              Pupil_Diameter.px = RIGHT_PUPIL_SIZE,
                              Event = R_Event, ms_conversion, subset)
      }
    }
  }

  data$Message <- gsub("# Message: ", "", data$Message)

  ## Set Trial at start_tracking.message
  if (is.null(timing.file)) {
    timing_data <- data.frame()
  } else if (stringr::str_detect(timing.file, "csv")) {
    timing_data <- readr::read_csv(timing.file)
  } else if (stringr::str_detect(timing.file, "xlsx")) {
    timing_data <- readxl::read_excel(timing.file)
  }
  if ("Subject" %in% colnames(timing_data)) {
    subj <- data$Subject[1]
    timing_data <- dplyr::filter(timing_data, Subject == subj)
  }
  if (ncol(timing_data) > 0) {
    starttrack_timing <- dplyr::mutate(timing_data,
                                       Time = get(start_tracking.message),
                                       Message = start_tracking.message)
    starttrack_timing <- dplyr::select(starttrack_timing, Trial, Time, Message)
    data <- dplyr::full_join(data, starttrack_timing, by = "Time")
    data <- dplyr::rename(data, Message = Message.y)
    data <- dplyr::select(data, -Trial.x, -Trial.y, -Message.x)
    data <- dplyr::mutate(data,
                          Message_Inserted = ifelse(is.na(Subject),
                                                    1, 0),
                          Subject = zoo::na.locf(Subject, na.rm = FALSE),
                          ms_conversion = zoo::na.locf(ms_conversion,
                                                       na.rm = FALSE))
    if (model == "Red250m") {
      data <- dplyr::mutate(data,
                            Head_Dist.cm = zoo::na.locf(Head_Dist.cm,
                                                        na.rm = FALSE),
                            Gaze.quality = zoo::na.locf(Gaze.quality,
                                                        na.rm = FALSE))
    }
    data <- dplyr::arrange(data, Subject, Time)
  }
  if (start_tracking.match == "exact"){
    data <- dplyr::mutate(data,
                          starttracking.time =
                            ifelse(Message == start_tracking.message, Time, NA))
  } else if (start_tracking.match == "pattern"){
    data <- dplyr::mutate(data,
                          starttracking.time =
                            ifelse(stringr::str_detect(Message,
                                                       start_tracking.message),
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
                                           start_tracking.message, negate = TRUE)
    for (message in message_markers) {
      message_start <- dplyr::filter(data, Message == start_tracking.message)
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
      data <- dplyr::full_join(data, message_start, by = "Time")
      data <- dplyr::mutate(data,
                            Message = dplyr::case_when(
                              !is.na(Message.x) ~ Message.x,
                              !is.na(Message.y) ~ Message.y,
                              TRUE ~ as.character(NA)),
                            Trial = ifelse(is.na(Trial.x), Trial.y,
                                           ifelse(is.na(Trial.y),
                                                  Trial.x, Trial.x)))
      data <- dplyr::select(data, -Trial.y, -Message.x, -Message.y)
      data <- dplyr::mutate(data,
                            Message_Inserted = ifelse(is.na(Subject),
                                                      1, Message_Inserted),
                            Subject = zoo::na.locf(Subject, na.rm = FALSE),
                            ms_conversion = zoo::na.locf(ms_conversion,
                                                         na.rm = FALSE))
      if (model == "Red250m") {
        data <- dplyr::mutate(data,
                              Head_Dist.cm = zoo::na.locf(Head_Dist.cm,
                                                          na.rm = FALSE),
                              Gaze.quality = zoo::na.locf(Gaze.quality,
                                                          na.rm = FALSE))
      }

      data <- dplyr::arrange(data, Subject, Trial, Time)
    }

  }

  if (!("Message_Inserted" %in% colnames(data))) {
    data <- dplyr::mutate(data, Message_Inserted = 0)
  }

  data <- dplyr::select(data, -starttracking.time)

  if (left.recorded == TRUE & right.recorded == TRUE) {
    col_order <- c("Subject", "Trial", "Time", "Message", "Message_Inserted",
                   "L_Pupil_Diameter.mm", "R_Pupil_Diameter.mm",
                   "L_Pupil_Diameter.px", "R_Pupil_Diameter.px", "L_Event",
                   "R_Event", "L_Gaze_Position.x", "L_Gaze_Position.y",
                   "R_Gaze_Position.x", "R_Gaze_Position.y", "Gaze.quality",
                   "Head_Dist.cm", "ms_conversion", subset)

  } else {
    col_order <- c("Subject", "Trial", "Time", "Message", "Message_Inserted",
                   "Pupil_Diameter.mm", "Pupil_Diameter.px", "Event",
                   "Gaze_Position.x", "Gaze_Position.y", "Gaze.quality",
                   "Head_Dist.cm", "ms_conversion", subset)
  }

  col_order <- colnames(data)[order(match(colnames(data), col_order))]

  data <- data[,col_order]

  if (!is.null(trial.exclude)){
    data <- dplyr::filter(data, !(Trial %in% trial.exclude))
  }
  return(data)
}
