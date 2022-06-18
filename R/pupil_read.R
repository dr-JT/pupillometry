#' Import eye tracking data
#'
#' Imports eye tracking data from any eye tracker with special support for SMI,
#' SR Research, and Tobii eye trackers. Standardizes columns and values
#' to be used with other functions in this package. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Standardized columns and values to be used with other functions in this
#' package.
#'
#' @section Eye Tracker Support:
#'
#' Ease of import is provided for certain eye trackers:
#'
#' - SMI: Red250m and glasses: `pupil_read(eyetracker = "smi")`
#' - SR Research: EyeLink 1000 Plus `pupil_read(eyetracker = "eyelink")`
#' - Tobii: Pro Fusion `pupil_read(eyetracker = "tobii")`
#'
#' Other eye trackers from these companies may or may not be supported but you
#' are welcome to try.
#'
#' All other eye trackers and data file types can be imported by specifying
#' the column names.
#'
#' @section Importing Options:
#'
#' There are several importing options to specify, some of them required, others
#' are more optional. See the full argument list for all options.
#'
#' - start_tracking_message (recommended): helps to correctly
#' identify trials segments
#'
#' - eye_use (optional): specify which eye you want to perform analyses on
#'
#' - px_to_mm_conversion (optional): convert pupil pixel values to millimeters
#'
#' - include_col (optional): other columns you may want to
#' include (e.g., RT, accuracy, condition)
#'
#' - trial_exclude (optional): remove certain trials from the data
#'
#' - quality_check_dir (optional): a directory to save an import
#' quality check file to
#'
#' - subj_prefix: required for SMI eye trackers, might be useful
#' to remove strings in subject column
#'
#' - subj_suffix: required for SMI eye trackers, might be useful
#' to remove strings in subject column
#'
#' @param file A file path to the raw data.
#' @param eyetracker Which eye tracker system was used to record data?
#'     options: "smi", "eyelink", NULL.
#' @param eye_use Which eye to use?
#'     options: "left", "right" or NULL (default to keep both).
#' @param subj_prefix The unique pattern prefix (letter(s)
#'     and/or symbol(s)) that comes before the subject number in the data file.
#'     Required for SMI, but likely not needed for other eye trackers.
#' @param subj_suffix The unique pattern suffix (letter(s) or
#'     symbol(s)) that comes after the subject number in the data file.
#'     Required for SMI, but likely not needed for other eye trackers.
#' @param include_col Extra columns from the raw data file to include. c().
#' @param start_tracking_message Message used in experiment software to
#'     mark the onset of "Start Tracking". Most eye trackers include a marker
#'     message corresponding to this event. Usually included at the start of
#'     every trial. Not required for SMI, SR Research, and Tobii Pro Fusion.
#' @param start_tracking_match Should the message string be an
#'     "exact" match or a "pattern" match? default: "exact".
#' @param ms_conversion The conversion factor to get the timing column in
#'     milliseconds.
#' @param px_to_mm_conversion The conversion factor to go from
#'     px pupil diameter to mm pupil diameter.
#' @param trial_exclude Specify if there are any trials to exclude. c().
#' @param quality_check_dir Directory to save quality check file to.
#' @param timing_file File location and name that contains timing
#'     information for message markers. Required if no message markers are
#'     included in data.
#' @param delim File delimiter type. (needed if eyetracker is not specified)
#' @param na How are missing values defined in the data files
#' @param subject Column name that contains subject IDs.
#'     (needed if eyetracker is not specified)
#' @param trial Column name that contains the trial number.
#'     (needed if eyetracker is not specified)
#' @param time Column name that contains the timing variable.
#'     (needed if eyetracker is not specified)
#' @param message_event Column name that contains the message markers.
#'     (needed if eyetracker is not specified)
#' @param pupil.mm Column name that contains the pupil data in millimeters.
#'     If the eye tracker data file has pupil data from only one eye use this
#'     argument.
#' @param pupil.px Column name that contains the pupil data in pixels.
#'     If the eye tracker data file has pupil data from only one eye use this
#'     argument.
#' @param gaze.x Column name that contains the gaze position on the x-axis.
#'     If the eye tracker data file has data from only one eye use this argument.
#' @param gaze.y Column name that contains the gaze position on the y-axis.
#'     If the eye tracker data file has data from only one eye use this argument.
#' @param blink_event Column name that contains whether the eye event sample is
#'     in a blink or not in a blink. If the eye tracker data file
#'     has data from only one eye use this argument.
#' @param fixation_event Column name that contains whether the eye event sample
#'     is in a fixation or not in a fixation. If the eye tracker data file
#'     has data from only one eye use this argument.
#' @param saccade_event Column name that contains whether the eye event sample
#'     is in a saccade or not in a saccade. If the eye tracker data file
#'     has data from only one eye use this argument.
#' @param eye_event Column name that contains the eye event data
#'     (e.g., fixation, saccade, and blink). If the eye tracker data file
#'     has data from only one eye use this argument. Some eye trackers will
#'     store the event data in one column with values corresponding to
#'     fixation, blink, or saccades. You also need to specify what these
#'     values are with the eye_event_blink,  eye_event_fixation, and
#'     eye_event_saccade arguments.
#' @param eye_event_blink Values in the eye_event column that correspond to
#'     blinks.
#' @param eye_event_fixation Values in the eye_event column that correspond to
#'     fixations.
#' @param eye_event_saccade Values in the eye_event column that correspond to
#'     saccades.
#' @param left_pupil.mm Column name that contains the left pupil data in
#'     millimeters. If the eye tracker data file has pupil data from both eyes
#'     use this argument for the left pupil.
#' @param right_pupil.mm Column name that contains the right pupil data in
#'     millimeters. If the eye tracker data file has pupil data from both eyes
#'     use this argument for the right pupil.
#' @param left_pupil.px Column name that contains the left pupil data in pixels.
#'     If the eye tracker data file has pupil data from both eyes
#'     use this argument for the left pupil.
#' @param right_pupil.px Column name that contains the right pupil data in pixels.
#'     If the eye tracker data file has pupil data from both eyes
#'     use this argument for the right pupil.
#' @param left_gaze.x Column name that contains the left eye gaze position
#'     on the x-axis. If the eye tracker data file has data from both eyes
#'     use this argument for the left eye.
#' @param left_gaze.y Column name that contains the left eye gaze position
#'     on the y-axis. If the eye tracker data file has data from both eyes
#'     use this argument for the left eye.
#' @param right_gaze.x Column name that contains the right eye gaze position
#'     on the x-axis. If the eye tracker data file has data from both eyes
#'     use this argument for the right eye.
#' @param right_gaze.y Column name that contains the right eye gaze position
#'     on the y-axis. If the eye tracker data file has data from both eyes
#'     use this argument for the right eye.
#' @param left_blink_event Column name that contains whether the eye event
#'     sample is in a blink or not in a blink. If the eye tracker data file
#'     has data from both eyes use this argument for the left eye.
#' @param left_fixation_event Column name that contains whether the eye event
#'     sample is in a fixation or not in a fixation. If the eye tracker data file
#'     has data from both eyes use this argument for the left eye.
#' @param left_saccade_event Column name that contains whether the eye event
#'     sample is in a saccade or not in a saccade. If the eye tracker data file
#'     has data from both eyes use this argument for the left eye.
#' @param right_blink_event Column name that contains whether the eye event
#'     sample is in a blink or not in a blink. If the eye tracker data file
#'     has data from both eyes use this argument for the right eye.
#' @param right_fixation_event Column name that contains whether the eye event
#'     sample is in a fixation or not in a fixation. If the eye tracker data file
#'     has data from both eyes use this argument for the right eye.
#' @param right_saccade_event Column name that contains whether the eye event
#'     sample is in a saccade or not in a saccade. If the eye tracker data file
#'     has data from only one eye use this argument. If the eye tracker data file
#'     has data from both eyes use this argument for the right eye.
#' @param left_eye_event Column name that contains the left eye event data
#'     (e.g., fixation, saccade, and blink). If the eye tracker data file
#'     has data from both eyes use this argument for the left eye.
#'     Some eye trackers will store the event data in one column with values
#'     corresponding to fixation, blink, or saccades. You also need to specify
#'     what these values are with the eye_event_blink, eye_event_fixation,
#'     and eye_event_saccade arguments.
#' @param right_eye_event Column name that contains the right eye event data
#'     (e.g., fixation, saccade, and blink). If the eye tracker data file
#'     has data from both eyes use this argument for the right eye.
#'     Some eye trackers will store the event data in one column with values
#'     corresponding to fixation, blink, or saccades. You also need to specify
#'     what these values are with the eye_event_blink, eye_event_fixation,
#'     and eye_event_saccade arguments.
#' @param start_tracking.message See start_tracking_message.
#' @param start_tracking.match See start_tracking_match.
#' @param starttracking.message See start_tracking_message.
#' @param starttracking.match See start_tracking_match.
#' @export
#'

pupil_read <- function(file, eyetracker = "", eye_use = NULL,
                       subj_prefix = NULL, subj_suffix = NULL,
                       include_col = NULL,
                       start_tracking_message = "default",
                       start_tracking_match = "exact",
                       ms_conversion = NULL, px_to_mm_conversion = NULL,
                       trial_exclude = NULL, quality_check_dir = NULL,
                       timing_file = NULL,
                       delim = NULL, na = "NA",
                       subject = NULL, trial = NULL, time = NULL,
                       message_event = NULL,
                       pupil.mm = NULL, pupil.px = NULL,
                       gaze.x = NULL, gaze.y = NULL,
                       blink_event = NULL, fixation_event = NULL,
                       saccade_event = NULL, eye_event = NULL,
                       eye_event_blink = "Blink",
                       eye_event_fixation = "Fixation",
                       eye_event_saccade = "Saccade",
                       left_pupil.mm = NULL, right_pupil.mm = NULL,
                       left_pupil.px = NULL, right_pupil.px = NULL,
                       left_gaze.x = NULL, left_gaze.y = NULL,
                       right_gaze.x = NULL, right_gaze.y = NULL,
                       left_blink_event = NULL, right_blink_event = NULL,
                       left_fixation_event = NULL, right_fixation_event = NULL,
                       left_saccade_event = NULL, right_saccade_event = NULL,
                       left_eye_event = NULL, right_eye_event = NULL,
                       start_tracking.message = NULL,
                       start_tracking.match = NULL,
                       starttracking.message = NULL,
                       starttracking.match = NULL) {
  ## Setup and functions ####
  if (!is.null(start_tracking.message)) {
    start_tracking_message <- start_tracking.message
  }
  if (!is.null(start_tracking.match)) {
    start_tracking_match <- start_tracking.match
  }
  if (!is.null(starttracking.message)) {
    start_tracking_message <- starttracking.message
  }
  if (!is.null(starttracking.match)) {
    start_tracking_match <- starttracking.match
  }

  subj.extract <- function(x, prefix, suffix){
    x <- stringr::str_split(x, "/")[[1]]
    x <- x[length(x)]
    if (!is.null(prefix)) {
      if (!is.null(suffix)) {
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
    } else if (!is.null(suffix)) {
      pattern.suffix <- paste("(?<=\\d)", suffix, sep = "")

      subj <- stringr::str_split(x, pattern.suffix)[[1]][1]
      subj <- as.numeric(subj)
    } else {
      subj <- x
    }
    return(subj)
  }
  ###################################

  ## Import and Standardize ####
  if (eyetracker == "smi") {
    ## Eye tracker is SMI ####
    if (!is.null(start_tracking_message)) {
      if (start_tracking_message == "default") {
        start_tracking_message <- "StartTracking.bmp"
      }
    }

    header <- readr::read_table(file, col_names = FALSE)
    if (ncol(header) >= 1) {
      samples.total <- as.numeric(strsplit(header$X1[10], "\t")[[1]][2])
      head.distance <- as.numeric(strsplit(header$X1[24], "\t")[[1]][2])/10
      found <- NA
      checkrow <- 0
      while (is.na(found)) {
        checkrow <- checkrow + 1
        found <- match("Time", strsplit(header[checkrow,][[1]], "\t")[[1]][1])
        datastart <- checkrow - 1
      }
    } else {
      datastart <- 0
    }
    data <- readr::read_delim(file, "\t", escape_double = FALSE,
                              trim_ws = TRUE, skip = datastart,
                              guess_max = 100000)
    names(data) <- gsub(" ", "_", gsub("\\[mm\\]", "mm",
                                       gsub("\\[px\\]", "px", names(data))))

    data <- dtplyr::lazy_dt(data)

    if ("Time" %in% colnames(data)) {
      model <- "Red250m"
    } else {
      model <- "glasses"
    }

    if (model == "Red250m") {
      ## SMI eyetracker is Red250m ####
      subj <- subj.extract(file, prefix = subj_prefix, suffix = subj_suffix)
      ms_conversion <- 1000
      message.column <- names(data[4])

      data <- dplyr::mutate(data,
                            Message = ifelse(get(message.column) >= 0,
                                             NA, get(message.column)))

      left_recorded <- "L_Pupil_Diameter_mm" %in% colnames(data)
      right_recorded <- "R_Pupil_Diameter_mm" %in% colnames(data)
      left_gaze <- "L_POR_X_px" %in% colnames(data)
      right_gaze <- "R_POR_X_px" %in% colnames(data)
      if (left_recorded == TRUE) {
        if (right_recorded == TRUE) {
          data <- dplyr::mutate(data,
                                L_Pupil_Diameter.mm = L_Pupil_Diameter_mm,
                                L_Eye_Event = ifelse((L_Event_Info == "-" |
                                                        is.na(L_Event_Info)),
                                                     NA, L_Event_Info))
          if (left_gaze == TRUE) {
            data <- dplyr::rename(data,
                                  L_Gaze_Position.x = L_POR_X_px,
                                  L_Gaze_Position.y = L_POR_Y_px)
          }
        } else {
          data <- dplyr::mutate(data,
                                Pupil_Diameter.mm = L_Pupil_Diameter_mm,
                                Eye_Event = ifelse((L_Event_Info == "-" |
                                                        is.na(L_Event_Info)),
                                                     NA, L_Event_Info))
          if (left_gaze == TRUE) {
            data <- dplyr::rename(data,
                                  Gaze_Position.x = L_POR_X_px,
                                  Gaze_Position.y = L_POR_Y_px)
          }
        }

      }

      if (right_recorded == TRUE) {
        if (left_recorded == TRUE) {
          data <- dplyr::mutate(data,
                                R_Pupil_Diameter.mm = R_Pupil_Diameter_mm,
                                R_Eye_Event = ifelse((R_Event_Info == "-" |
                                                        is.na(R_Event_Info)),
                                                     NA, R_Event_Info))
          if (right_gaze == TRUE) {
            data <- dplyr::rename(data,
                                  R_Gaze_Position.x = R_POR_X_px,
                                  R_Gaze_Position.y = R_POR_Y_px)
          }
        } else {
          data <- dplyr::mutate(data,
                                Pupil_Diameter.mm = R_Pupil_Diameter_mm,
                                Eye_Event = ifelse((R_Event_Info == "-" |
                                                        is.na(R_Event_Info)),
                                                     NA, R_Event_Info))
          if (right_gaze == TRUE) {
            data <- dplyr::rename(data,
                                  Gaze_Position.x = R_POR_X_px,
                                  Gaze_Position.y = R_POR_Y_px)
          }
        }
      }
      ###################################
    } else if (model == "glasses") {
      ## SMI eyetracker is glasses ####
      subj <- subj.extract(data$Participant[1], prefix = subj_prefix,
                           suffix = subj_suffix)
      ms_conversion <- 1
      head.distance <- NA

      eye.gaze <- "Point_of_Regard_Binocular_X_px" %in% colnames(data)

      data <- dplyr::mutate(data,
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
                            L_Eye_Event = Event,
                            R_Eye_Event = Event,
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
                            L_Pupil_Diameter.mm =
                              as.numeric(L_Pupil_Diameter.mm),
                            R_Pupil_Diameter.mm =
                              as.numeric(R_Pupil_Diameter.mm))

      if (eye.gaze == TRUE) {
        data <- dplyr::mutate(data,
                              L_Gaze_Position.x =
                                Point_of_Regard_Binocular_X_px,
                              R_Gaze_Position.x =
                                Point_of_Regard_Binocular_X_px,
                              L_Gaze_Position.y =
                                Point_of_Regard_Binocular_Y_px,
                              R_Gaze_Position.y =
                                Point_of_Regard_Binocular_Y_px)
      }
      ######################################
    }
    data <- dplyr::mutate(data, Head_Distance.cm = head.distance)
    ################################
  } else if (eyetracker == "eyelink") {
    ## Eye tracker is EyeLink ####
    if (!is.null(start_tracking_message)) {
      if (start_tracking_message == "default") {
        start_tracking_message <- "TRIALID"
      }
    }

    data <- readr::read_delim(file, "\t", escape_double = FALSE,
                              trim_ws = TRUE, na = ".", guess_max = 100000)

    #data <- dtplyr::lazy_dt(data)

    data <- dplyr::rename(data, Time = TIMESTAMP, Message = SAMPLE_MESSAGE)
    if ("HTARGET_DISTANCE" %in% colnames(data)) {
      data <- dplyr::rename(data, Head_Distance.cm = HTARGET_DISTANCE)
    }
    if (!is.null(subj_prefix) | !is.null(subj_suffix)) {
      subj <- subj.extract(data$RECORDING_SESSION_LABEL[1],
                           prefix = subj_prefix, suffix = subj_suffix)
    }

    ms_conversion <- 1
    left_recorded <- "LEFT_PUPIL_SIZE" %in% colnames(data)
    right_recorded <- "RIGHT_PUPIL_SIZE" %in% colnames(data)
    left_gaze <- "LEFT_GAZE_X" %in% colnames(data)
    right_gaze <- "RIGHT_GAZE_X" %in% colnames(data)
    if (left_recorded == TRUE) {
      if (right_recorded == TRUE) {
        data <- dplyr::mutate(data,
                              L_Pupil_Diameter.px = LEFT_PUPIL_SIZE,
                              L_Eye_Event = ifelse(LEFT_IN_BLINK == 1,
                                                   "Blink", NA))
        if ("LEFT_IN_SACCADE" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                L_Eye_Event = ifelse(LEFT_IN_SACCADE == 1,
                                                     "Saccade", L_Eye_Event))
        }
        if ("LEFT_FIX_INDEX" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                L_Eye_Event = ifelse(!is.na(LEFT_FIX_INDEX),
                                                     "Fixation", L_Eye_Event))
        }
        if (left_gaze == TRUE) {
          data <- dplyr::rename(data,
                                L_Gaze_Position.x = LEFT_GAZE_X,
                                L_Gaze_Position.y = LEFT_GAZE_Y)
        }
      } else {
        data <- dplyr::mutate(data,
                              Pupil_Diameter.px = LEFT_PUPIL_SIZE,
                              Eye_Event = ifelse(LEFT_IN_BLINK == 1,
                                                 "Blink", NA))
        if ("LEFT_IN_SACCADE" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                Eye_Event = ifelse(LEFT_IN_SACCADE == 1,
                                                   "Saccade", Eye_Event))
        }
        if ("LEFT_FIX_INDEX" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                Eye_Event = ifelse(!is.na(LEFT_FIX_INDEX),
                                                   "Fixation", Eye_Event))
        }
        if (left_gaze == TRUE) {
          data <- dplyr::rename(data,
                                Gaze_Position.x = LEFT_GAZE_X,
                                Gaze_Position.y = LEFT_GAZE_Y)
        }
      }
    }

    if (right_recorded == TRUE) {
      if (left_recorded == TRUE) {
        data <- dplyr::mutate(data,
                              R_Pupil_Diameter.px = RIGHT_PUPIL_SIZE,
                              R_Eye_Event = ifelse(RIGHT_IN_BLINK == 1,
                                                   "Blink", NA))
        if ("RIGHT_IN_SACCADE" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                R_Eye_Event = ifelse(RIGHT_IN_SACCADE == 1,
                                                     "Saccade", R_Eye_Event))
        }
        if ("RIGHT_FIX_INDEX" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                R_Eye_Event = ifelse(!is.na(RIGHT_FIX_INDEX),
                                                     "Fixation", R_Eye_Event))
        }
        if (left_gaze == TRUE) {
          data <- dplyr::rename(data,
                                R_Gaze_Position.x = RIGHT_GAZE_X,
                                R_Gaze_Position.y = RIGHT_GAZE_Y)
        }
      } else {
        data <- dplyr::mutate(data,
                              Pupil_Diameter.px = RIGHT_PUPIL_SIZE,
                              Eye_Event = ifelse(RIGHT_IN_BLINK == 1,
                                                 "Blink", NA))
        if ("RIGHT_IN_SACCADE" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                Eye_Event = ifelse(RIGHT_IN_SACCADE == 1,
                                                   "Saccade", Eye_Event))
        }
        if ("RIGHT_FIX_INDEX" %in% colnames(data)) {
          data <- dplyr::mutate(data,
                                Eye_Event = ifelse(!is.na(RIGHT_FIX_INDEX),
                                                   "Fixation", Eye_Event))
        }
        if (left_gaze == TRUE) {
          data <- dplyr::rename(data,
                                Gaze_Position.x = RIGHT_GAZE_X,
                                Gaze_Position.y = RIGHT_GAZE_Y)
        }
      }
    }
    ################################
  } else if (eyetracker == "") {
    ## Eye tracker is not specified ####
    if (delim == "\t") {
      data <- readr::read_delim(file, delim = "\t", escape_double = FALSE,
                                trim_ws = TRUE, guess_max = 100000,
                                na = na)
    } else if (delim == "," | delim == "csv") {
      data <- readr::read_csv(file, guess_max = 100000, na = na)
    }

    data <- dtplyr::lazy_dt(data)

    data <- dplyr::select(data,
                          Subject = subject, Trial = trial, Time = time,
                          Message = message_event,
                          Pupil_Diameter.mm = pupil.mm,
                          Pupil_Diameter.px = pupil.px,
                          L_Pupil_Diameter.mm = left_pupil.mm,
                          L_Pupil_Diameter.px = left_pupil.px,
                          R_Pupil_Diameter.mm = right_pupil.mm,
                          R_Pupil_Diameter.px = right_pupil.px,
                          Gaze_Position.x = gaze.x, Gaze_Position.y = gaze.y,
                          L_Gaze_Position.x = left_gaze.x,
                          L_Gaze_Position.y = left_gaze.y,
                          R_Gaze_Position.x = right_gaze.x,
                          R_Gaze_Position.y = right_gaze.y,
                          Eye_Event = eye_event, Blink_Event = blink_event,
                          Fixation_Event = fixation_event,
                          Saccade_Event = saccade_event,
                          L_Eye_Event = left_eye_event,
                          L_Blink_Event = left_blink_event,
                          L_Fixation_Event = left_fixation_event,
                          L_Saccade_Event = left_saccade_event,
                          R_Eye_Event = right_eye_event,
                          R_Blink_Event = right_blink_event,
                          R_Fixation_Event = right_fixation_event,
                          R_Saccade_Event = right_saccade_event,
                          include_col)

    if (is.null(left_pupil.mm) & is.null(left_pupil.px)) {
      if (!is.null(right_pupil.mm)) {
        data <- dplyr::rename(data,
                              Pupil_Diameter.mm = R_Pupil_Diameter.mm)
      }
      if (!is.null(right_pupil.px)) {
        data <- dplyr::rename(data,
                              Pupil_Diameter.px = R_Pupil_Diameter.px)
      }
    }

    if (is.null(right_pupil.mm) & is.null(right_pupil.px)) {
      if (!is.null(left_pupil.mm)) {
        data <- dplyr::rename(data,
                              Pupil_Diameter.mm = L_Pupil_Diameter.mm)
      }
      if (!is.null(left_pupil.px)) {
        data <- dplyr::rename(data,
                              Pupil_Diameter.px = L_Pupil_Diameter.px)
      }
    }

    if (is.null(eye_event) & is.null(left_eye_event) & is.null(right_eye_event)) {
      data <- dplyr::mutate(data, Eye_Event = NA, L_Eye_Event = NA,
                            R_Eye_Event = NA)

      if (!is.null(blink_event)) {
        data <- dplyr::mutate(data,
                              Eye_Event =
                                ifelse(!is.na(Blink_Event),
                                       ifelse(Blink_Event >= 1,
                                              "Blink", Eye_Event),
                                       Eye_Event))
      }
      if (!is.null(fixation_event)) {
        data <- dplyr::mutate(data,
                              Eye_Event =
                                ifelse(!is.na(Fixation_Event),
                                       ifelse(Fixation_Event >= 1,
                                              "Fixation", Eye_Event),
                                       Eye_Event))
      }
      if (!is.null(saccade_event)) {
        data <- dplyr::mutate(data,
                              Eye_Event =
                                ifelse(!is.na(Saccade_Event),
                                       ifelse(Saccade_Event >= 1,
                                              "Saccade", Eye_Event),
                                       Eye_Event))
      }

      if (!is.null(left_blink_event)) {
        data <- dplyr::mutate(data,
                              L_Eye_Event =
                                ifelse(!is.na(L_Blink_Event),
                                       ifelse(L_Blink_Event >= 1,
                                              "Blink", L_Eye_Event),
                                       L_Eye_Event))
      }
      if (!is.null(left_fixation_event)) {
        data <- dplyr::mutate(data,
                              L_Eye_Event =
                                ifelse(!is.na(L_Fixation_Event),
                                       ifelse(L_Fixation_Event >= 1,
                                              "Fixation", L_Eye_Event),
                                       L_Eye_Event))
      }
      if (!is.null(left_saccade_event)) {
        data <- dplyr::mutate(data,
                              L_Eye_Event =
                                ifelse(!is.na(L_Saccade_Event),
                                       ifelse(L_Saccade_Event >= 1,
                                              "Saccade", L_Eye_Event),
                                       L_Eye_Event))
      }

      if (!is.null(right_blink_event)) {
        data <- dplyr::mutate(data,
                              R_Eye_Event =
                                ifelse(!is.na(R_Blink_Event),
                                       ifelse(R_Blink_Event >= 1,
                                              "Blink", R_Eye_Event),
                                       R_Eye_Event))
      }
      if (!is.null(right_fixation_event)) {
        data <- dplyr::mutate(data,
                              R_Eye_Event =
                                ifelse(!is.na(R_Fixation_Event),
                                       ifelse(R_Fixation_Event >= 1,
                                              "Fixation", R_Eye_Event),
                                       R_Eye_Event))
      }
      if (!is.null(right_saccade_event)) {
        data <- dplyr::mutate(data,
                              R_Eye_Event =
                                ifelse(!is.na(R_Saccade_Event),
                                       ifelse(R_Saccade_Event >= 1,
                                              "Saccade", R_Eye_Event),
                                       R_Eye_Event))
      }

      if (length(which(!is.na(data$Eye_Event))) == 0) {
        data <- dplyr::select(data, -Eye_Event)
      }
      if (length(which(!is.na(data$L_Eye_Event))) == 0) {
        data <- dplyr::select(data, -L_Eye_Event)
      }
      if (length(which(!is.na(data$R_Eye_Event))) == 0) {
        data <- dplyr::select(data, -R_Eye_Event)
      }
    }
    if (!is.null(ms_conversion)) {
      data <- dplyr::mutate(data, Time = Time / ms_conversion)
    }
    if (start_tracking_message == "default") start_tracking_message <- NULL
      ####################################
  }

  if (eyetracker != "") {
    data <- dplyr::mutate(data,
                          Subject = subj,
                          Time = Time / ms_conversion,
                          Message = gsub("# Message: ", "", Message))
    data <- dplyr::select(data,
                          Subject, Time, tidyselect::any_of("Trial"), Message,
                          tidyselect::any_of("Pupil_Diameter.mm"),
                          tidyselect::any_of("Pupil_Diameter.px"),
                          tidyselect::any_of("Gaze_Position.x"),
                          tidyselect::any_of("Gaze_Position.y"),
                          tidyselect::any_of("Eye_Event"),
                          tidyselect::any_of("L_Pupil_Diameter.mm"),
                          tidyselect::any_of("L_Pupil_Diameter.px"),
                          tidyselect::any_of("R_Pupil_Diameter.mm"),
                          tidyselect::any_of("R_Pupil_Diameter.px"),
                          tidyselect::any_of("L_Gaze_Position.x"),
                          tidyselect::any_of("L_Gaze_Position.y"),
                          tidyselect::any_of("R_Gaze_Position.x"),
                          tidyselect::any_of("R_Gaze_Position.y"),
                          tidyselect::any_of("L_Eye_Event"),
                          tidyselect::any_of("R_Eye_Event"),
                          tidyselect::any_of("Head_Distance.cm"),
                          tidyselect::any_of(include_col))
  }

  ########################################

  ## Select eye ####
  if (("L_Pupil_Diameter.mm" %in% colnames(data) &
       "R_Pupil_Diameter.mm" %in% colnames(data)) |
      ("L_Pupil_Diameter.px" %in% colnames(data) &
       "R_Pupil_Diameter.px" %in% colnames(data))) {
    data <- pupil_cor(data)
  }
  if (!is.null(eye_use)) {
    data <- select_eye(data, eye_use = eye_use)
  }
  ##################

  ## If timing file exists insert start message marker ####
  if (!is.null(timing_file)) {
    if (file.exists(timing_file)) {
      if (stringr::str_detect(timing_file, "csv")) {
        timing_data <- readr::read_csv(timing_file)
      } else if (stringr::str_detect(timing_file, "xlsx")) {
        timing_data <- readxl::read_excel(timing_file)
      }
      subj <- data$Subject[1]
      timing_data <- dplyr::filter(timing_data, Subject == subj)
      starttrack_timing <- dplyr::mutate(timing_data,
                                         Time = get(start_tracking_message),
                                         Time = Time / ms_conversion,
                                         Message = start_tracking_message)
      starttrack_timing <- dplyr::select(starttrack_timing, Trial, Time, Message)
      data <- dplyr::select(data, -Message, -tidyselect::any_of("Trial"))
      data <- dplyr::full_join(data, starttrack_timing, by = "Time")
      data <- dplyr::mutate(data,
                            Message_Inserted = ifelse(is.na(Subject),
                                                      1, 0),
                            Subject = zoo::na.locf(Subject, na.rm = FALSE),
                            Head_Distance.cm = zoo::na.locf(Head_Distance.cm,
                                                            na.rm = FALSE))
      data <- dplyr::arrange(data, Subject, Time)
    }
  } else {
    data <- dplyr::mutate(data, Message_Inserted = 0)
  }
  ########################################################

  ## Set Trial at start_tracking_message ####
  if (!is.null(start_tracking_message)) {
    if (start_tracking_match == "exact") {
      data <- dplyr::mutate(data,
                            starttracking.time =
                              ifelse(Message == start_tracking_message,
                                     Time, NA))
    } else if (start_tracking_match == "pattern") {
      data <- dplyr::mutate(data,
                            starttracking.time =
                              ifelse(stringr::str_detect(
                                Message, start_tracking_message),
                                Time, NA))
    }
    data <- dplyr::mutate(data,
                          starttracking.time = zoo::na.locf(starttracking.time,
                                                            na.rm = FALSE),
                          Trial = dplyr::dense_rank(starttracking.time))
    check <- data
    check <- pupil_missing(check)
    check <- dplyr::select(check, Subject, Time, Trial,
                           contains("Pupil_Missing"), starttracking.time)
    check <- dplyr::filter(check, Time == starttracking.time)
    check <- dplyr::group_by(check, Subject)
    if (("L_Pupil_Diameter.mm" %in% colnames(data) &
         "R_Pupil_Diameter.mm" %in% colnames(data)) |
        ("L_Pupil_Diameter.px" %in% colnames(data) &
         "R_Pupil_Diameter.px" %in% colnames(data))) {
      check <- dplyr::summarise(check,
                                Trials = dplyr::n(),
                                L_Pupil_Missing.mean =
                                  mean(L_Pupil_Missing, na.rm = TRUE),
                                R_Pupil_Missing.mean =
                                  mean(R_Pupil_Missing, na.rm = TRUE))
    } else {
      check <- dplyr::summarise(check,
                                Trials = dplyr::n(),
                                Pupil_Missing.mean =
                                  mean(Pupil_Missing, na.rm = TRUE))
    }
    check <- dplyr::ungroup(check)
  } else {
    check <- data.frame()
  }
  ###########################################

  ## Save trial quality check ####
  if (!is.null(quality_check_dir)) {
    if (!dir.exists(quality_check_dir)) dir.create(quality_check_dir)
    check_file <- paste(quality_check_dir, "quality_check.csv", sep = "/")
    if (!file.exists(check_file)) {
      readr::write_csv(check, check_file)
    } else {
      data_check <- readr::read_csv(check_file)
      data_check <- dplyr::bind_rows(data_check, check)
      readr::write_csv(data_check, check_file)
    }
  }
  ###############################

  ## If timing file exists insert other message markers ####
  if (!is.null(timing_file)) {
    if (file.exists(timing_file)) {
      message_markers <- stringr::str_subset(colnames(timing_data), "Subject",
                                             negate = TRUE)
      message_markers <- stringr::str_subset(message_markers,
                                             "Trial", negate = TRUE)
      message_markers <- stringr::str_subset(message_markers,
                                             start_tracking_message,
                                             negate = TRUE)
      for (message in message_markers) {
        message_start <- dplyr::filter(data, Message == start_tracking_message)
        message_start <- dplyr::select(message_start, Trial, Time,
                                       Message, starttracking.time)
        message_start <- merge(timing_data, message_start, by = "Trial")
        message_start <- dplyr::mutate(message_start,
                                       check =
                                         ifelse(
                                           get(message) - starttracking.time > 0,
                                           "abs", "rel"),
                                       Time = ifelse(check == "abs",
                                                     get(message) /
                                                       ms_conversion,
                                                     Time +
                                                       (get(message) /
                                                          ms_conversion)),
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
        data <- dplyr::select(data, -Trial.y, -Trial.x, -Message.x, -Message.y)
        data <- dplyr::mutate(data,
                              Message_Inserted = ifelse(is.na(Subject),
                                                        1, Message_Inserted),
                              Subject = zoo::na.locf(Subject, na.rm = FALSE),
                              Head_Distance.cm = zoo::na.locf(Head_Distance.cm,
                                                              na.rm = FALSE))

        data <- dplyr::arrange(data, Subject, Trial, Time)
        data <- dplyr::relocate(data, Trial, .before = "Time")
      }
      data <- dplyr::mutate(data,
                            Message_Inserted = ifelse(is.na(Message),
                                                      0, 1),
                            Message = dplyr::lag(Message))
    }
  }
  ##########################################################

  if (is.null(start_tracking_message)) {
    if (!("Trial" %in% colnames(data))) {
      data <- dplyr::mutate(data, Trial = 1)
    }
    data <- dplyr::mutate(data, starttracking.time = NA,
                          Message_Inserted = 0)
  }

  ## Convert Message column into Stimulus column ####
  if ("Message" %in% colnames(data)) {
    data <- dplyr::group_by(data, Trial)
    data <- dplyr::mutate(data, Stimulus = zoo::na.locf(Message, na.rm = FALSE))
    data <- dplyr::ungroup(data)
    data <- dplyr::filter(data, Message_Inserted == 0)
  }
  ###################################################

  ## Include extra columns and remove trials ####
  data <- dplyr::select(data, everything(), tidyselect::any_of(include_col))
  data <- dplyr::select(data, -starttracking.time, -Message_Inserted)

  if (!is.null(trial_exclude)) {
    data <- dplyr::filter(data, !(Trial %in% trial_exclude))
  }
  data <- dplyr::filter(data, !is.na(Trial))
  data <- dplyr::mutate(data, Time = round(Time))
  data <- dplyr::relocate(data, Trial, .before = "Time")
  if (!is.null(px_to_mm_conversion)) {
    if ("Pupil_Diameter.px" %in% colnames(data)) {
      data <- dplyr::mutate(data,
                            Pupil_Diameter.mm =
                              Pupil_Diameter.px * px_to_mm_conversion)
    }
    if ("L_Pupil_Diameter.px" %in% colnames(data)) {
      data <- dplyr::mutate(data,
                            L_Pupil_Diameter.mm =
                              L_Pupil_Diameter.px * px_to_mm_conversion,
                            R_Pupil_Diameter.mm =
                              R_Pupil_Diameter.px * px_to_mm_conversion)
    }
  }
  #################################################
  data <- dplyr::as_tibble(data)
  return(data)
}
