#' Preprocess pupil data using this single function
#'
#' This function will perform preprocessing on an entire folder of data files.
#' It takes as input a specified folder path and outputs the
#' preprocessed data to a specified folder path. For more details, see:
#' https://dr-jt.github.io/pupillometry/articles/pupil_preprocess.html
#' @param import_dir Folder path to raw data files
#' @param pattern Pattern to look for in data files (e.g. ".txt")
#' @param taskname Name of task - to be used in naming pre-processed files
#' @param subj_prefix The unique pattern prefix (letter(s) and/or symbol(s))
#'     that comes before the subject number in the data file. This is mostly
#'     for SMI eyetrackers that are not good at including subject number
#'     in the data and therefore the subject number needs to be determined
#'     and extracted from the file name. If the filename is
#'     "pitch_discrimination_19-1_001 Samples.txt" and the Subject number is
#'     19, then the subj_prefix = "n_" because that uniquely identifies a
#'     pattern that comes directly before the subject number in the file path.
#'     This can also be used for EyeLink or other eyetrackers, that include a
#'     subject number in a column in the datafile, to remove characters or
#'     numbers in front of a subject number.
#' @param subj_suffix The unique pattern suffix (letter(s) or symbol(s))
#'     that comes after the subject number in the data file. This is mostly
#'     for SMI eyetrackers that are not good at including subject number
#'     in the data and therefore the subject number needs to be determined
#'     and extracted from the file name. If the filename is
#'     "pitch_discrimination_19-1_001 Samples.txt" and the Subject number is
#'     19, then the subj_suffix = "-1" because that uniquely identifies a
#'     pattern that comes directly before the subject number in the file path.
#'     This can also be used for EyeLink or other eyetrackers, that include a
#'     subject number in a column in the datafile, to remove characters or
#'     numbers following (coming after) a subject number.
#' @param timing_file File location and name that contains timing
#'     information for message markers. This is only to be used if your
#'     data does not already have message markers embedded in the raw
#'     pupil data.
#' @param output_dir Folder path to output preprocessed data to
#' @param output_steps Output files for each step in preprocessing?
#'     This creates many more data files and therefore takes up more
#'     storage, but this can be useful if you want to analyze the data
#'     before and after a certain preprocessing method.
#' @param eyetracker The eye-tracker  used to record data. Options:
#'     "smi", "eyelink".
#' @param hz At which frequency was pupil data sampled at?
#'     (only required for interpolation and smoothing)
#' @param eye_use Which eye to use? "left" or "right"
#' @param px_to_mm.conversion The conversion factor to go from
#'     px pupil diameter to mm pupil diameter
#' @param start_tracking.message Message used to mark when eyetracking
#'     has started. For SMI eyetrackers, the default value is
#'     "StartTracking.bmp". For EyeLink eyetrackers, the default value is
#'     "TRIALID". For more information on how to use message markers see
#'     https://dr-jt.github.io/pupillometry/articles/message_markers.html
#' @param start_tracking.match Is the message string an "exact"
#'     match or a partial "pattern" match?
#' @param trial_onset.message Message string that marks the start of a trial.
#'     For more information on how to use message markers see
#'     https://dr-jt.github.io/pupillometry/articles/message_markers.html
#' @param trial_onset.match Is the message string an "exact"
#'     match or a partial "pattern" match
#' @param deblink_extend How many milliseconds to extend blinks
#'     before and after blink detection
#' @param pretrial.duration Duration of pre-trial baseline period
#'     in milliseconds
#' @param missing_allowed What proportion of missing data is allowed,
#'     on a trial-by-trial basis? If a trial exceeds this amount then it
#'     will be removed from further preprocessing.
#' @param interpolate What type of interpolation to use?
#'     "linear" or "cubic-spline"
#' @param interpolate.maxgap Maximum number of NAs to interpolate over.
#'     Any missing data gaps over this value will not be interpolated.
#' @param smooth The type of smoothing function to apply.
#'     "hann" or "mwa" (moving window average)
#' @param smooth.window Window size of smoothing function
#' @param method_first Should "smooth" or "interpolate" be applied first?
#'     It is highly suggested to apply smoothing before interpolation. See:
#'     https://dr-jt.github.io/pupillometry/articles/smooth_interpolate_first.html
#' @param bc Do you want to use "subtractive" or "divisive" baseline correction?
#' @param pre_bc.duration Duration of baseline period to use that comes
#'     before the baseline corrected period
#' @param bc_onset.message Message string(s) that marks the onset of the
#'     period to be baseline corrected.
#'     For more information on how to use message markers see
#'     https://dr-jt.github.io/pupillometry/articles/message_markers.html
#' @param bc_onset.match Is the message string an "exact"
#'     match or a partial "pattern" match
#' @param include_col Extra columns from the raw data file to include
#' @param trial_exclude Specify if there are any trials to exclude. Trial number
#' @param files_merge Do you want to create a single merge output file?
#'     TRUE or FALSE
#' @param starttracking.message See start_tracking.message
#' @param starttracking.match See start_tracking.match
#' @param trialonset.message See trial_onset.message
#' @param trialonset.match See trial_onset.match
#' @param pre_trial.duration See pretrial.duration
#' @param prebc.duration See pre_bc.duration
#' @param bconset.message See bc_onset.duration
#' @param bconset.match See bc_onset.match
#' @export
#'

pupil_preprocess <- function(import_dir = NULL, pattern = ".txt",
                             taskname = NULL, subj_prefix = NULL,
                             subj_suffix = NULL, timing_file = NULL,
                             output_dir = NULL, output_steps = FALSE,
                             eyetracker = NULL, hz = NULL, eye_use = NULL,
                             px_to_mm.conversion = NULL,
                             start_tracking.message = "default",
                             start_tracking.match = "exact",
                             trial_onset.message = NULL,
                             trial_onset.match = "exact",
                             deblink_extend = 0, pretrial.duration = NULL,
                             missing_allowed = 1, interpolate = NULL,
                             interpolate.maxgap = Inf, smooth = NULL,
                             smooth.window = 5, method_first = NULL, bc = NULL,
                             pre_bc.duration = NULL, bc_onset.message = NULL,
                             bc_onset.match = "exact", include_col = NULL,
                             trial_exclude = c(), files_merge = FALSE,
                             starttracking.message = NULL,
                             starttracking.match = NULL,
                             trialonset.message = NULL,
                             trialonset.match = NULL, pre_trial.duration = NULL,
                             prebc.duration = NULL, bconset.message = NULL,
                             bconset.match = NULL) {

  #### ----- Setup and Functions ----- ####
  if (!is.null(starttracking.message)) {
    start_tracking.message <- starttracking.message
  }
  if (!is.null(starttracking.match)) {
    start_tracking.match <- starttracking.match
  }
  if (!is.null(trialonset.message)) {
    trial_onset.message <- trialonset.message
  }
  if (!is.null(trialonset.match)) {
    trial_onset.match <- trialonset.match
  }
  if (!is.null(pretrial.duration)) {
    pretrial.duration <- pretrial.duration
  }
  if (!is.null(prebc.duration)) {
    pre_bc.duration <- prebc.duration
  }
  if (!is.null(bconset.message)) {
    bc_onset.message <- bconset.message
  }
  if (!is.null(bconset.match)) {
    bc_onset.match <- bconset.match
  }
  if (is.null(output_dir)) {
    output_dir <- export
  }

  save_file <- function(x, ext = "", to, task) {
    x <- select(x, Subject, Trial, Time, Stimulus,
                tidyselect::any_of(include_col),
                tidyselect::any_of("Pupil_Diameter.mm"),
                tidyselect::any_of("Pupil_Diameter.px"),
                tidyselect::any_of("Pupil_Diameter_bc.mm"),
                tidyselect::any_of("Pupil_Diameter_bc.px"),
                tidyselect::any_of("Eye_Event"),
                tidyselect::any_of("Trial_Phase"),
                tidyselect::any_of("Pupils.r"),
                tidyselect::any_of("Gaze_Position.x"),
                tidyselect::any_of("Gaze_Position.y"),
                tidyselect::any_of("Head_Distance.cm"))
    if (nrow(x) == 0) {
      message("No Trials with enough non-missing data. Subject: ", x$Subject[1])
    } else {
      output_file <- paste(to, "/", task, "_", x$Subject[1],
                           "_PupilData", ext, ".csv", sep = "")
      readr::write_csv(x, output_file)
      rm(output_file)
    }
  }

  ########################################

  filelist <- list.files(path = import_dir, pattern = pattern,
                         full.names = TRUE)
  for (file in filelist) {
    #### ----- Import Data File ----- ####
    data <- pupil_read(file, eyetracker = eyetracker,
                       start_tracking.message = start_tracking.message,
                       start_tracking.match = start_tracking.match,
                       subj_prefix = subj_prefix, subj_suffix = subj_suffix,
                       timing_file = timing_file,
                       include_col = include_col,
                       trial_exclude = trial_exclude,
                       quality_check_dir = output_dir)
    #####################################

    #### ----- Set timing column relative to onset of each trial ----- ####
    data <- set_timing(data, trial_onset.message = trial_onset.message,
                       match = trial_onset.match,
                       pretrial.duration = pretrial.duration)
    ######################################################################

    #### ----- Select eye to preprocess ----- ####
    left_recorded <- ifelse("L_Pupil_Diameter.mm" %in% colnames(data), TRUE,
                            ifelse("L_Pupil_Diameter.px" %in% colnames(data),
                                   TRUE, FALSE))
    right_recorded <- ifelse("R_Pupil_Diameter.mm" %in% colnames(data), TRUE,
                             ifelse("R_Pupil_Diameter.px" %in% colnames(data),
                                    TRUE, FALSE))
    if (left_recorded == TRUE & right_recorded == TRUE) {
      ## Correlate pupil data from left and right eyes
      data <- pupil_cor(data)
      ## Select eyes and filter out trials with too much missing data
      data <- select_eye(data, eye_use = eye_use)
    }
    #############################################

    #### ----- Save to file ----- ####
    step <- ""
    if (output_steps == TRUE) {
      save_file(data, ext = step, to = output_dir, task = taskname)
    }
    #################################

    ## ----- Deblink ----- ####
    data <- pupil_deblink(data, extend = deblink_extend)

    step <- paste(step, "_deblink", sep = "")
    ext <- step
    if (output_steps == TRUE) {
      data_save <- data
      if (!is.null(bc)) {
        data_save <- pupil_baselinecorrect(data_save,
                                           bc_onset.message = bc_onset.message,
                                           match = bc_onset.match,
                                           pre.duration = pre_bc.duration,
                                           type = bc)
        ext <- paste(step, "bc", sep = ".")
      }
      data_save <- pupil_missing(data_save, missing_allowed = missing_allowed)
      save_file(data_save, ext = ext, to = output_dir, task = taskname)
      rm(data_save)
    }
    ##########################

    ## ----- Smooth and interpolate ----- ####
    if (is.null(smooth) & !is.null(interpolate)) {
      data <- pupil_interpolate(data, type = interpolate,
                                maxgap = interpolate.maxgap,
                                hz = hz)
      step <- paste(step, "interpolate", sep = ".")
    }
    if (!is.null(smooth) & is.null(interpolate)) {
      data <- pupil_smooth(data, type = smooth,
                           window = smooth.window, hz = hz)
      step <- paste(step, "smooth", sep = ".")
    }
    if (!is.null(smooth) & !is.null(interpolate) & method_first == "smooth") {
      data <- pupil_smooth(data, type = smooth,
                           window = smooth.window, hz = hz)

      step <- paste(step, "smooth", sep = ".")
      ext <- step
      if (output_steps == TRUE) {
        data_save <- data
        if (!is.null(bc)) {
          data_save <- pupil_baselinecorrect(data_save,
                                             bc_onset.message = bc_onset.message,
                                             match = bc_onset.match,
                                             pre.duration = pre_bc.duration,
                                             type = bc)
          ext <- paste(step, "bc", sep = ".")
        }
        data_save <- pupil_missing(data_save, missing_allowed = missing_allowed)
        save_file(data_save, ext = ext, to = output_dir, task = taskname)
        rm(data_save)
      }

      data <- pupil_interpolate(data, type = interpolate,
                                maxgap = interpolate.maxgap,
                                hz = hz)
      step <- paste(step, "interpolate", sep = ".")
    }
    if (!is.null(smooth) & !is.null(interpolate) & method_first == "interpolate") {
      data <- pupil_interpolate(data, type = interpolate,
                                maxgap = interpolate.maxgap,
                                hz = hz)

      step <- paste(step, "interpolate", sep = ".")
      ext <- step
      if (output_steps == TRUE) {
        data_save <- data
        if (!is.null(bc)) {
          data_save <- pupil_baselinecorrect(data_save,
                                             bc_onset.message = bc_onset.message,
                                             match = bc_onset.match,
                                             pre.duration = pre_bc.duration,
                                             type = bc)

          ext <- paste(step, "bc", sep = ".")
        }
        data_save <- pupil_missing(data_save, missing_allowed = missing_allowed)
        save_file(data_save, ext = ext, to = output_dir, task = taskname)
        rm(data_save)
      }

      data <- pupil_smooth(data, type = smooth,
                           window = smooth.window, hz = hz)
      step <- paste(step, "smooth", sep = ".")
    }
    #########################################

    #### ----- Save Final Output ----- ####
    ext <- step
    if (!is.null(bc)) {
      data <- pupil_baselinecorrect(data,
                                    bc_onset.message = bc_onset.message,
                                    match = bc_onset.match,
                                    pre.duration = pre_bc.duration,
                                    type = bc)
      ext <- paste(step, "bc", sep = ".")
    }
    data <- pupil_missing(data, missing_allowed = missing_allowed)
    save_file(data, ext = ext, to = output_dir, task = taskname)
    ########################
  }

  #### ----- Print Quality Check ----- ####
  data_check <- readr::read_csv(paste(output_dir, "quality_check.csv",
                                      sep = "/"))
  message("---- Quality Check ---- \n",
          "Number of Trials in Data Files: ", unique(data_check$Trials),
          "\n -----------------------")
  #########################################

  #### ----- Merge Individual Files ----- ####
  if (files_merge == TRUE) {
    if (!is.null(bc)) {
      preprocessing <- paste(step, "bc", sep = ".")
    } else {
      preprocessing <- step
    }
    split <- stringr::str_split(output_dir, "/")
    combine <- split[[1]][1:(length(split[[1]])-1)]
    merged.dir <- paste(combine, sep = "/", collapse = "/")
    output_file <- paste(merged.dir, "/", taskname,
                         "_Pupil_Preprocessed", ".csv", sep = "")
    pupil_merge(path = output_dir,
                pattern = preprocessing,
                output_file = output_file)
  }
  ###########################################
}
