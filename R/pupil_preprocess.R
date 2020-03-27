#' Preprocess pupil data using this single function
#'
#' This function will perform preprocessing on an entire folder of data files.
#' It takes as input a specified folder path and outputs the
#' preprocessed data to a specified folder path. For more details, see:
#' https://dr-jt.github.io/pupillometry/articles/pupil_preprocess.html
#' @param import.dir Folder path to raw data files
#' @param pattern Pattern to look for in data files (e.g. ".txt")
#' @param taskname Name of task - to be used in naming pre-processed files
#' @param subj.prefix The unique pattern prefix (letter(s) and/or symbol(s))
#'     that comes before the subject number in the data file. This is mostly
#'     for SMI eyetrackers that are not good at including subject number
#'     in the data and therefore the subject number needs to be determined
#'     and extracted from the file name. If the filename is
#'     "pitch_discrimination_19-1_001 Samples.txt" and the Subject number is
#'     19, then the subj.prefix = "n_" because that uniquely identifies a
#'     pattern that comes directly before the subject number in the file path.
#'     This can also be used for EyeLink or other eyetrackers, that include a
#'     subject number in a column in the datafile, to remove characters or
#'     numbers in front of a subject number.
#' @param subj.suffix The unique pattern suffix (letter(s) or symbol(s))
#'     that comes after the subject number in the data file. This is mostly
#'     for SMI eyetrackers that are not good at including subject number
#'     in the data and therefore the subject number needs to be determined
#'     and extracted from the file name. If the filename is
#'     "pitch_discrimination_19-1_001 Samples.txt" and the Subject number is
#'     19, then the subj.suffix = "-1" because that uniquely identifies a
#'     pattern that comes directly before the subject number in the file path.
#'     This can also be used for EyeLink or other eyetrackers, that include a
#'     subject number in a column in the datafile, to remove characters or
#'     numbers following (coming after) a subject number.
#' @param timing.file File location and name that contains timing
#'     information for message markers. This is only to be used if your
#'     data does not already have message markers embedded in the raw
#'     pupil data.
#' @param output.dir Folder path to output preprocessed data to
#' @param output.steps Output files for each step in preprocessing?
#'     This creates many more data files and therefore takes up more
#'     storage, but this can be useful if you want to analyze the data
#'     before and after a certain preprocessing method.
#' @param eyetracker The eye-tracker  used to record data. Options:
#'     "smi", "eyelink".
#' @param hz At which frequency was pupil data sampled at?
#'     (only required for interpolation and smoothing)
#' @param eye.use Which eye to use? "left" or "right"
#' @param px_to_mm.conversion The conversion factor to go from
#'     px pupil diameter to mm pupil diameter
#' @param start_tracking.message Message used to mark when eyetracking
#'     has started. For SMI eyetrackers, the default value is
#'     "StartTracking.bmp". For EyeLink eyetrackers, the default value is
#'     "TRIALID". For more information on how to use message markers see
#'     https://dr-jt.github.io/pupillometry/articles/message_markers.html
#' @param start_tracking.match Should the message string be an "exact"
#'     match or a partial "pattern" match?
#' @param trial_onset.message Message string that marks the start of a trial.
#'     For more information on how to use message markers see
#'     https://dr-jt.github.io/pupillometry/articles/message_markers.html
#' @param trial_onset.match Should the message string be an "exact"
#'     match or a partial "pattern" match
#' @param deblink.extend How many milliseconds to extend blinks
#'     before and after blink detection
#' @param pre_trial.duration Duration of pre-trial baseline period
#'     in milliseconds
#' @param missing.allowed What proportion of missing data is allowed,
#'     on a trial-by-trial basis? If a trial exceeds this amount then it
#'     will be removed from further preprocessing.
#' @param interpolate What type of interpolation to use?
#'     "linear" or "cubic-spline"
#' @param interpolate.maxgap Maximum number of NAs to interpolate over.
#'     Any missing data gaps over this value will not be interpolated.
#' @param smooth The type of smoothing function to apply.
#'     "hann" or "mwa" (moving window average)
#' @param smooth.window Window size of smoothing function
#' @param method.first Should "smooth" or "interpolate" be applied first?
#'     It is highly suggested to apply smoothing before interpolation. See:
#'     https://dr-jt.github.io/pupillometry/articles/smooth_interpolate_first.html
#' @param bc Do you want to use "subtractive" or "divisive" baseline correction?
#' @param pre_bc.duration Duration of baseline period to use that comes
#'     before the baseline corrected period
#' @param bc_onset.message Message string(s) that marks the onset of the
#'     period to be baseline corrected.
#'     For more information on how to use message markers see
#'     https://dr-jt.github.io/pupillometry/articles/message_markers.html
#' @param bc_onset.match Should the message string be an "exact"
#'     match or a partial "pattern" match
#' @param subset Extra columns from the raw data file to keep
#' @param trial.exclude Specify if there are any trials to exclude. Trial number
#' @param files.merge Do you want to create a single merge output file?
#'     TRUE or FALSE
#' @param starttracking.message See start_tracking.message
#' @param starttracking.match See start_tracking.match
#' @param trialonset.message See trial_onset.message
#' @param trialonset.match See trial_onset.match
#' @param pretrial.duration See pre_trial.duration
#' @param prebc.duration See pre_bc.duration
#' @param bconset.message See bc_onset.duration
#' @param bconset.match See bc_onset.match
#' @export
#'

pupil_preprocess <- function(import.dir = NULL, pattern = "*.txt",
                             taskname = NULL, subj.prefix = NULL,
                             subj.suffix = NULL, timing.file = NULL,
                             output.dir = NULL, output.steps = FALSE,
                             eyetracker = NULL, hz = NULL, eye.use = NULL,
                             px_to_mm.conversion = NULL,
                             start_tracking.message = "default",
                             start_tracking.match = "exact",
                             trial_onset.message = NULL,
                             trial_onset.match = "exact",
                             deblink.extend = 0, pre_trial.duration = NULL,
                             missing.allowed = 1, interpolate = NULL,
                             interpolate.maxgap = Inf, smooth = NULL,
                             smooth.window = 5, method.first = NULL, bc = NULL,
                             pre_bc.duration = NULL, bc_onset.message = NULL,
                             bc_onset.match = "exact", subset = "default",
                             trial.exclude = c(), files.merge = FALSE,
                             starttracking.message = NULL,
                             starttracking.match = NULL,
                             trialonset.message = NULL,
                             trialonset.match = NULL, pretrial.duration = NULL,
                             prebc.duration = NULL, bconset.message = NULL,
                             bconset.match = NULL){

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
    pre_trial.duration <- pretrial.duration
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

  if (is.null(output.dir)){
    output.dir <- export
  }

  if (is.null(interpolate) & is.null(smooth)) {
    final_step <- "deblinked"
  } else if (is.null(interpolate)) {
    final_step <- "smoothed"
  } else if (is.null(smooth)) {
    final_step <- "interpolated"
  } else if (method.first == "interpolate") {
    final_step <- "interpolated.smoothed"
  } else if (method.first == "smooth") {
    final_step <- "smoothed.interpolated"
  }

  ###############################
  #### ----- Functions ----- ####

  ## Save data and do baseline correction first if bc==TRUE
  saveData <- function(x, preprocessing.stage = ""){
    if ("Trial.x" %in% colnames(x)) {
      x <- dplyr::select(x, -Trial.x)
    }
    if ("Message_Inserted" %in% colnames(x)) {
      x <- dplyr::select(x, -Message_Inserted)
    }

    if (!is.null(bc)){
      preprocessing <- paste(preprocessing.stage, "bc", sep = ".")
      x <- pupil_baselinecorrect(x, bc_onset.message = bc_onset.message,
                                 match = bc_onset.match,
                                 pre.duration = pre_bc.duration, type = bc)
      x <- pupil_missing(x, missing.allowed = missing.allowed)

      ## Save file
      subj <- x$Subject[1]
      if (nrow(x) == 0){
        message("No Trials with enough non-missing data. Subject: ", subj)
      } else {
        SaveAs <- paste(output.dir, "/", taskname, "_", subj, "_PupilData_",
                        preprocessing, ".csv", sep = "")
        readr::write_csv(x, SaveAs)
        rm(SaveAs)
      }
    } else {
      preprocessing <- preprocessing.stage
      x <- pupil_missing(x, missing.allowed = missing.allowed)

      ## Save file
      subj <- x$Subject[1]
      if (nrow(x) == 0){
        message("No Trials with enough non-missing data. Subject: ", subj)
      } else {
        SaveAs <- paste(output.dir, "/", taskname, "_", subj, "_PupilData_",
                        preprocessing, ".csv", sep = "")
        readr::write_csv(x, SaveAs)
        rm(SaveAs)
      }
    }
  }

  ###############################

  ## Get list of data files to be pre-processed
  filelist <- list.files(path = import.dir, pattern = pattern,
                         full.names = TRUE)
  for (file in filelist){
    #### ----- Create Tidy Raw Data ----- ####

    ## Convert messy to tidy
    data <- pupil_read(file, eyetracker = eyetracker,
                       start_tracking.message = start_tracking.message,
                       start_tracking.match = start_tracking.match,
                       subj.prefix = subj.prefix, subj.suffix = subj.suffix,
                       timing.file = timing.file,
                       subset = subset, trial.exclude = trial.exclude)

    #### ----- Preprocessing procedures ----- ####


    left.recorded <- "L_Pupil_Diameter.mm" %in% colnames(data) |
      "L_Pupil_Diameter.px" %in% colnames(data)
    right.recorded <- "R_Pupil_Diameter.mm" %in% colnames(data) |
      "R_Pupil_Diameter.px" %in% colnames(data)
    if (left.recorded == TRUE & right.recorded == TRUE) {
      ## Correlate pupil data from left and right eyes
      data <- pupil_cor(data)
      ## Select eyes and filter out trials with too much missing data
      data <- select_eye(data, eye.use = eye.use)
    }

    ## Sets the Timing column relative to the onset of each trial
    ms.conversion <- data$ms_conversion[1]
    data <- set_timing(data, trial_onset.message = trial_onset.message,
                       match = trial_onset.match, ms.conversion = ms.conversion,
                       pre_trial.duration = pre_trial.duration)

    ## Creates a column that specifies the current stimulus
    ## (based on Messages in the data)
    data <- set_stimuli(data)

    ## Add a function to add message markers?

    ## Save tidy data file
    if (output.steps == TRUE) {
      subj <- data$Subject[1]
      SaveAs <- paste(output.dir, "/", taskname, "_",
                      subj, "_PupilData.csv", sep = "")
      readr::write_csv(data, SaveAs)
      rm(SaveAs)
    }
    ###########################################

    data <- pupil_deblink(data, extend = deblink.extend)

    ## Save data at this step?
    step <- "deblinked"
    if (step == final_step){
      saveData(data, preprocessing.stage = step)
    } else if (output.steps == TRUE) {
      saveData(data, preprocessing.stage = step)
    }

    if (is.null(method.first)){
      ## Next, either interpolate or smooth
      if (!is.null(interpolate)){
        data <- pupil_interpolate(data, type = interpolate,
                                  maxgap = interpolate.maxgap, hz = hz)
        ## Save data at this stage
        step <- "interpolated"
        if (step == final_step){
          saveData(data, preprocessing.stage = step)
        } else if (output.steps == TRUE) {
          saveData(data, preprocessing.stage = step)
        }
      } else if (!is.null(smooth)){
        data <- pupil_smooth(data, type = smooth,
                             window = smooth.window, hz = hz)
        ## Save data at this stage
        step <- "smoothed"
        if (step == final_step){
          saveData(data, preprocessing.stage = step)
        } else if (output.steps == TRUE) {
          saveData(data, preprocessing.stage = step)
        }
      }
    } else if (method.first == "interpolate"){
      ## Next, Interpolate data
      data <- pupil_interpolate(data, type = interpolate,
                                maxgap = interpolate.maxgap, hz = hz)
      ## Save data at this stage
      if (output.steps == TRUE) {
        saveData(data, preprocessing.stage = "interpolated")
      }
      ## Next, Smooth data
      data <- pupil_smooth(data, type = smooth,
                           window = smooth.window, hz = hz)
      ## Save data at this stage
      step <- "interpolated.smoothed"
      if (step == final_step){
        saveData(data, preprocessing.stage = step)
      } else if (output.steps == TRUE) {
        saveData(data, preprocessing.stage = step)
      }
    } else if (method.first == "smooth"){
      ## Next, Smooth data
      data <- pupil_smooth(data, type = smooth,
                           window = smooth.window, hz = hz)
      ## Save data at this stage
      if (output.steps == TRUE) {
        saveData(data, preprocessing.stage = "smoothed")
      }
      ## Next, Interpolate data
      data <- pupil_interpolate(data, type = interpolate,
                                maxgap = interpolate.maxgap, hz = hz)
      ## Save data at this stage
      step <- "smoothed.interpolated"
      if (step == final_step){
        saveData(data, preprocessing.stage = step)
      } else if (output.steps == TRUE) {
        saveData(data, preprocessing.stage = step)
      }
    }
    ##############################################
  }
  if (files.merge == TRUE) {
    if (!is.null(bc)){
      preprocessing <- paste(step, "bc", sep = ".")
    } else {
      preprocessing <- step
    }
    split <- stringr::str_split(output.dir, "/")
    combine <- split[[1]][1:(length(split[[1]])-1)]
    merged.dir <- paste(combine, sep = "/", collapse = "/")
    SaveAs <- paste(merged.dir, "/", taskname,
                    "_Pupil_Preprocessed", ".csv", sep = "")
    pupil_merge(path = output.dir,
                pattern = preprocessing,
                output.file = SaveAs)
  }
}
