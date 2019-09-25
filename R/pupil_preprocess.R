#' Preprocess pupil data using this single function
#'
#' This function will perform preprocessing on an entire folder of data files.
#' It takes as input a specified folder path
#' and outputs the preprocessed data to a specified folder path
#' @param import.dir Folder path to raw data files
#' @param pattern Pattern to look for in data files
#' @param taskname Name of task - to be used in naming pre-processed files
#' @param subj.prefix The unique pattern prefix (letter(s) and/or symbol(s))
#'     that comes before the subject number in the data file
#' @param subj.suffix The unique pattern suffix (letter(s) or symbol(s))
#'     that comes after the subject number in the data file
#' @param timing.file File location and name that contains timing
#'     information for message markers
#' @param output.dir Folder path to output preprocessed data to
#' @param output.steps Output files for each step in preprocessing?
#' @param eyetracker Which eye-tracker was used to record data
#' @param hz At which frequency was pupil data sampled at?
#'     (only required for interpolation and smoothing)
#' @param eye.use Which eye to use? Left or right
#' @param starttracking.message Message used in SMI experiment
#'     to mark StartTracking inline
#' @param starttracking.match Should the message string be an "exact"
#'     match or a "pattern" match?
#' @param trialonset.message Message string that marks the start of a trial
#' @param trialonset.match Should the message string be an "exact"
#'     match or a "pattern" match
#' @param deblink.extend How many milliseconds to extend blinks
#'     before and after blink detection
#' @param pretrial.duration Duration of pre-trial baseline period
#'     in milliseconds
#' @param missing.allowed What proportion of missing data is allowed,
#'     on a trial-by-trial basis? (Default: 1)
#' @param interpolate What type of interpolation to use? linear or cubic-spline
#' @param interpolate.maxgap Maximum number of NAs to interpolate over.
#'     Anything gaps over this value will not be interpolated.
#' @param smooth The type of smoothing function to apply.
#'     hann or moving window average (mwa)
#' @param smooth.window Window size of smoothing function
#'     default is 5 milliseconds
#' @param method.first Should "smooth" or "interpolate" be applied first?
#'     (default: NULL)
#' @param bc Do you want to use "subtractive" or "divisive"
#'     baseline correction? (default: "subtractive")
#' @param bconset.message Message string(s) that marks the offset of
#'     baseline period(s)
#' @param bconset.match Message string(s) that marks the offset of
#'     baseline period(s)
#' @param prebc.duration Duration baseline period(s) to use for correction
#' @param subset Which columns in the raw data output file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @param files.merge Do you want to create a single merge output file?
#' @keywords preprocess
#' @export
#'

pupil_preprocess <- function(import.dir = NULL, pattern = "*.txt",
                             taskname = NULL, subj.prefix = NULL,
                             subj.suffix = NULL, timing.file = NULL,
                             output.dir = NULL, output.steps = FALSE,
                             eyetracker = NULL, hz = NULL, eye.use = NULL,
                             starttracking.message = "default",
                             starttracking.match = "exact",
                             trialonset.message = NULL, trialonset.match = "exact",
                             deblink.extend = 0, pretrial.duration = NULL,
                             missing.allowed = 1, interpolate = NULL,
                             interpolate.maxgap = Inf, smooth = NULL,
                             smooth.window = 5, method.first = NULL, bc = NULL,
                             prebc.duration = NULL, bconset.message = NULL,
                             bconset.match = "exact", subset = "default",
                             trial.exclude = c(), files.merge = FALSE){

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
    if (!is.null(bc)){
      preprocessing <- paste(preprocessing.stage, "bc", sep = ".")
      x <- pupil_baselinecorrect(x, message = bconset.message,
                                 match = bconset.match,
                                 pre.duration = prebc.duration, type = bc)
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
                       starttracking.message = starttracking.message,
                       starttracking.match = starttracking.match,
                       subj.prefix = subj.prefix, subj.suffix = subj.suffix,
                       timing.file = timing.file,
                       subset = subset, trial.exclude = trial.exclude)

    #### ----- Preprocessing procedures ----- ####

    ## Correlate pupil data from left and right eyes
    left.recorded <- "L_Pupil_Diameter.mm" %in% colnames(data)
    right.recorded <- "R_Pupil_Diameter.mm" %in% colnames(data)
    if (left.recorded == TRUE & right.recorded == TRUE) {
      data <- pupil_cor(data)
    }

    ## Select eyes and filter out trials with too much missing data
    data <- select_eye(data, eye.use = eye.use)

    ## Sets the Timing column relative to the onset of each trial
    ms.conversion <- data$ms_conversion[1]
    data <- set_timing(data, trialonset.message = trialonset.message,
                       match = trialonset.match, ms.conversion = ms.conversion,
                       pretrial.duration = pretrial.duration)

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
