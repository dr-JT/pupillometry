#' Pupil Data Function
#'
#' This function will perform preprocessing on an entire folder of data files. It takes as input a specified folder path
#' and outputs the preprocessed data to a specified folder path
#' @param import Folder path to raw data files
#' @param pattern Pattern to look for in data files
#' @param export Folder path to export preprocessed data to
#' @param taskname Name of task - to be used in naming pre-processed files
#' @param eyetracker Which eye-tracker was used to record data
#' @param trialmarker.message Message used in SMI experiment to mark StartTracking inline
#' @param trialonset.message Message string that marks the start of a trial
#' @param targetonset.message Message string that marks the target onset for baseline correction
#' @param eye.recorded Do you want to inclue the "left", "right', or "both" eyes?
#' @param eye.use Which method of using left and right eye data? Average, missing, left, or right.
#' @param pretrial.duration Duration of pre-trial baseline period in milliseconds
#' @param bc.duration PreTarget duration to use for baseline correction
#' @param velocity The velocity threshold for Blink detection
#' @param margin The margin before and after Blink onset and offset
#' @param interpolate Do you want to do a linear interpolation over missing values?
#' @param interpolate.type What type of interpolation to use? linear or cubic-spline
#' @param smooth Do you want to apply a moving average smoothing function?
#' @param smooth.type The type of smoothing function to apply. hann or moving window average (mwa)
#' @param smooth.window Window size of smoothing function default is 11 milliseconds
#' @param downsample.Hz The frequency you want to downsample to
#' @param subj.prefix The prefix that comes before the subject number in the data file (including "-")
#' @param subset Which columns in the raw data export file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @keywords preprocess
#' @export
#' @examples
#'
#'
preprocess <- function(import = "", pattern = "*.txt", export = "", taskname = "", eyetracker = "",
                       trialmarker.message = "default", trialonset.message = "", targetonset.message = "",
                       eye.recorded = "", eye.use = "", pretrial.duration = "", bc.duration = "",
                       velocity = "", margin = "",
                       interpolate = FALSE, interpolate.type = "", smooth = FALSE, smooth.type = "", smooth.window = 11,
                       downsample.Hz = "", bc = FALSE,
                       subj.prefix = "default", subset = "default", trial.exclude = c()){

  ###############################
  #### ----- Functions ----- ####

  ## Function to do baseline correction (allows for multiple baseline corrections in a trial)
  doBaselineCorrection <- function(x){
    for (i in 1:bc.iterations){
      x <- baselinecorrect(x, bc.duration = bc.duration, start.target = targetonset.message[i], iteration = i)
    }
    return(x)
  }

  ## Set of functions that are performed every time the data set are saved.
  readyToSave <- function(x){
    ## Method to use to combine data streams for two eyes. Method options are: average, missing (least missing data), left, right
    x <- eye.method(x, eye.recorded = eye.recorded, method = eye.use)
    ## Creates a column that specifies the current stimulus (based on Messages in the data)
    x <- set.stimuli(x)
    ## Sets the Timing column relative to the onset of each trial
    x <- set.timing(x, start.trial = trialonset.message,
                    ms.conversion = ms.conversion, pretrial.duration = pretrial.duration)
    return(x)
  }

  ## Save data and do baseline correction first if bc==TRUE
  saveData <- function(x, preprocessing.stage = ""){
    if (bc==TRUE){
      preprocessing <- paste(preprocessing.stage, "bc", sep = ".")
      x <- lapply(x, readyToSave)
      x <- lapply(x, doBaselineCorrection)
      ## Save file
      for (i in 1:length(x)){
        subj <- x[[i]]$Subject[1]
        SaveAs <- paste(export, "/", taskname, "_", subj, "_PupilData_", preprocessing, ".txt", sep = "")
        write.table(x[[i]], file = SaveAs, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    } else {
      x <- lapply(x, readyToSave)
      for (i in 1:length(x)){
        subj <- x[[i]]$Subject[1]
        SaveAs <- paste(export, "/", taskname, "_", subj, "_PupilData_", preprocessing, ".txt", sep = "")
        write.table(x[[i]], file = SaveAs, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    }
    return(x)
  }

  ## Sacve data at the end of pre-processing pipeline
  saveData.ds <- function(x, preprocessing.stage = ""){
    x <- lapply(x, downsample, Hz = downsample.Hz)
    ## Save file
    if (bc==TRUE){
      preprocessing <- paste(preprocessing.stage, "bc.ds", sep = ".")
    } else {
      preprocessing <- paste(preprocessing.stage, "ds", sep = ".")
    }
    for (i in 1:length(x)){
      subj <- x[[i]]$Subject[1]
      SaveAs <- paste(export, "/", taskname, "_", subj, "_PupilData_", preprocessing, ".txt", sep = "")
      write.table(x[[i]], file = SaveAs, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  }

  ###############################

  #### ----- Set Defaults ----- ####
  if (eyetracker=="smi") {
    ## SMI uses a timing variable that is 1000*ms
    ms.conversion <- 1000
  }

  if (eyetracker=="eyelink") {
    ## EyeLink uses a timing variable in ms
    ms.conversion <- 1
  }

  ## How many baseline corrections were specificed?
  bc.iterations <- length(targetonset.message)
  ##################################

  #### ----- Create Tidy Raw Data ----- ####
  ## Get list of data files to be pre-processed
  filelist <- list.files(path = import, pattern = pattern, full.names = TRUE)

  ## Convert messy to tidy
  data.list <- lapply(filelist, tidy_eyetracker, eyetracker = eyetracker, trialmarker.message = trialmarker.message,
                      eye = eye.recorded, subj.prefix = subj.prefix, subset = subset, trial.exclude = trial.exclude)

  ## Save tidy data file
  for (i in 1:length(data.list)){
    subj <- data.list[[i]]$Subject[1]
    write.table(data.list[[i]], file = paste(export, "/", taskname, "_", subj, "_RawPupilData.txt", sep = ""),
                sep = "\t", row.names = FALSE, quote = FALSE)
  }
  ##########################################

  #### ----- Preprocessing procedures ----- ####

  ## First of all, remove data during blinks and create columns of how much missing data each trial has. pupil.missing()
  data.list <- lapply(data.list, pupil.missing, eye.recorded = eye.recorded)
  ## Save data at this stage
  preprocessing.stage <- "naremoved"
  data.pre <- saveData(data.list, preprocessing.stage = preprocessing.stage)

  ## Next, Interpolate data
  if (interpolate==TRUE){
    data.list <- lapply(data.list, pupil.interpolate, type = interpolate.type, eye.recorded = eye.recorded)
    ## Save data at this stage
    preprocessing.stage <- "interpolated"
    data.pre <- saveData(data.list, preprocessing.stage = preprocessing.stage)
  }

  ## Next, Smooth data
  if (smooth==TRUE){
    data.list <- lapply(data.list, pupil.smooth, type = smooth.type, window = smooth.window, eye.recorded = eye.recorded)
    ## Save data at this stage
    if (interpolate==TRUE){
      preprocessing.stage = "interpolated.smoothed"
    } else {
      preprocessing.stage = "smoothed"
    }
    data.pre <- saveData(data.list, preprocessing.stage = preprocessing.stage)
  }

  ##############################################

  #### ----- Save Files ----- ####
  if (downsample.Hz>0){
    saveData.ds(data.pre, preprocessing.stage = preprocessing.stage)
  }
  ################################
}
