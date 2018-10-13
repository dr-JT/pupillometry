#' Pupil Data Function
#'
#' This function will perform preprocessing on an entire folder of data files. It takes as input a specified folder path
#' and outputs the preprocessed data to a specified folder path
#' @param import Folder path to raw data files
#' @param pattern Pattern to look for in data files
#' @param output Folder path to output preprocessed data to
#' @param export Folder path to export preprocessed data to
#' @param taskname Name of task - to be used in naming pre-processed files
#' @param eyetracker Which eye-tracker was used to record data
#' @param subj.prefix The unique pattern prefix (letter(s) and/or symbol(s)) that comes before the subject number in the data file
#' @param subj.suffix The unique pattern suffix (letter(s) or symbol(s)) that comes after the subject number in the data file
#' @param subset Which columns in the raw data output file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @param eye.recorded Do you want to inclue the "left", "right', or "both" eyes?
#' @param eye.use Which eye to use? Left or right
#' @param hz At which frequency was pupil data sampled at? (only required for interpolation and smoothing)
#' @param startrecording.message Message used in SMI experiment to mark StartTracking inline
#' @param startrecording.match Should the message string be an "exact" match or a "pattern" match?
#' @param trialonset.message Message string that marks the start of a trial
#' @param pretrial.duration Duration of pre-trial baseline period in milliseconds
#' @param velocity The velocity threshold for Blink detection
#' @param margin The margin before and after Blink onset and offset
#' @param missing.allowed What proportion of missing data is allowed, on a trial-by-trial basis? (Default: 1)
#' @param interpolate Do you want to do a linear interpolation over missing values?
#' @param interpolate.type What type of interpolation to use? linear or cubic-spline
#' @param interpolate.maxgap Maximum number of NAs to interpolate over. Anything gaps over this value will not be interpolated.
#' @param smooth Do you want to apply a moving average smoothing function?
#' @param smooth.type The type of smoothing function to apply. hann or moving window average (mwa)
#' @param smooth.window Window size of smoothing function default is 5 milliseconds
#' @param method.first Should "smooth" or "interpolate" be applied first? (default: NULL)
#' @param bc Logical. Do baseline correction?
#' @param baselineoffset.message Message string(s) that marks the offset of baseline period(s)
#' @param bc.duration Duration baseline period(s) to use for correction
#' @param bc.type Do you want to use "subtractive" or "divisive" baseline correction? (default: "subtractive")
#' @param downsample.binlength Length of bins to average (default: NULL)
#' @keywords preprocess
#' @export
#' @examples
#'
#'
preprocess <- function(import = "", pattern = "*.txt", output = NULL, export = "", taskname = "", eyetracker = "",
                       subj.prefix = NULL, subj.suffix = NULL, subset = "default", trial.exclude = c(),
                       eye.recorded = "", eye.use = "", hz = "",
                       startrecording.message = "default",  startrecording.match = "exact",
                       trialonset.message = "", pretrial.duration = "",
                       velocity = "", margin = "", missing.allowed = 1,
                       interpolate = FALSE, interpolate.type = "", interpolate.maxgap = Inf,
                       smooth = FALSE, smooth.type = "", smooth.window = 5, method.first = NULL,
                       bc = FALSE, baselineoffset.message = "", bc.duration = "", bc.type = "subtractive",
                       downsample.binlength = NULL){

  if (is.null(output)){
    output <- export
  }

  ###############################
  #### ----- Functions ----- ####

  ## Save data and do baseline correction first if bc==TRUE
  saveData <- function(x, preprocessing.stage = ""){
    if (bc==TRUE){
      preprocessing <- paste(preprocessing.stage, "bc", sep = ".")
      x <- pupil.baselinecorrect(x, baselineoffset.message = baselineoffset.message, bc.duration = bc.duration)
      if (preprocessing.stage==""){
        preprocessing <- "bc"
      } else {
        preprocessing <- paste(preprocessing.stage, "bc", sep = ".")
      }
      x <- pupil.baselinecorrect(x, baselineoffset.message = baselineoffset.message, bc.duration = bc.duration, bc.type = bc.type)
      # Downsample?
      if (!is.null(downsample.binlength)){
        preprocessing <- paste(preprocessing, "ds", sep = ".")
        x <- pupil.downsample(x, bin.length = downsample.binlength, bc = bc)
      }
      ## Save file
      subj <- x$Subject[1]
      SaveAs <- paste(output, "/", taskname, "_", subj, "_PupilData_", preprocessing, ".txt", sep = "")
      write.table(x, file = SaveAs, sep = "\t", row.names = FALSE, quote = FALSE)
    } else {
      preprocessing <- preprocessing.stage
      # Downsample?
      if (!is.null(downsample.binlength)){
        preprocessing <- paste(preprocessing, "ds", sep = ".")
        x <- pupil.downsample(x, bin.length = downsample.binlength, bc = bc)
      }
      ## Save file
      subj <- x$Subject[1]
      SaveAs <- paste(output, "/", taskname, "_", subj, "_PupilData_", preprocessing, ".txt", sep = "")
      write.table(x, file = SaveAs, sep = "\t", row.names = FALSE, quote = FALSE)
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


  ## Get list of data files to be pre-processed
  filelist <- list.files(path = import, pattern = pattern, full.names = TRUE)
  for (file in filelist){
    #### ----- Create Tidy Raw Data ----- ####

    ## Convert messy to tidy
    data <- tidy_eyetracker(file, eyetracker = eyetracker, startrecording.message = startrecording.message,
                            startrecording.match = startrecording.match, eye = eye.recorded,
                            subj.prefix = subj.prefix, subj.suffix = subj.suffix,
                            subset = subset, trial.exclude = trial.exclude)
    ## Save tidy data file
    subj <- data$Subject[1]
    write.table(data, file = paste(output, "/", taskname, "_", subj, "_RawPupilData.txt", sep = ""),
                sep = "\t", row.names = FALSE, quote = FALSE)
    ###########################################

    #### ----- Preprocessing procedures ----- ####

    ## Select eyes and filter out trials with too much missing data
    data <- pupil.eye(data, eye.recorded = eye.recorded, eye.use = eye.use)

    ## First of all, remove data during blinks and create columns of how much missing data each trial has. pupil.missing()
    data <- pupil.missing(data, missing.allowed = missing.allowed)

    ## Creates a column that specifies the current stimulus (based on Messages in the data)
    data <- set.stimuli(data)
    ## Sets the Timing column relative to the onset of each trial
    data <- set.timing(data, trialonset.message = trialonset.message,
                       ms.conversion = ms.conversion, pretrial.duration = pretrial.duration)

    ## Save data at this stage
    saveData(data, preprocessing.stage = "naremoved")

    if (is.null(method.first)){
      ## Next, either interpolate or smooth
      if (interpolate==TRUE){
        data <- pupil.interpolate(data, type = interpolate.type, maxgap = interpolate.maxgap, hz = hz)
        ## Save data at this stage
        saveData(data, preprocessing.stage = "interpolated")
      } else if (smooth==TRUE){
        data <- pupil.smooth(data, type = smooth.type, window = smooth.window, hz = hz)
        ## Save data at this stage
        saveData(data, preprocessing.stage = "smoothed")
      }
    } else if (method.first == "interpolate"){
      ## Next, Interpolate data
      data <- pupil.interpolate(data, type = interpolate.type, maxgap = interpolate.maxgap, hz = hz)
      ## Next, Smooth data
      data <- pupil.smooth(data, type = smooth.type, window = smooth.window, hz = hz)
      ## Save data at this stage
      saveData(data, preprocessing.stage = "interpolated.smoothed")
    } else if (method.first == "smooth"){
      ## Next, Smooth data
      data <- pupil.smooth(data, type = smooth.type, window = smooth.window, hz = hz)
      ## Next, Interpolate data
      data <- pupil.interpolate(data, type = interpolate.type, maxgap = interpolate.maxgap, hz = hz)
      ## Save data at this stage
      saveData(data, preprocessing.stage = "smoothed.interpolated")
    }
    ##############################################
  }
}
