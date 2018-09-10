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
#' @param eye.use Which eye to use? Left or right
#' @param pretrial.duration Duration of pre-trial baseline period in milliseconds
#' @param bc.duration PreTarget duration to use for baseline correction
#' @param velocity The velocity threshold for Blink detection
#' @param margin The margin before and after Blink onset and offset
#' @param interpolate Do you want to do a linear interpolation over missing values?
#' @param interpolate.type What type of interpolation to use? linear or cubic-spline
#' @param interpolate.maxgap Maximum number of NAs to interpolate over. Anything gaps over this value will not be interpolated.
#' @param smooth Do you want to apply a moving average smoothing function?
#' @param smooth.type The type of smoothing function to apply. hann or moving window average (mwa)
#' @param smooth.window Window size of smoothing function default is 5 milliseconds
#' @param downsample.binlength Length of bins to average
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
                       eye.recorded = "", eye.use = "",
                       pretrial.duration = "", bc.duration = "",
                       velocity = "", margin = "",
                       interpolate = FALSE, interpolate.type = "", interpolate.maxgap = Inf,
                       smooth = FALSE, smooth.type = "", smooth.window = 5,
                       downsample.binlength = "", bc = FALSE,
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

  ## Save data and do baseline correction first if bc==TRUE
  saveData <- function(x, preprocessing.stage = ""){
    if (bc==TRUE){
      preprocessing <- paste(preprocessing.stage, "bc", sep = ".")
      x <- doBaselineCorrection(x)
      # Downsample?
      if (downsample.binlength>0){
        preprocessing <- paste(preprocessing, "ds", sep = ".")
        x <- downsample(x, bin.length = downsample.binlength)
      }
      ## Save file
      subj <- x$Subject[1]
      SaveAs <- paste(export, "/", taskname, "_", subj, "_PupilData_", preprocessing, ".txt", sep = "")
      write.table(x, file = SaveAs, sep = "\t", row.names = FALSE, quote = FALSE)
    } else {
      preprocessing <- preprocessing.stage
      # Downsample?
      if (downsample.binlength>0){
        preprocessing <- paste(preprocessing, "ds", sep = ".")
        x <- downsample(x, bin.length = downsample.binlength)
      }
      ## Save file
      subj <- x$Subject[1]
      SaveAs <- paste(export, "/", taskname, "_", subj, "_PupilData_", preprocessing, ".txt", sep = "")
      write.table(x, file = SaveAs, sep = "\t", row.names = FALSE, quote = FALSE)
    }
    return(x)
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


  ## Get list of data files to be pre-processed
  filelist <- list.files(path = import, pattern = pattern, full.names = TRUE)
  for (file in filelist){
    #### ----- Create Tidy Raw Data ----- ####

    ## Convert messy to tidy
    data <- tidy_eyetracker(file, eyetracker = eyetracker, trialmarker.message = trialmarker.message,
                            eye = eye.recorded, subj.prefix = subj.prefix, subset = subset,
                            trial.exclude = trial.exclude)
    ## Save tidy data file
    subj <- data$Subject[1]
    write.table(data, file = paste(export, "/", taskname, "_", subj, "_RawPupilData.txt", sep = ""),
                sep = "\t", row.names = FALSE, quote = FALSE)
    ###########################################

    #### ----- Preprocessing procedures ----- ####

    ## First of all, remove data during blinks and create columns of how much missing data each trial has. pupil.missing()
    data <- pupil.missing(data, eye.recorded = eye.recorded)

    ## Correlate and select Eyes
    if (eye.recorded == "both"){
      # correlate eyes
      data <- eyes.cor(data)
      # remove either left or right eye
      if (eye.use=="left"){
        data <- dplyr::mutate(data, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                              Missing.Total = L_Missing.Total,
                              Eye_Event = L_Event)
      } else if (eye.use=="right"){
        data <- dplyr::mutate(data, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                              Missing.Total = R_Missing.Total, Eye_Event = R_Event)
      }
      data <- dplyr::select(data, -L_Pupil_Diameter.mm, -R_Pupil_Diameter.mm,
                            -L_Missing.Total, -R_Missing.Total, -L_Event, -R_Event)
    }

    ## Creates a column that specifies the current stimulus (based on Messages in the data)
    data <- set.stimuli(data)
    ## Sets the Timing column relative to the onset of each trial
    data <- set.timing(data, start.trial = trialonset.message,
                       ms.conversion = ms.conversion, pretrial.duration = pretrial.duration)

    ## Save data at this stage
    saveData(data, preprocessing.stage = "")

    ## Next, Interpolate data
    if (interpolate==TRUE){
      data <- pupil.interpolate(data, type = interpolate.type, maxgap = interpolate.maxgap)
      ## Save data at this stage
      saveData(data, preprocessing.stage = "interpolated")
    }

    ## Next, Smooth data
    if (smooth==TRUE){
      data <- pupil.smooth(data, type = smooth.type, window = smooth.window)
      ## Save data at this stage
      if (interpolate==TRUE){
        saveData(data, preprocessing.stage = "interpolated.smoothed")
      } else {
        saveData(data, preprocessing.stage = "smoothed")
      }
    }
    ##############################################
  }
}
