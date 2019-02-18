#' Preprocess pupil data using this single function
#'
#' This function will perform preprocessing on an entire folder of data files. It takes as input a specified folder path
#' and outputs the preprocessed data to a specified folder path
#' @param import Folder path to raw data files
#' @param pattern Pattern to look for in data files
#' @param taskname Name of task - to be used in naming pre-processed files
#' @param subj.prefix The unique pattern prefix (letter(s) and/or symbol(s)) that comes before the subject number in the data file
#' @param subj.suffix The unique pattern suffix (letter(s) or symbol(s)) that comes after the subject number in the data file
#' @param output Folder path to output preprocessed data to
#' @param gazedata.include Logical. Include columns for x and y coordinates of eye gaze? (Default: FALSE)
#' @param eyetracker Which eye-tracker was used to record data
#' @param hz At which frequency was pupil data sampled at? (only required for interpolation and smoothing)
#' @param eye.recorded Do you want to inclue the "left", "right', or "both" eyes?
#' @param eye.use Which eye to use? Left or right
#' @param startrecording.message Message used in SMI experiment to mark StartTracking inline
#' @param startrecording.match Should the message string be an "exact" match or a "pattern" match?
#' @param trialonset.message Message string that marks the start of a trial
#' @param trialonset.match Should the message string be an "exact" match or a "pattern" match?
#' @param pretrial.duration Duration of pre-trial baseline period in milliseconds
#' @param missing.allowed What proportion of missing data is allowed, on a trial-by-trial basis? (Default: 1)
#' @param interpolate What type of interpolation to use? linear or cubic-spline
#' @param interpolate.maxgap Maximum number of NAs to interpolate over. Anything gaps over this value will not be interpolated.
#' @param smooth The type of smoothing function to apply. hann or moving window average (mwa)
#' @param smooth.window Window size of smoothing function default is 5 milliseconds
#' @param method.first Should "smooth" or "interpolate" be applied first? (default: NULL)
#' @param bc Do you want to use "subtractive" or "divisive" baseline correction? (default: "subtractive")
#' @param baselineoffset.message Message string(s) that marks the offset of baseline period(s)
#' @param baselineoffset.match Message string(s) that marks the offset of baseline period(s)
#' @param bc.duration Duration baseline period(s) to use for correction
#' @param downsample.binlength Length of bins to average (default: NULL)
#' @param subset Which columns in the raw data output file do you want to keep
#' @param trial.exclude Specify if ther are any trials to exclude. Trial number
#' @keywords preprocess
#' @export
#' @examples
#'
#'
preprocess <- function(import = NULL, pattern = "*.txt", taskname = NULL, subj.prefix = NULL, subj.suffix = NULL,
                       output = NULL, gazedata.include = FALSE,
                       eyetracker = NULL, hz = NULL, eye.recorded = NULL, eye.use = NULL,
                       startrecording.message = "default",  startrecording.match = "exact",
                       trialonset.message = NULL, trialonset.match = "exact", pretrial.duration = NULL,
                       missing.allowed = 1, interpolate = NULL, interpolate.maxgap = Inf,
                       smooth = NULL, smooth.window = 5, method.first = NULL,
                       bc = NULL, bc.duration = NULL, baselineoffset.message = NULL, baselineoffset.match = "exact",
                       downsample.binlength = NULL,
                       subset = "default", trial.exclude = c()){

  if (is.null(output)){
    output <- export
  }

  ###############################
  #### ----- Functions ----- ####

  ## Save data and do baseline correction first if bc==TRUE
  saveData <- function(x, preprocessing.stage = ""){
    if (bc==TRUE){
      preprocessing <- paste(preprocessing.stage, "bc", sep = ".")
      x <- pupil_baselinecorrect(x, message = baselineoffset.message, match = baselineoffset.match,
                                 duration = bc.duration, type = bc)
      # Downsample?
      if (!is.null(downsample.binlength)){
        preprocessing <- paste(preprocessing, "ds", sep = ".")
        x <- pupil_downsample(x, bin.length = downsample.binlength, bc = bc)
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
        x <- pupil_downsample(x, bin.length = downsample.binlength, bc = bc)
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
                            subset = subset, trial.exclude = trial.exclude, gazedata.include = gazedata.include)
    ## Save tidy data file
    subj <- data$Subject[1]
    write.table(data, file = paste(output, "/", taskname, "_", subj, "_PupilData.txt", sep = ""),
                sep = "\t", row.names = FALSE, quote = FALSE)
    ###########################################

    #### ----- Preprocessing procedures ----- ####

    ## Select eyes and filter out trials with too much missing data
    data <- pupil_eye(data, eye.recorded = eye.recorded, eye.use = eye.use, gazedata.include = gazedata.include)

    ## First of all, remove data during blinks and create columns of how much missing data each trial has. pupil_missing()
    data <- pupil_missing(data, missing.allowed = missing.allowed)

    if (nrow(data)==0){
      next
    }

    ## Creates a column that specifies the current stimulus (based on Messages in the data)
    data <- set_stimuli(data)
    ## Sets the Timing column relative to the onset of each trial
    data <- set_timing(data, trialonset.message = trialonset.message, match = trialonset.match,
                       ms.conversion = ms.conversion, pretrial.duration = pretrial.duration)

    if (gazedata.include==TRUE){
      gazedata <- dplyr::select(data, Subject, Head_Dist.cm, Time, Trial, Message, Event, Stimulus,
                                PreTrial, Gaze_Position.x, Gaze_Position.y)
      gazedata <- dplyr::mutate(gazedata,
                                Gaze_Position.x = ifelse(Event=="Blink"|is.na(Event)|Gaze_Position.x==0, NA, Gaze_Position.x),
                                Gaze_Position.y = ifelse(Event=="Blink"|is.na(Event)|Gaze_Position.y==0, NA, Gaze_Position.y))

      data <- dplyr::select(data, -Gaze_Position.x, -Gaze_Position.y)

      ## Save gazedata
      write.table(gazedata, file = paste(output, "/", taskname, "_", subj, "_EyeGazeData.txt", sep = ""),
                  sep = "\t", row.names = FALSE, quote = FALSE)
    }

    ## Save data at this stage
    saveData(data, preprocessing.stage = "naremoved")

    if (is.null(method.first)){
      ## Next, either interpolate or smooth
      if (interpolate==TRUE){
        data <- pupil_interpolate(data, type = interpolate, maxgap = interpolate.maxgap, hz = hz)
        ## Save data at this stage
        saveData(data, preprocessing.stage = "interpolated")
      } else if (smooth==TRUE){
        data <- pupil_smooth(data, type = smooth, window = smooth.window, hz = hz)
        ## Save data at this stage
        saveData(data, preprocessing.stage = "smoothed")
      }
    } else if (method.first == "interpolate"){
      ## Next, Interpolate data
      data <- pupil_interpolate(data, type = interpolate, maxgap = interpolate.maxgap, hz = hz)
      ## Save data at this stage
      saveData(data, preprocessing.stage = "interpolated")
      ## Next, Smooth data
      data <- pupil_smooth(data, type = smooth, window = smooth.window, hz = hz)
      ## Save data at this stage
      saveData(data, preprocessing.stage = "interpolated.smoothed")
    } else if (method.first == "smooth"){
      ## Next, Smooth data
      data <- pupil_smooth(data, type = smooth, window = smooth.window, hz = hz)
      ## Save data at this stage
      saveData(data, preprocessing.stage = "smoothed")
      ## Next, Interpolate data
      data <- pupil_interpolate(data, type = interpolate, maxgap = interpolate.maxgap, hz = hz)
      ## Save data at this stage
      saveData(data, preprocessing.stage = "smoothed.interpolated")
    }
    ##############################################
  }
}
