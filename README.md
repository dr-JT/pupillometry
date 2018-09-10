# pupillometry
An R Package to preprocess pupil data

The package contains various functions for different steps in the preprocessing pipeline, such as interpolation or baseline correction.

## preprocess()

preprocess() is a wrapper around the other functions to allow full preprocessing of pupil data using a single function.

As such, you will need to pass many arguments to the preprocess() function that specifies all the details and preprocessing options.

The overall workflow of preprocess() is:

For each eyetracking file in a specified directory

1. **Convert** "messy" raw data file to a standardized "tidy" raw data file
2. **Save** the "tidy" raw data file
3. Set and evaluate amount of **missing samples** per trial
4. Correlate left and right pupil size (If both eyes were recorded from)
5. Choose to keep only left or right pupil data (If both eyes were recorded from)
6. Set **Timing** variable to be relative to onset of each trial
7. **Save** data at this stage*
8. Choose whether to **interpolate** or not
9. **Save** data at this stage*
10. Choose whether to **smooth** or not
11. Choose whether to **baseline correct** or not
12. Choose whether to **downsample** the data or not
13. **Save** final pre-processing data

\* When data is saved at these stages; if baseline correction and/or downsampling is specified, then these will occur prior to saving. What this does is saves a data file at each stage of preprocessing. That way you have a baseline corrected/downsampled file at each stage of pre-processing. This can be useful if you originally specified a pre-processing step, such as smoothing, but then later decide you do not want to use that pre-processing method - you will already have the pre-processed (including baseline correction/down sampling) data file prior to that step. Or you may wish to compare how your results change depending on what pre-processing steps you perform.

### Example
```r
## Preprocessing parameters
import <- "./rawdata"
export <- "./preprocesseddata"
task <- "PVT"
pattern <- "*.txt"
eyetracker <- "smi"
eye.recorded <- "both"
eye.use <- "left"
hz <- 250
trialonset.message <- "Beep"
pretrial.duration <- 2000
interpolate <- TRUE
interpolate.type <- "cubic-spline"
interpolate.maxgap <- 300
smooth <- FALSE
bc <- TRUE
baselineoffset.message <- "Target"
bc.duration <- 200
downsample.binlength <- 20
############################

preprocess(import = import, pattern = pattern, export = export, taskname = task, eyetracker = eyetracker, 
           eye.recorded = eye.recorded, eye.use = eye.use, hz = hz,
           startrecording.message = "default", trialonset.message = trialonset.message, pretrial.duration = pretrial.duration,
           velocity = "", margin = "",
           interpolate = interpolate, interpolate.type = interpolate.type, interpolate.maxgap = interpolate.maxgap,
           smooth = smooth, smooth.type = "", smooth.window = "",
           bc = bc, baselineoffset.message = baselineoffset.message, bc.duration = bc.duration,
           downsample.binlength = downsample.binlength,
           subj.prefix = "default", subset = "default", trial.exclude = c())
```

## Message Markers

Your data file should conatain messages that mark the onset/offset of critical stimuli and events

For preprocessing your data these are the message markers you need to know

### startrecording.message

Eye-tracking systems might use different defaults for how it marks the onset of recoring on a trial. This is not necessarily when the trial begins - simply whent he eye-tracker starts recording.

The SMI default is : "# Message: StartTracking.bmp"

This parameter can be set to "default". If you are not using the default message then you need to specify a string.

### trialonset.message and pretrial.duration

trialonset.message is the message that marks the onset of a trial.

The pre-trial period is set relative to the timing of the trialonset.message

For instance if pretrial.duration = 2000, then the 2000 milliseconds before trialonset.message will be marked as a pre-trial period.

### baselineoffset.message

baselineoffset.message is the message that marks the offset of the baseline period

This will correspond to the message that is sent that marks the critical task period that needs to be baseline corrected

## pupil.interpolate()

Different interpolation options are available

### type

* linear
* cubic-spline

### maxgap

* The maximum gap (in milliseconds) of missing pupil data that will not be interpolated over. Any gaps larger than this value will not be interpolated.

## pupil.smooth()

Different smoothing options are available

### type

* hann
* mwa (moving window average)

### window

* hanning or moving average window size


