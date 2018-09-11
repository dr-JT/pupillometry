# pupillometry
An R Package to preprocess pupil data

The package contains various functions for different steps in the preprocessing pipeline, such as interpolation or baseline correction.

The pre-processing steps are based on what has commonly been used in the literature and suggested by 

Mathôt, S., Fabius, J., Van Heusden, E., & Van der Stigchel, S. (2018). Safe and sensible preprocessing and baseline correction of pupil-size data. Behavior research methods, 50(1), 94-106.

## Eyetrackers

Currently the package only supports data exported from BeGaze using an SMI eye-tracker. Other eye-trackers will be included in future updates.

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
8. **Interpolate**
9. **Save** data at this stage*
10. **Smooth**
11. **Save** data at this stage*

\* When data is saved at these stages the following preprocessing steps may follow:
1. **Baseline Correct**
2. **Down Sample**

What this does is saves a data file at each stage of preprocessing. That way you have a baseline corrected/downsampled file at each stage of pre-processing. This can be useful if you originally specified a pre-processing step, such as smoothing, but then later decide you do not want to use that pre-processing method - you will already have the pre-processed (including baseline correction/down sampling) data file prior to that step. Or you may wish to compare how your results change depending on what pre-processing steps you perform.

### Example
```r
## Preprocessing parameters
import <- "./raw_data"
export <- "./preprocessed_data"
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

For preprocess() these are the message markers you need to know

### startrecording.message

Eye-tracking systems use different defaults for how it marks the start of recording on a trial. This is not necessarily when the trial begins - simply whent he eye-tracker starts recording.

The SMI default is : "# Message: StartTracking.bmp"

This parameter can be set to "default" to use the eyetracker default. If you are not using the default message then you need to specify the custom string you used.

### trialonset.message and pretrial.duration

trialonset.message is the message that marks the onset of a trial. If you included a fixation at the start of every trial, then the trial onset should correspond to the stimulus after the fixation. The fixation period will be treated as a pre-trial period.

The duration of the pre-trial period is set relative to the timing of the trialonset.message

For instance if pretrial.duration = 2000, then the 2000 milliseconds before trialonset.message will be marked as a pre-trial period. This duration typically corresponds to the duration of the fixation period. It should definitely not exceed the startrecording.message.

### baselineoffset.message

baselineoffset.message is the message that marks the offset of the baseline period and the start of the critical task period that needs to be baseline corrected.

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


