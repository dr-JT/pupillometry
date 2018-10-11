# pupillometry <img src = "man/figures/logo_small.png" align = "right" />

An R Package to preprocess pupil data

Preprocess pupil data with `preprocess()` - a wrapper around other functions that perform various steps in the preprocessing pipeline, such as interpolation or baseline correction.

The pre-processing steps are based on what has commonly been used in the literature and influenced by sensible preprocessing methods suggested by

Mathôt, S., Fabius, J., Van Heusden, E., & Van der Stigchel, S. (2018). Safe and sensible preprocessing and baseline correction of pupil-size data. Behavior research methods, 50(1), 94-106.

The main goal of this package is to provide:

1) ease of use for pupillometry researchers

2) flexibility in choosing which preprocessing methods and parameters are used. 

## Install

```r
devtools::install_github("dr-JT/pupillometry")
```

## Eyetracker Support

The format and organization of the raw data file will depend on the type of Eyetracker used. The `tidy_eyetracker()` function imports the "messy" raw data file and it's output is a "tidy" raw data file with standardized column and value labels to be used by the other functions. 

Currently, `tidy_eyetracker()` only supports data exported from BeGaze using an SMI eye-tracker. Support for other eye-trackers will be included in future updates.

## Usage

**`preprocess()` is a wrapper around the other functions to allow full preprocessing of pupil data using a single function.**

As such, you will need to pass many arguments to the `preprocess()` function that specifies all the details and preprocessing options.

`preprocess()` will be performed on an entire `import` directory of raw data files that match a certain `pattern`. At various stages of preprocessing the data will be saved to a specified `output` directory.

The overall workflow of `preprocess()` is:

1. **Import** "messy" raw data files and convert to a standardized "tidy" raw data format. `tidy_eyetracker()`
2. **Output** the standardized "tidy" raw data file.
3. Evaluate amount of **missing samples** per trial. `pupil.missing()`
4. Correlate left and right pupil size (if both eyes were recorded from). `pupil.cor()`
5. Keep either left or right pupil data (if both eyes were recorded from).
6. Set **Timing** variable to be relative to onset of each trial. `set.trial()` and `set.timing()`
7. **Output** data at this stage*
8. If specified, **Interpolate**. `pupil.interpolate()`
9. **Output** data at this stage*
10. If specified, **Smooth**. `pupil.smooth()`
11. **Output** data at this stage*

\* When data is outputed at these stages the following preprocessing steps will follow if they are specified:
1. **Baseline Correct** `pupil.baselinecorrect()`
2. **Down Sample** `pupil.downsample()`

What this does is saves a data file at each stage of preprocessing. That way you have a baseline corrected/downsampled file at each stage of pre-processing. This can be useful if you originally specified a pre-processing step, such as smoothing, but then later decide you do not want to use that pre-processing method - you will already have the pre-processed (including baseline correction/down sampling) data file prior to that step. Or you may wish to compare how your results change depending on what pre-processing steps you perform.

If there is a need, a future update might include an option to not save after each preprocessing step, but to only save at the very end.

### Example
```r
## Preprocessing parameters
import <- "./raw_data"
output <- "./preprocessed_data"
task <- "PVT"
pattern <- "*.txt"
eyetracker <- "smi"
eye.recorded <- "both"
eye.use <- "left"
hz <- 250
startrecording.message <- "default"
startrecording.match <- "exact"
trialonset.message <- "Beep"
pretrial.duration <- 2000
velocity <- ""
margin <- ""
interpolate <- TRUE
interpolate.type <- "cubic-spline"
interpolate.maxgap <- 300
smooth <- FALSE
smooth.type <- ""
smooth.window <- ""
bc <- TRUE
baselineoffset.message <- "Target"
bc.duration <- 200
downsample.binlength <- 20
subj.prefix <- "default"
subset <- "default"
trial.exclude <- c()
############################

preprocess(import = import, pattern = pattern, output = output, taskname = task, eyetracker = eyetracker, 
           eye.recorded = eye.recorded, eye.use = eye.use, hz = hz,
           startrecording.message = startrecording.message, startrecording.match = startrecording.match,
           trialonset.message = trialonset.message, pretrial.duration = pretrial.duration,
           velocity = velocity, margin = margin,
           interpolate = interpolate, interpolate.type = interpolate.type, interpolate.maxgap = interpolate.maxgap,
           smooth = smooth, smooth.type = smooth.type, smooth.window = smooth.window,
           bc = bc, baselineoffset.message = baselineoffset.message, bc.duration = bc.duration,
           downsample.binlength = downsample.binlength,
           subj.prefix = subj.prefix, subset = subset, trial.exclude = trial.exclude)
```

## Planned Updates

* Add divisive baseline correction option

* Add more flexibility to choose whether smoothing or interpolation should occur first

* Add option to not save data file at every stage of preprocessing (reduces number of files that get created if storage space is an issue)

* Add hampel filter option (maybe?)

* Add greater support for SR Research EyeLink Eyetrackers

* Add support for Tobii Eyetrackers

* Add support for GazePoint Eyetrackers
