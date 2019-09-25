# pupillometry <img src = "man/figures/logo_small.png" align = "right" />

> An R Package to preprocess pupil data

The package contains various functions for different steps in the preprocessing pipeline, such as interpolation, smoothing, and baseline correction.

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

The format and organization of the raw data file will depend on the type of Eyetracker used. The `pupil_read()` function imports the "messy" raw data file and it's output is a "tidy" raw data file with standardized column and value labels to be used by the other functions. 

Currently, `pupil_read()` only supports data exported from BeGaze using an SMI eye-tracker or glasses. Support for other eye-trackers will be included in future updates.

## Usage

**`pupil_preprocess()` is a wrapper around the other functions to allow full preprocessing of pupil data using a single function.**

As such, you will need to pass many arguments to the `pupil_preprocess()` function that specifies all the details and preprocessing options.

`pupil_preprocess()` will be performed on an entire `import.dir` directory of raw data files that match a certain `pattern`. The preprocessed data will be saved to a specified `output.dir` directory.

The overall workflow of `pupil_preprocess()` is:

1. **Read** in raw data files `pupil_read()`

    - If `tracking.file` is supplied will also add message markers to the data
    
2. Clean up raw data files and more

    - **Correlate** left and right pupil size (if both eyes were recorded from). `pupil_cor()`
    
    - **Select** either left or right pupil data (if both eyes were recorded from). `select_eye()`
    
    - Set **Timing** variable to be relative to onset of each trial. `set_timing()`
    
3. **De-blink** data. `pupil_deblink()`

4. **Smooth** (if specified). `pupil_smooth()`

5. **Interpolate** (if specified). `pupil_interpolate()`

6. **Baseline Correct** (if specified). `pupil_baselinecorrect()`

7. Remove trials with too much **Missing Data**. `pupil_missing()`

8. **Merges** files into a single merged file (if specified). `pupil_merge()`

A final preprocessed data file will be saved for every original raw data file.

If `output.steps == TRUE` a data file will be saved after steps 3, 4, and 5. Before saving the data file at each of these steps, the final two steps 6 and 7 are performed. This results in baseline corrected and missing data removed files before each major preprocessing step. This is obviously not necessary and so `output.steps = FALSE` is the default and will only save one final preprocessed data file per subject. 

### Message Markers

You will need to supply message markers to correctly preprocess your data. This image is a representation of what the message markers `starttracking.message`, `trialonset.message`, and `bconset.message` correspond to. For further detail see [Example Data Set]() and [Message Markers]() Articles.

<img src="../reference/figures/message_markers_1.png" align = "center" />

### Script Template

You can copy and paste the following code into a script and use it as a template.

```r
## Preprocessing parameters

# File Import Information
import.dir <- "data/Raw"
pattern <- "*.txt"
taskname <- "Pitch_Discrimination"

# Eyetrackers save the subject number information in different ways and is not
# always easy to obtain. For SMI eyetrackers we need to extract it from the
# datafile name. You need to identify a unique subj.prefix pattern and 
# subj.suffix pattern that surrounds the subject # in the datafile name.

subj.prefix <- "n_"             ## For SMI eyetrackers
subj.suffix <- "-"              ## For SMI eyetrackers
timing.file <- NULL

# File Output Information
output.dir <- "data/Preprocessed"
output.steps <- FALSE
files.merge <- FALSE

# Eyetracker Information
eyetracker <- "smi"
hz <- 250
eye.use <- "left"

# Message Marker Information
starttracking.message <- "default"
starttracking.match <- "exact"
trialonset.message <- "Tone 1" 
trialonset.match <- "exact"
pretrial.duration <- 1000
bconset.message <- "Tone 1"
bconset.match <- "exact"

# Preprocessing Options
deblink.extend <- 100
smooth <- "hann"
smooth.window <- 500
interpolate <- "linear"
interpolate.maxgap <- 750
method.first <- "smooth"
bc <- "subtractive"
prebc.duration <- 200
missing.allowed <- .30

# Misc.
subset <- "default"
trial.exclude <- c()

############################

pupil_preprocess(import.dir = import.dir, pattern = pattern, taskname = taskname,
                 subj.prefix = subj.prefix, subj.suffix = subj.suffix, 
                 timing.file = timing.file, output.dir = output.dir, 
                 output.steps = output.steps, files.merge = files.merge, 
                 eyetracker = eyetracker, hz = hz, eye.use = eye.use, 
                 starttracking.message = starttracking.message, 
                 starttracking.match = starttracking.match,
                 trialonset.message = trialonset.message, 
                 trialonset.match = trialonset.match,
                 pretrial.duration = pretrial.duration, 
                 bconset.message = bconset.message, 
                 bconset.match = bconset.match, deblink.extend = deblink.extend,
                 smooth = smooth, smooth.window = smooth.window, 
                 interpolate = interpolate, 
                 interpolate.maxgap = interpolate.maxgap, 
                 method.first = method.first, bc = bc, 
                 prebc.duration = prebc.duration, 
                 missing.allowed = missing.allowed, subset = subset, 
                 trial.exclude = trial.exclude)
```

## Articles

For more detailed information on how to use `pupil_preprocess()` and other functions see the **Articles** tab above. 

## Planned Updates

* Add data preprocessing visualizations?

* Add hampel filter option (maybe?)

* Add greater support for SR Research EyeLink Eyetrackers

* Add support for Tobii Eyetrackers

* Add support for GazePoint Eyetrackers

## Citation

> Tsukahara, J.S. (2018). pupillometry: An R Package to Preprocess Pupil Data. Retrieved from https://dr-jt.github.io/pupillometry

