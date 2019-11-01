# pupillometry <img src = "man/figures/logo_small.png" align = "right" />

> An R Package to preprocess pupil data

The package contains various functions for different steps in the preprocessing pipeline, such as interpolation, smoothing, and baseline correction.

The pre-processing steps are based on what has commonly been used in the literature and influenced by sensible preprocessing methods suggested by

Mathôt, S., Fabius, J., Van Heusden, E., & Van der Stigchel, S. (2018). Safe and sensible preprocessing and baseline correction of pupil-size data. Behavior research methods, 50(1), 94-106.

The main goal of this package is to provide:

1) ease of use for pupillometry researchers to perform preprocessing steps

2) flexibility in choosing which preprocessing methods and parameters are used. 

## Install

```r
devtools::install_github("dr-JT/pupillometry")
```

## Eyetracker Support

The format and organization of the raw data file will depend on the type of Eyetracker used. The `pupil_read()` function imports the "messy" raw data file and its output is a "tidy" raw data file with standardized column names and value labels to be used by the other functions in this package. 

Currently, `pupil_read()` supports:

- SensoMotoric Instruments (SMI) eyetrackers: `eyetracker = "smi"`

    - RED250m
    
    - Eye glasses
    
- SR Research EyeLink100 eyetrackers: `eyetracker = "eyelink"`

- Support for other eye-trackers will be included in future updates.


## Usage

There are two general ways you can use this package to do preprocessing:

1) Build your own sequence of preprocessing functions

Documentation on using this method is provided in [Using the Preprocessing Functions](https://dr-jt.github.io/pupillometry/articles/Message%20Markers.html)

or

2) Use `pupil_preprocess()`

**`pupil_preprocess()` is a wrapper around the other functions to allow full preprocessing of pupil data using a single function.**
    
## `pupil_preprocess()`

Using this method, you will need to pass many arguments to the `pupil_preprocess()` function that specifies all the details and preprocessing options.

`pupil_preprocess()` will be performed on an entire `import.dir` directory of raw data files that match a certain `pattern` in their filename. The preprocessed data will be saved to a specified `output.dir` directory.

The overall workflow of `pupil_preprocess()` is:

1. **Read** in raw data files `pupil_read()`

    - If `tracking.file` is supplied will also add message markers to the data
    
2. Clean up raw data files and more

    - **Correlate** left and right pupil size (if both eyes were recorded from). `pupil_cor()`
    
    - **Select** either left or right pupil data (if both eyes were recorded from). `select_eye()`
    
    - Set **Timing** variable to be relative to onset of each trial. `set_timing()`
    
    - Set **Stimulus** variable from **Message Markers**. `set_stimuli()`
    
3. **De-blink** data. `pupil_deblink()`

4. **Smooth** (if specified). `pupil_smooth()`

5. **Interpolate** (if specified). `pupil_interpolate()`

6. **Baseline Correct** (if specified). `pupil_baselinecorrect()`

7. Remove trials with too much **Missing Data**. `pupil_missing()`

8. **Merges** files into a single merged file (if specified). `pupil_merge()`

A final preprocessed data file will be saved for every original raw data file.

If `output.steps == TRUE` a data file will be saved after steps 3, 4, and 5. Before saving the data file at each of these steps, the final two steps 6 and 7 are performed. This results in baseline corrected and missing data removed files before each major preprocessing step. This is obviously not necessary and so `output.steps = FALSE` is the default and will only save one final preprocessed data file per subject. 

### Message Markers

You will need to supply message markers to correctly preprocess your data. This image is a representation of what the message markers `start_tracking.message`, `trial_onset.message`, and `bc_onset.message` correspond to. For further detail see [Example Data Set](https://dr-jt.github.io/pupillometry/articles/Example%20Data%20Set.html) and [Message Markers](https://dr-jt.github.io/pupillometry/articles/Message%20Markers.html) Articles.

<img src="./reference/figures/message_markers_1.png" align = "center" />

### Script Template

You can copy and paste the following code into a script and use it as a template.

```r
## Preprocessing parameters

# File Import Information
import.dir <- "data/Raw"
pattern <- ".txt"
taskname <- "Attention_Task"
timing.file <- NULL

# Eyetrackers save the subject number information in different ways and is not
# always easy to obtain. For SMI Red250m eyetrackers this is extracted from the
# datafile name. For SR-Research Eyelink1000 eyetrackers this is extracted from 
# the column name with the subject ID. You need to identify a unique subj.prefix 
# pattern and subj.suffix pattern that surrounds the subject # in the 
# datafile name/subject ID column.

subj.prefix <- "n_"
subj.suffix <- "-"

# File Output Information
output.dir <- "data/Preprocessed"
output.steps <- FALSE
files.merge <- FALSE

# Eyetracker Information
eyetracker <- "smi"
hz <- 250
eye.use <- "left"
px_to_mm.conversion <- NULL

# Message Marker Information
start_tracking.message <- "StartTracking.bmp"
start_tracking.match <- "exact"
trial_onset.message <- "Cue" 
trial_onset.match <- "exact"
pre_trial.duration <- 1000
bc_onset.message <- "Stimulus"
bc_onset.match <- "exact"

# Preprocessing Options
deblink.extend <- 100
smooth <- "hann"
smooth.window <- 500
interpolate <- "linear"
interpolate.maxgap <- 750
method.first <- "smooth"
bc <- "subtractive"
pre_bc.duration <- 200
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
                 px_to_mm.conversion = px_to_mm.conversion, 
                 start_tracking.message = start_tracking.message, 
                 start_tracking.match = start_tracking.match,
                 trial_onset.message = trial_onset.message, 
                 trial_onset.match = trial_onset.match,
                 pre_trial.duration = pre_trial.duration, 
                 bc_onset.message = bc_onset.message, 
                 bc_onset.match = bc_onset.match, 
                 deblink.extend = deblink.extend, 
                 smooth = smooth, smooth.window = smooth.window, 
                 interpolate = interpolate, 
                 interpolate.maxgap = interpolate.maxgap, 
                 method.first = method.first, bc = bc, 
                 pre_bc.duration = pre_bc.duration, 
                 missing.allowed = missing.allowed, subset = subset, 
                 trial.exclude = trial.exclude)
```

For any of the parameters that do not apply to your data or preprocessing steps, then you can just set them to `NULL` (e.g. `timing.file <- NULL`)

## Articles

For more detailed information on how to use `pupil_preprocess()` and other functions see the **Articles** tab above. 

## Planned Updates

* Better error and warning messages

* Data preprocessing visualizations?

* hampel filter option (maybe?)

* Support for Tobii Eyetrackers

* Support for GazePoint Eyetrackers

## Citation

> Tsukahara, J.S. (2018). pupillometry: An R Package to Preprocess Pupil Data. Retrieved from https://dr-jt.github.io/pupillometry

