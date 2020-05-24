# devtools::install_github("dr-JT/pupillometry", ref = "dev")

library(pupillometry)

## Preprocessing parameters

# File Import Information
import_dir <- "test/Raw"
pattern <- "NoMessageMarkers"
taskname <- "Pitch_Discrimination"

# Eyetrackers save the subject number information in different ways and is not
# always easy to obtain. For SMI eyetrackers we need to extract it from the
# datafile name. You need to identify a unique subj_prefix pattern and
# subj_suffix pattern that surrounds the subject # in the datafile name.

subj_prefix <- "n_"             ## For SMI eyetrackers
subj_suffix <- "-"              ## For SMI eyetrackers
timing_file <- "test/pitch_timing.csv"

# File Output Information
output_dir <- "test/Preprocessed"
output_steps <- FALSE
files_merge <- FALSE

# Eyetracker Information
eyetracker <- "smi"
hz <- 250
eye_use <- "left"

# Message Marker Information
start_tracking.message <- "Fixation"
start_tracking.match <- "exact"
trial_onset.message <- "Tone 1"
trial_onset.match <- "exact"
pretrial.duration <- 1000
bc_onset.message <- "Tone 1"
bc_onset.match <- "exact"

# Preprocessing Options
deblink_extend <- 100
smooth <- "hann"
smooth.window <- 500
interpolate <- "cubic-spline"
interpolate.maxgap <- Inf
method_first <- "smooth"
bc <- "subtractive"
pre_bc.duration <- 200
missing_allowed <- .30

# Misc.
subset <- "default"
trial_exclude <- c()

############################

pupil_preprocess(import_dir = import_dir, pattern = pattern, taskname = taskname,
                 subj_prefix = subj_prefix, subj_suffix = subj_suffix,
                 timing_file = timing_file, output_dir = output_dir,
                 output_steps = output_steps, files_merge = files_merge,
                 eyetracker = eyetracker, hz = hz, eye_use = eye_use,
                 start_tracking.message = start_tracking.message,
                 start_tracking.match = start_tracking.match,
                 trial_onset.message = trial_onset.message,
                 trial_onset.match = trial_onset.match,
                 pretrial.duration = pretrial.duration,
                 bc_onset.message = bc_onset.message, bc_onset.match = bc_onset.match,
                 deblink_extend = deblink_extend, smooth = smooth,
                 smooth.window = smooth.window, interpolate = interpolate,
                 interpolate.maxgap = interpolate.maxgap,
                 method_first = method_first, bc = bc,
                 pre_bc.duration = pre_bc.duration,
                 missing_allowed = missing_allowed, subset = subset,
                 trial_exclude = trial_exclude)
