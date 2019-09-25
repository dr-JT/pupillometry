# devtools::install_github("dr-JT/pupillometry", ref = "dev")

library(pupillometry)

## Preprocessing parameters

# File Import Information
import.dir <- "test/Raw"
pattern <- "NoMessageMarkers"
taskname <- "Pitch_Discrimination"

# Eyetrackers save the subject number information in different ways and is not
# always easy to obtain. For SMI eyetrackers we need to extract it from the
# datafile name. You need to identify a unique subj.prefix pattern and
# subj.suffix pattern that surrounds the subject # in the datafile name.

subj.prefix <- "n_"             ## For SMI eyetrackers
subj.suffix <- "-"              ## For SMI eyetrackers
timing.file <- "test/pitch_timing.csv"

# File Output Information
output.dir <- "test/Preprocessed"
output.steps <- FALSE
files.merge <- FALSE

# Eyetracker Information
eyetracker <- "smi"
hz <- 250
eye.use <- "left"

# Message Marker Information
starttracking.message <- "Fixation"
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
interpolate <- "cubic-spline"
interpolate.maxgap <- Inf
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
                 bconset.message = bconset.message, bconset.match = bconset.match,
                 deblink.extend = deblink.extend, smooth = smooth,
                 smooth.window = smooth.window, interpolate = interpolate,
                 interpolate.maxgap = interpolate.maxgap,
                 method.first = method.first, bc = bc,
                 prebc.duration = prebc.duration,
                 missing.allowed = missing.allowed, subset = subset,
                 trial.exclude = trial.exclude)
