% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pupil_preprocess.R
\name{pupil_preprocess}
\alias{pupil_preprocess}
\title{Preprocess pupil data using a single function}
\usage{
pupil_preprocess(
  import_dir = NULL,
  pattern = ".txt",
  taskname = NULL,
  subj_prefix = NULL,
  subj_suffix = NULL,
  timing_file = NULL,
  output_dir = NULL,
  output_steps = FALSE,
  eyetracker = NULL,
  hz = NULL,
  eye_use = NULL,
  px_to_mm.conversion = NULL,
  start_tracking.message = "default",
  start_tracking.match = "exact",
  trial_onset.message = NULL,
  trial_onset.match = "exact",
  deblink_extend = 0,
  pretrial.duration = NULL,
  missing_allowed = 1,
  interpolate = NULL,
  interpolate.maxgap = Inf,
  smooth = NULL,
  smooth.window = 5,
  method_first = NULL,
  bc = NULL,
  pre_bc.duration = NULL,
  bc_onset.message = NULL,
  bc_onset.match = "exact",
  include_col = NULL,
  trial_exclude = c(),
  files_merge = FALSE,
  starttracking.message = NULL,
  starttracking.match = NULL,
  trialonset.message = NULL,
  trialonset.match = NULL,
  pre_trial.duration = NULL,
  prebc.duration = NULL,
  bconset.message = NULL,
  bconset.match = NULL
)
}
\arguments{
\item{import_dir}{Folder path to raw data files}

\item{pattern}{Pattern to look for in data files (e.g. ".txt")}

\item{taskname}{Name of task - to be used in naming pre-processed files}

\item{subj_prefix}{The unique pattern prefix (letter(s) and/or symbol(s))
that comes before the subject number in the data file. This is mostly
for SMI eyetrackers that are not good at including subject number
in the data and therefore the subject number needs to be determined
and extracted from the file name. If the filename is
"pitch_discrimination_19-1_001 Samples.txt" and the Subject number is
19, then the subj_prefix = "n_" because that uniquely identifies a
pattern that comes directly before the subject number in the file path.
This can also be used for EyeLink or other eyetrackers, that include a
subject number in a column in the datafile, to remove characters or
numbers in front of a subject number.}

\item{subj_suffix}{The unique pattern suffix (letter(s) or symbol(s))
that comes after the subject number in the data file. This is mostly
for SMI eyetrackers that are not good at including subject number
in the data and therefore the subject number needs to be determined
and extracted from the file name. If the filename is
"pitch_discrimination_19-1_001 Samples.txt" and the Subject number is
19, then the subj_suffix = "-1" because that uniquely identifies a
pattern that comes directly before the subject number in the file path.
This can also be used for EyeLink or other eyetrackers, that include a
subject number in a column in the datafile, to remove characters or
numbers following (coming after) a subject number.}

\item{timing_file}{File location and name that contains timing
information for message markers. This is only to be used if your
data does not already have message markers embedded in the raw
pupil data.}

\item{output_dir}{Folder path to output preprocessed data to}

\item{output_steps}{Output files for each step in preprocessing?
This creates many more data files and therefore takes up more
storage, but this can be useful if you want to analyze the data
before and after a certain preprocessing method.}

\item{eyetracker}{The eye-tracker  used to record data. Options:
"smi", "eyelink".}

\item{hz}{At which frequency was pupil data sampled at?
(only required for interpolation and smoothing)}

\item{eye_use}{Which eye to use? "left" or "right"}

\item{px_to_mm.conversion}{The conversion factor to go from
px pupil diameter to mm pupil diameter}

\item{start_tracking.message}{Message used to mark when eyetracking
has started. For SMI eyetrackers, the default value is
"StartTracking.bmp". For EyeLink eyetrackers, the default value is
"TRIALID". For more information on how to use message markers see
https://dr-jt.github.io/pupillometry/articles/message_markers.html}

\item{start_tracking.match}{Is the message string an "exact"
match or a partial "pattern" match?}

\item{trial_onset.message}{Message string that marks the start of a trial.
For more information on how to use message markers see
https://dr-jt.github.io/pupillometry/articles/message_markers.html}

\item{trial_onset.match}{Is the message string an "exact"
match or a partial "pattern" match}

\item{deblink_extend}{How many milliseconds to extend blinks
before and after blink detection}

\item{pretrial.duration}{Duration of pre-trial baseline period
in milliseconds}

\item{missing_allowed}{What proportion of missing data is allowed,
on a trial-by-trial basis? If a trial exceeds this amount then it
will be removed from further preprocessing.}

\item{interpolate}{What type of interpolation to use?
"linear" or "cubic-spline"}

\item{interpolate.maxgap}{Maximum number of NAs to interpolate over.
Any missing data gaps over this value will not be interpolated.}

\item{smooth}{The type of smoothing function to apply.
"hann" or "mwa" (moving window average)}

\item{smooth.window}{Window size of smoothing function}

\item{method_first}{Should "smooth" or "interpolate" be applied first?
It is highly suggested to apply smoothing before interpolation. See:
https://dr-jt.github.io/pupillometry/articles/smooth_interpolate_first.html}

\item{bc}{Do you want to use "subtractive" or "divisive" baseline correction?}

\item{pre_bc.duration}{Duration of baseline period to use that comes
before the baseline corrected period}

\item{bc_onset.message}{Message string(s) that marks the onset of the
period to be baseline corrected.
For more information on how to use message markers see
https://dr-jt.github.io/pupillometry/articles/message_markers.html}

\item{bc_onset.match}{Is the message string an "exact"
match or a partial "pattern" match}

\item{include_col}{Extra columns from the raw data file to include}

\item{trial_exclude}{Specify if there are any trials to exclude. Trial number}

\item{files_merge}{Do you want to create a single merge output file?
TRUE or FALSE}

\item{starttracking.message}{See start_tracking.message}

\item{starttracking.match}{See start_tracking.match}

\item{trialonset.message}{See trial_onset.message}

\item{trialonset.match}{See trial_onset.match}

\item{pre_trial.duration}{See pretrial.duration}

\item{prebc.duration}{See pre_bc.duration}

\item{bconset.message}{See bc_onset.duration}

\item{bconset.match}{See bc_onset.match}
}
\description{
This function is largely outdated and will not undergo continued maintenance
or updates.
}
