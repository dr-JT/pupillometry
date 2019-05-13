# pupillometry 0.3.0

Updated: 05/12/2019

* Added `pupil_deblink()` with the parameter option `extend`

* Added option to not save data file at each step of preprocessing with paramter argument `preprocess(output.step = FALSE)`.

* Changed `tidy_eyetracker()` to `read_pupil()`

* Changed `pupil_eye()` to `select_eye()`

# pupillometry 0.2.1

Updated: 11/13/2018

* Changed function names to use "_" rather than "."

  "." functions can still be used but have been deprecated.
  
* Added `aoi_downsample()` function

# pupillometry 0.2.0

Updated: 10/13/2018

* Important update that fixes several bugs

* Smoothing function was also interpolating. This has been fixed. 

* Added `plot.comparison()` to visually compare different preprocessing parameters in a convenient way

* Added `pupil.eye()` to simplify selecting which eye to use in the final output.

* Added parameter `missing.allowed` to `preprocess()` and `pupil.missing()` to filter out trials with too much missing data.

* Added `gazedata.include()` parameter to import and output gaze position x and y coordinates from original raw data file

* Other bug fixes and minor changes

# pupillometry 0.1.1

Updated: 10/11/2018

* Added the option to do "divisive" baseline correction

    Paramaters added: `preprocess(bc.type = c("subtractive", "divisive"))` and `pupil.baselinecorrect(bc.type = c("subtractive", "divisive"))`
    
    Default: "subtractive"

* More flexibility to choose the ordering of interpolation and smoothing in `preprocess()`

    Paramaters added: `preprocess(method.first = c("smooth", "interpolate"))`
    
* More precise specifications to obtain Subject # from the file name.

    Paramaters added: `preprocess(subj.prefix = NULL, subj.suffix = NULL)` and `tidy_eyetracker(subj.prefix = NULL, subj.suffix = NULL)`
    
    If parameters are set as `NULL` (default), then the entire file name will be used as the Subject #. This is not ideal.


# pupillometry 0.1.0

Updated: 10/7/2018

## New Features

* Added `startrecording.match` parameter to preprocess() and tidy_eyetracker()
* Added `match` paramter to set.trial()

    This allows the user to specify whether the `startrecording.message` is an "exact" or "pattern" match. Default is "exact" match.
    
## Added Support

* Impproved support for EyeLink 1000 Plus eyetrackers
