# pupillometry 0.8.1

Updated: 10/11/2018

* Added the option to do "divisive" baseline correction

    Paramaters added: `preprocess(bc.type = c("subtractive", "divisive"))` and `pupil.baselinecorrect(bc.type = c("subtractive", "divisive"))`
    
    Default: "subtractive"

* More flexibility to choose the ordering of interpolation and smoothing in `preprocess()`

    Paramaters added: `preprocess(method.first = c("smooth", "interpolate"))`
    
* More precise specifications to obtain Subject # from the file name.

    Paramaters added: `preprocess(subj.prefix = NULL, subj.suffix = NULL)` and `tidy_eyetracker(subj.prefix = NULL, subj.suffix = NULL)`
    
    If parameters are set as `NULL` (default), then the entire file name will be used as the Subject #. This is not ideal.


# pupillometry 0.8.0

Updated: 10/7/2018

## New Features

* Added `startrecording.match` parameter to preprocess() and tidy_eyetracker()
* Added `match` paramter to set.trial()

    This allows the user to specify whether the `startrecording.message` is an "exact" or "pattern" match. Default is "exact" match.
    
## Added Support

* Impproved support for EyeLink 1000 Plus eyetrackers
