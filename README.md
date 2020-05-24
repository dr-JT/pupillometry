# pupillometry <img src = "man/figures/logo_small.png" align = "right" />

> An R Package to preprocess pupil data

The package contains various functions for different steps in the preprocessing pipeline, such as deblinking, interpolation, smoothing, and baseline correction.

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

**1)** Use `pupil_preprocess()`

Documentation on using this method is provided in [How to Use pupil_preprocess()](https://dr-jt.github.io/pupillometry/articles/pupil_preprocess.html)

**or**

**2)** Build your own sequence of pupillometry functions

Documentation on using this method is provided in [Build Your Own Sequence of Pupillometry Functions](https://dr-jt.github.io/pupillometry/articles/pupillometry_functions.html)

## Planned Updates

* Better error and warning messages

* Data preprocessing visualizations?

* hampel filter option (maybe?)

* Support for Tobii Eyetrackers

* Support for GazePoint Eyetrackers

## Support

If you are having difficulty getting the package to work or would like to make a feature request then do not hesitate to contact me: <jason.tsukahara@gatech.edu>

## Citation

> Tsukahara, J.S. (2020). pupillometry: An R Package to Preprocess Pupil Data. Retrieved from https://dr-jt.github.io/pupillometry

