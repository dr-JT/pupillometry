% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pupil_smooth.R
\name{pupil_smooth}
\alias{pupil_smooth}
\title{Apply smoothing/low frequency filter}
\usage{
pupil_smooth(x, type = "hann", window = 5, hz = "")
}
\arguments{
\item{x}{dataframe}

\item{type}{The type of smoothing function to apply. "hann" or "mwa"}

\item{window}{The size of the smoothing window (default = 5).
Value is in milliseconds}

\item{hz}{The recording frequency (used to calculate window size)}
}
\description{
This function performs a smoothing function over an array of x
The 'zoo' package is a dependency
}
