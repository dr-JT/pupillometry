% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pupil_interpolate.R
\name{pupil_interpolate}
\alias{pupil_interpolate}
\title{Apply interpolation}
\usage{
pupil_interpolate(x, type = "cubic-spline", maxgap = Inf, hz = "")
}
\arguments{
\item{x}{dataframe}

\item{type}{What type of interpolation to use? "linear" or "cubic-spline"}

\item{maxgap}{Maximum number of NAs to interpolate over.
Any gaps over this value will not be interpolated.}

\item{hz}{The recording frequency (used to calculate window size)}
}
\description{
This function performs an interpolation over missing values.
The 'zoo' package is a dependency
}
