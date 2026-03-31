#' Apply an interpolation function on pupil data
#'
#' The purpose of interpolation is to replace gaps of missing values using
#' an estimation function. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Changes values in the column containing pupil data.
#'
#' @section Interpolation functions:
#'
#' This function applies either a linear or cubic-spline interpolation.
#'
#' The linear interpolation is the most straighforward to understand. It
#' basically draws a straight line from one end of the missing data to the
#' other end. Linear interpolation is adequate if the actual curve of the
#' pupil response is not of interest, such as when looking at summary statistics
#' (mean, standard deviation) over fairly large intervals.
#'
#' The cubic-spline interpolation will more accurately reflect the actual
#' curve of the pupil response over gaps of missing values. This is more
#' important to use if the actual curve of the pupil response is of interest,
#' such as when looking at peak or minimum values or when doing growth curve
#' analysis.
#'
#' It may not be a good idea to interpolate over too large of gaps with missing
#' values. For instance, you may not want to interpolate over a gap duration
#' that is equivalent to the length of your time-of-interest. You can specify
#' the maximum gap of missing values to interpolate over using the `maxgap`
#' argument (in milliseconds). If `maxgap` is used, then the sampling
#' frequency (`hz`) needs to be specified.
#'
#' @section Plot inspection:
#'
#' To inspect how the preprocesing step changed pupil size values,
#' use `plot = TRUE`.
#'
#' Warning: this will create a separate plot for every trial and therefore can
#' be time consuming and overwhelming. The plot argument is meant for initial
#' exploratory steps to determine the appropriate preprocessing parameters.
#'
#' @section Up-sample to 1000Hz:
#'
#' There are some advantages to up-sampling the data to a sampling frequency of
#' 1000Hz, and is even a recommended step in preprocessing by
#' Kiret & Sjak-Shie (2019).
#'
#' Up-sampling, should occur before smoothing and interpolation. In general,
#' it is safer to apply smoothing before interpolation (particularly if
#' cubic-spline interpolation is to be used). However, if up-sampling is to be
#' used, interpolation needs to occur first in order to fill in the missing
#' up-sampled values. The question, then, is how can we apply smoothing first
#' while still doing up-sampling?
#'
#' This is resolved in this package by first up-sampling with `pupil_upsample()`
#' and then smoothing `pupil_smooth()`. `pupil_upsample()` will not interpolate
#' the missing up-sampled values. Instead, a linear interpolation will be
#' done in`pupil_smooth()`, if `pupil_upsample()` was used prior, followed
#' by smoothing and then after smoothing, originally missing values (including
#' the missing up-sampled values and missing values due to blinks and other
#' reasons) will replace the linearly interpolated values (essentially undoing
#' the initial interpolation). After `pupil_smooth()`, interpolation can then
#' be applied to the up-sampled-smoothed data with `pupil_interpolate()`.
#'
#' This is all to say that, the intuitive workflow can still be used in which,
#' `pupil_upsample()` is used, followed by `pupil_smooth()`, followed by
#' `pupil_interpolate()`.
#'
#' Alternatively, to interpolate before smoothing, `pupil_upsample()` is used,
#' followed by `pupil_interpolate()`, followed by `pupil_smooth()`. The
#' difference being that, in this case, no interpolation and then replacing
#' the missing values back in the data is done in `pupil_smooth()` because
#' interpolation was performed first anyways.
#'
#' @param x dataframe.
#' @param type What type of interpolation to use? "linear" or "cubic-spline".
#' @param maxgap Maximum number of NAs to interpolate over.
#'     Any gaps over this value will not be interpolated.
#' @param hz The recording frequency (used to calculate maxgap).
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param plot_trial what trial(s) to plot. default = "all"
#' @param on_error How should errors be handled at the trial level?
#'     One of:
#'     \describe{
#'       \item{"missing"}{If an error occurs during processing, all pupil values
#'       for that trial are set to \code{NA}. This is the default and is the
#'       most conservative option, ensuring that failed trials are not treated
#'       as successfully processed.}
#'       \item{"original"}{If an error occurs, the original (unprocessed) pupil
#'       values for that trial are retained. A warning is still issued to
#'       indicate that processing failed for that trial.}
#'     }
#'     Errors are handled independently for each trial, so a failure in one
#'     trial will not interrupt processing of the remaining trials.
#' @import data.table
#' @export
#'

pupil_interpolate <- function(x, type = "cubic-spline",
                              maxgap = Inf, hz = "",
                              plot = FALSE, plot_trial = "all",
                              on_error = c("missing", "original")) {

  on_error <- match.arg(on_error)
  x_before <- dplyr::as_tibble(x)

  if ("UpSampled" %in% colnames(x)) {
    hz <- 1000
  }
  if (maxgap != Inf) {
    maxgap <- round(maxgap / (1000 / hz))
  }

  #### Define interpolate function ####
  interpolate <- function(x, type, maxgap) {
    interpolate_one_trial <- function(df_trial, trial_id) {
      df_trial <- dplyr::arrange(df_trial, Time)

      out <- tryCatch(
        {
          if (type == "cubic-spline") {
            df_trial <- dplyr::mutate(
              df_trial,
              index = ifelse(
                is.na(pupil_val),
                NA_real_,
                dplyr::row_number()
              ),
              index = zoo::na.approx(index, na.rm = FALSE),
              pupil_val = zoo::na.spline(
                pupil_val,
                na.rm = FALSE,
                x = index,
                maxgap = maxgap
              )
            ) |>
              dplyr::select(-index)
          }

          if (type == "linear") {
            df_trial <- dplyr::mutate(
              df_trial,
              pupil_val = zoo::na.approx(
                pupil_val,
                na.rm = FALSE,
                maxgap = maxgap
              )
            )
          }

          df_trial
        },
        error = function(e) {
          warning(
            paste0(
              "Interpolation failed for Trial ",
              trial_id,
              ". Returning ",
              ifelse(on_error == "missing", "NA", "original"),
              " pupil values."
            ),
            call. = FALSE
          )

          if (on_error == "missing") {
            df_trial |> dplyr::mutate(pupil_val = NA_real_)
          } else {
            df_trial
          }
        }
      )

      out
    }

    x |>
      dplyr::group_by(Trial) |>
      dplyr::group_modify(~ interpolate_one_trial(.x, .y$Trial[[1]])) |>
      dplyr::ungroup() |>
      dplyr::arrange(Trial, Time)
  }
  #####################################

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- interpolate(x, type, maxgap)
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  if (plot == TRUE) pupil_plot(x_before, x, trial = plot_trial,
                               sub_title =
                                 paste("pupil_interpolate(type = \"", type,
                                       "\", maxgap = ", maxgap,  ")",
                                       sep = ""))

  return(x)
}
