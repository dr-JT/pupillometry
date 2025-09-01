#' Downsample pupil data into fixed **time** windows (ms) and average
#'
#' @description
#' Aggregates pupil data into **non-overlapping** windows of width
#' `window` (milliseconds), computing the mean of pupil size per window.
#'
#' - Within each window, if the number of samples is **odd**, the timestamp is the
#'   exact middle sample’s time; if **even**, the timestamp is chosen via `even_window`.
#'
#' @param x A data frame imported with `pupil_read()`.
#' @param window Positive integer; the **time** window width in **milliseconds**.
#' @param na_allow Proportion (0–1). The **maximum** fraction of missing values
#'   allowed in a window to still compute a mean. If the fraction of missing
#'   values exceeds `na_allow`, the downsampled value is set to `NA_real_`.
#'   Default is `0.5` (require at least half of values present).
#' @param even_window Strategy for the timestamp when the **number of samples in a
#'   window is even**. One of `c("mean","left","right","start","end")`:
#'   - `"mean"`: numeric midpoint of the two central **sample times**;
#'   - `"left"` / `"right"`: the left- or right-middle **sample time**;
#'   - `"start"` / `"end"`: the **first**/**last** sample time in the bin.
#'   Default: `"left"`.
#'
#' @details
#' **Time-based binning.** Bins are created purely by time boundaries of width
#' `window` starting from `t0 = min(time)`. This is robust to missing samples
#' and irregular sampling—any samples falling within the same time window are
#' aggregated together.
#'
#' **Representative time.** For **odd** counts, the middle sample’s time is used.
#' For **even** counts, `even_time` controls whether the timestamp is the numeric
#' midpoint of the two central sample times (`"mean"`), one of the central sample
#' times (`"left"`/`"right"`), or the first/last sample time in the bin
#' (`"start"`/`"end"`). Set `snap_to_existing = TRUE` with `"mean"` to avoid
#' fractional timestamps.
#'
#'
#' @return A data frame with the downsampled time and mean pupil size for each time window.
#'
#'
#' @export
pupil_downsample <- function(
    x,
    window,
    na_allowed   = 0.5,
    even_window  = c("mean", "left", "right", "start", "end")
) {

  even_window <- match.arg(even_window)
  if (even_window == "mean") {
    snap_to <- TRUE
  } else {
    snap_to <- FALSE
  }

  col_order <- names(x)

  if (na_allowed != 1) {
    drop <- FALSE
  } else {
    drop <- TRUE
  }

  x <- pupillometry::pupil_upsample(x)

  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x_down <- pupillometry::window_mean(x, window = window,
                                        drop_incomplete = drop,
                                        na_prop_max = na_allowed,
                                        time_col = "Time_EyeTracker",
                                        value_col = "pupil_val",
                                        even_time = even_window,
                                        snap_to_existing = snap_to)
    x <- dplyr::select(x, -pupil_val)
    x <- dplyr::right_join(x, x_down, by = "Time_EyeTracker")

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  }

  x <- dplyr::select(x, col_order)
  return(x)
}
