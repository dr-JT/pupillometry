#' Downsample a (time, value) series into fixed **time** windows (ms) and average
#'
#' @description
#' Aggregates a time series into **non-overlapping, time-based** windows of width
#' `window` (data points), computing the mean of `value_col` per window
#' (with NA tolerance) and assigning a representative timestamp to each window.
#'
#' - Windows are defined on the **time axis** using the column `time_col`.
#' - The first window starts at `min(df[[time_col]])` (call this `t0`), covering
#'   `[t0, t0 + window)`, then `[t0 + window, t0 + 2*window)`, and so on.
#' - Within each window, if the number of samples is **odd**, the timestamp is the
#'   exact middle sample’s time; if **even**, the timestamp is chosen via `even_time`.
#'
#' @param df A data frame (or tibble) containing at least `time_col` and `value_col`.
#' @param window Positive integer; the **time** window width (number of data points).
#'   Windows are formed by `floor((time - t0) / window)` with `t0 = min(time)`.
#' @param drop_incomplete Logical. If `TRUE`, drops the **trailing partial time bin**
#'   when the series ends before the right boundary of the last window.
#'   Default `FALSE`.
#' @param na_prop_max Proportion (0–1). The **maximum** fraction of missing values
#'   allowed in a window to still compute a mean. If the fraction of missing
#'   values exceeds `na_prop_max`, the downsampled value is set to `NA_real_`.
#'   Default is `0.5` (require at least half of values present).
#' @param time_col,value_col Character strings giving the column names in `df`
#'   for time (numeric, in ms) and value, respectively. Defaults:
#'   `"Time_EyeTracker"` and `"pupil_val"`.
#' @param even_time Strategy for the timestamp when the **number of samples in a
#'   bin is even**. One of `c("mean","left","right","start","end")`:
#'   - `"mean"`: numeric midpoint of the two central **sample times**;
#'   - `"left"` / `"right"`: the left- or right-middle **sample time**;
#'   - `"start"` / `"end"`: the **first**/**last** sample time in the bin.
#'   Default: `"mean"`.
#' @param snap_to_existing Logical. If `TRUE` and `even_time = "mean"`, snaps the
#'   numeric midpoint to the **nearest of the two central sample times**, avoiding
#'   fractional timestamps. Default `FALSE`.
#'
#' @details
#' **Time-based binning.** Bins are created purely by time boundaries of width
#' `window` starting from `t0 = min(time)`. This is robust to missing samples
#' and irregular sampling—any samples falling within the same time window are
#' aggregated together.
#'
#' **Trailing partial bin.** When `drop_incomplete = TRUE`, the final bin is
#' dropped if its right boundary `(t0 + (k+1) * window)` lies beyond the
#' maximum observed time. This is analogous to discarding a partial last interval.
#'
#' **Missing data rule.** Let `n_win` be the number of (non-NA/NA) samples in the
#' bin and `n_nonmiss` the count of non-NA values for `value_col`. The bin mean is
#' returned only if `n_nonmiss >= ceiling((1 - na_prop_max) * n_win)`, otherwise
#' `NA_real_`.
#'
#' **Representative time.** For **odd** counts, the middle sample’s time is used.
#' For **even** counts, `even_time` controls whether the timestamp is the numeric
#' midpoint of the two central sample times (`"mean"`), one of the central sample
#' times (`"left"`/`"right"`), or the first/last sample time in the bin
#' (`"start"`/`"end"`). Set `snap_to_existing = TRUE` with `"mean"` to avoid
#' fractional timestamps.
#'
#' **Column order.** The result returns exactly two columns, named as `time_col`
#' and `value_col`, sorted ascending by time.
#'
#' @return A tibble with two columns named as `time_col` and `value_col`, giving
#' the downsampled time and mean value for each time window.
#'
#' @examples
#' df <- tibble::tibble(
#'   Time_EyeTracker = c(0, 1, 2, 5, 7, 8, 10, 11),   # ms (irregular)
#'   pupil_val = c(1, 2, NA, 4, 5, 6, 7, NA)
#' )
#'
#' # 4-ms windows: [0,4), [4,8), [8,12)
#' window_mean(df, window = 4)
#'
#' # Even-sample bin rule: use midpoint timestamp (or snap to an existing sample)
#' window_mean(df, window = 4, even_time = "mean", snap_to_existing = TRUE)
#'
#' # Align to first/last sample timestamp in each bin
#' window_mean(df, window = 4, even_time = "start")
#'
#' # Drop trailing partial bin
#' window_mean(df, window = 4, drop_incomplete = TRUE)
#'
#' @seealso
#' \itemize{
#'   \item \code{slider::slide_index()} for rolling/overlapping windows with time awareness.
#'   \item \code{dplyr::reframe()} and \code{dplyr::summarise()} for custom per-bin summaries.
#' }
#'
#' @export
#' @importFrom dplyr mutate group_by filter summarise nth first last transmute
#' @importFrom dplyr arrange row_number n
#' @importFrom rlang .data sym
window_mean <- function(
    df,
    window,
    drop_incomplete   = FALSE,
    na_prop_max       = 0.5,
    time_col          = "Time_EyeTracker",
    value_col         = "pupil_val",
    even_time         = c("mean", "left", "right", "start", "end"),
    snap_to_existing  = FALSE
) {
  # ---- validate ----
  if (!is.numeric(window) || length(window) != 1L || is.na(window) || window <= 0)
    stop("`window` must be a positive number.", call. = FALSE)
  if (!all(c(time_col, value_col) %in% names(df)))
    stop("`time_col` and/or `value_col` not found in `df`.", call. = FALSE)
  if (!is.numeric(df[[time_col]]))
    stop("`time_col` must be numeric.", call. = FALSE)
  if (!is.numeric(na_prop_max) || na_prop_max < 0 || na_prop_max > 1)
    stop("`na_prop_max` must be in [0, 1].", call. = FALSE)
  if (!is.logical(drop_incomplete) || length(drop_incomplete) != 1L)
    stop("`drop_incomplete` must be TRUE/FALSE.", call. = FALSE)

  even_time <- match.arg(even_time)

  t0     <- min(df[[time_col]], na.rm = TRUE)
  t_max  <- max(df[[time_col]], na.rm = TRUE)
  w      <- as.numeric(window)

  res <- df |>
    dplyr::mutate(
      .g          = floor((.data[[time_col]] - t0) / w),
      .bin_start  = .g * w + t0,
      .bin_end    = .bin_start + w
    ) |>
    dplyr::group_by(.g, .bin_start, .bin_end) |>
    # Drop the trailing partial bin if requested
    (\(x) {
      if (!drop_incomplete) return(x)
      # Identify last bin by max(.g) and drop it if its right boundary exceeds t_max
      g_max <- max(x$.g, na.rm = TRUE)
      dplyr::filter(x, !(.data$.g == g_max & .data$.bin_end > t_max))
    })() |>
    dplyr::summarise(
      n_win     = dplyr::n(),
      n_nonmiss = sum(!is.na(.data[[value_col]])),
      mid_left  = (n_win + 1L) %/% 2L,   # floor middle index among samples
      mid_right = (n_win + 2L) %/% 2L,   # ceil middle index among samples
      time = {
        # sort by time inside bin to make "middle" well-defined
        ord  <- order(.data[[time_col]])
        tvec <- .data[[time_col]][ord]
        if (n_win == 0L) {
          NA_real_
        } else if (n_win %% 2L == 1L) {
          dplyr::nth(tvec, mid_right)
        } else {
          tl <- dplyr::nth(tvec, mid_left)
          tr <- dplyr::nth(tvec, mid_right)
          out <- switch(
            even_time,
            mean  = (tl + tr) / 2,
            left  = tl,
            right = tr,
            start = dplyr::first(tvec),
            end   = dplyr::last(tvec)
          )
          if (snap_to_existing && identical(even_time, "mean")) {
            if (abs(out - tl) <= abs(out - tr)) tl else tr
          } else {
            out
          }
        }
      },
      mean_value = if (n_win > 0L && n_nonmiss >= ceiling((1 - na_prop_max) * n_win)) {
        mean(.data[[value_col]], na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) |>
    dplyr::transmute(.time = .data[["time"]], .value = .data[["mean_value"]])

  # rename and sort
  res |>
    dplyr::rename(
      !!rlang::sym(time_col)  := .data[[".time"]],
      !!rlang::sym(value_col) := .data[[".value"]]
    ) |>
    dplyr::arrange(.data[[time_col]])
}
