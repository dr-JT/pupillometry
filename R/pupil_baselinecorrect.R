#' Baseline correction
#'
#' Apply a subtractive or divisive baseline correction to pupil data. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Adds a `Pupil_Diameter_bc` column to the data.
#'
#' @section Baseline correction:
#'
#' Baseline correction is calculated based on the median pupil size during
#' a defined baseline period. That baseline period is defined with the
#'
#' 1) `bc_onset_message` argument that specifies a message string that is sent
#' to the eye tracker at onset of the segment to be baseline corrected. The
#' values in the Stimulus column can be used here. Multiple values can be
#' specified if multiple segments need to be baseline corrected.
#'
#' AND
#'
#' 2) `baseline_duration` argument that specifies the duration of the baseline
#' period, before `bc_onset_message`, to use in calculating the median baseline
#' pupil size.
#'
#' Either "subtractive" or "divisive" baseline correction can be applied.
#'
#' @param x dataframe.
#' @param bc_onset_message Message string(s) that marks the onset of the
#'     segment to be baseline corrected. The values in the Stimulus column can
#'     be used here. Multiple values can be specified if multiple segments
#'     need to be baseline corrected.
#' @param baseline_duration Duration of baseline period(s). Multiple values
#'     can be specified if multiple segments need to be baseline corrected with
#'     different baseline durations. default: 200
#' @param type Do you want to use "subtractive" or "divisive"
#'     baseline correction? default: "subtractive"
#' @param match Is the message string an "exact" match or a "pattern" match?
#' @param no_pretrial The design of the task did not include a pretrial period
#'     to use for baseline correction. Therefore, use the end of the previous
#'     trial as the baseline period. default: FALSE
#' @param bc_onset.message deprecated. see bc_onset_message
#' @param pre.duration deprecated. see baseline_duration.
#' @import data.table
#' @export
#'

pupil_baselinecorrect <- function(x, bc_onset_message = "",
                                  baseline_duration = 200, type = "subtractive",
                                  match = "exact", no_pretrial = FALSE,
                                  bc_onset.message = NULL,
                                  pre.duration = NULL) {

  if (!is.null(bc_onset.message)) {
    bc_onset_message <- bc_onset.message
  }
  if (!is.null(pre.duration)) {
    baseline_duration <- pre.duration
  }

  x <- dplyr::as_tibble(x)

  if (no_pretrial == FALSE) {
    #### Setup baseline timing variables ####
    x <- dplyr::group_by(x, Trial, Stimulus)
    x <- dplyr::mutate(x, onset.time = min(Time, na.rm = TRUE))
    x <- dplyr::group_by(x, Trial)
    x <- dplyr::mutate(x, PreTarget = 0, Target = 0,
                       pupil_z = )

    for (m in bc_onset_message) {
      n <- match(m, bc_onset_message)
      if (match == "exact") {
        x <- dplyr::mutate(x,
                           bconset.time =
                             ifelse(Stimulus == m, onset.time, as.numeric(NA)))
      } else if (match == "pattern") {
        x <- dplyr::mutate(x,
                           bconset.time =
                             ifelse(stringr::str_detect(Stimulus, m),
                                    onset.time, as.numeric(NA)))
      }
      x <- dplyr::mutate(x,
                         min_time = min(bconset.time, na.rm = TRUE),
                         bconset.time = ifelse(is.na(bconset.time) |
                                                 bconset.time != min_time,
                                               as.numeric(NA), bconset.time),
                         bconset.time = zoo::na.locf(bconset.time, na.rm = FALSE),
                         bconset.time = zoo::na.locf(bconset.time, na.rm = FALSE,
                                                     fromLast = TRUE),
                         bconset.time = ifelse(is.infinite(min_time),
                                               as.numeric(Inf), bconset.time),
                         PreTarget =
                           ifelse(Time >= (bconset.time - baseline_duration) &
                                    Time < bconset.time, n, PreTarget),
                         Target = ifelse(Time >= bconset.time, n, Target))
    }
    x <- dplyr::select(x, -bconset.time, -min_time, -onset.time)
    ########################################

    #### Define baseline correction function ####
    baseline_correct <- function(x, type) {
      x <- dplyr::mutate(x,
                         .by = Trial,
                         pupil_val_z = scale(pupil_val)[,1])
      x <- dplyr::group_by(x, Trial, PreTarget)
      x <- dplyr::mutate(x,
                         PreTarget.median = median(pupil_val, na.rm = TRUE),
                         PreTarget.median_z = median(pupil_val_z, na.rm = TRUE),
                         PreTarget.median = ifelse(is.na(PreTarget) |
                                                     PreTarget == 0,
                                                   as.numeric(NA), PreTarget.median),
                         PreTarget.median_z = ifelse(is.na(PreTarget) |
                                                       PreTarget == 0,
                                                     as.numeric(NA), PreTarget.median_z))
      x <- dplyr::group_by(x, Trial)
      x <- dplyr::mutate(x,
                         PreTarget.median =
                           zoo::na.locf(PreTarget.median, na.rm = FALSE),
                         PreTarget.median_z =
                           zoo::na.locf(PreTarget.median_z, na.rm = FALSE))
      x <- dplyr::mutate(x,
                         PreTarget.median =
                           ifelse(PreTarget > Target, as.numeric(NA), PreTarget.median),
                         PreTarget.median_z =
                           ifelse(PreTarget > Target, as.numeric(NA), PreTarget.median_z))
      x <- dplyr::mutate(x,
                         PreTarget.median =
                           zoo::na.locf(PreTarget.median, na.rm = FALSE),
                         PreTarget.median_z =
                           zoo::na.locf(PreTarget.median_z, na.rm = FALSE))
      if (type == "subtractive") {
        x <- dplyr::mutate(x,
                           pupil_val_bc = pupil_val - PreTarget.median,
                           pupil_val_z_bc = pupil_val_z - PreTarget.median_z)
      } else if (type == "divisive") {
        x <- dplyr::mutate(x,
                           pupil_val_bc = ((pupil_val - PreTarget.median) /
                                             PreTarget.median) * 100,
                           pupil_val_z_bc = ((pupil_val_z - PreTarget.median_z) /
                                               PreTarget.median_z) * 100)
      }

      x <- dplyr::ungroup(x)
      x <- dplyr::select(x, -PreTarget.median, -Target,
                         -PreTarget.median_z, -pupil_val_z)
      x <- dplyr::relocate(x, pupil_val_bc, .after = pupil_val)
      x <- dplyr::relocate(x, pupil_val_z_bc, .after = pupil_val_bc)
    }
    ############################################
  } else {
    x <- dplyr::mutate(x, .by = c(Trial, Stimulus),
                    onset.time = min(Time_EyeTracker, na.rm = TRUE),
                    PreTarget = NA)
    if (match == "exact") {
      x <- dplyr::mutate(x, .by = Trial,
                         bconset.time =
                           ifelse(Stimulus == bc_onset_message,
                                  onset.time, as.numeric(NA)))
    } else if (match == "pattern") {
      x <- dplyr::mutate(x, .by = Trial,
                         bconset.time =
                           ifelse(stringr::str_detect(Stimulus, bc_onset_message),
                                  onset.time, as.numeric(NA)))
    }
    x <- x |>
      dplyr::mutate(.by = Trial,
                    min_time = min(bconset.time, na.rm = TRUE),
                    bconset.time = ifelse(is.na(bconset.time) |
                                            bconset.time != min_time,
                                          as.numeric(NA), bconset.time)) |>
      dplyr::mutate(bconset.time = zoo::na.locf(bconset.time, na.rm = FALSE,
                                                fromLast = TRUE),
                    PreTarget =
                      ifelse(Time_EyeTracker >= (bconset.time - baseline_duration) &
                               Time_EyeTracker <= bconset.time, 1, PreTarget),
                    Trial_lead =
                      ifelse(PreTarget == 1 & Time_EyeTracker >= (bconset.time - baseline_duration),
                             Trial + 1, Trial),
                    Trial_lead = ifelse(Trial_lead == Trial + 1 & Time_EyeTracker <= min_time,
                                        Trial, Trial_lead))

    x <- dplyr::select(x, -min_time, -onset.time)

    baseline_correct <- function(x, type) {
      message("here0")
      x <- x |>
        dplyr::mutate(.by = Trial,
                      pupil_val_z = scale(pupil_val)[,1])
      message("here1")
      x <- x |>
        dplyr::mutate(.by = c(Trial_lead, PreTarget),
                      PreTarget.median = median(pupil_val, na.rm = TRUE),
                      PreTarget.median_z = median(pupil_val_z, na.rm = TRUE),
                      PreTarget.median = ifelse(Time_EyeTracker != bconset.time,
                                                as.numeric(NA), PreTarget.median),
                      PreTarget.median_z = ifelse(Time_EyeTracker != bconset.time,
                                                  as.numeric(NA), PreTarget.median_z))
      message("here2")
      x <- x |>
        dplyr::mutate(.by = Trial,
                      PreTarget.median = zoo::na.locf(PreTarget.median, na.rm = FALSE),
                      PreTarget.median_z = zoo::na.locf(PreTarget.median_z, na.rm = FALSE))
      message("here3")

      if (type == "subtractive") {
        message("pupil_val_z: ", length(!is.na(x$pupil_val_z)))
        message("PreTarget.median_z: ", length(!is.na(x$PreTarget.median_z)))
        x <- dplyr::mutate(x,
                           pupil_val_bc = pupil_val - PreTarget.median,
                           pupil_val_z_bc = pupil_val_z - PreTarget.median_z)
      } else if (type == "divisive") {
        x <- dplyr::mutate(x,
                           pupil_val_bc = ((pupil_val - PreTarget.median) /
                                             PreTarget.median) * 100,
                           pupil_val_z_bc = ((pupil_val_z - PreTarget.median_z) /
                                               PreTarget.median_z) * 100)
      }
      x <- dplyr::ungroup(x)
      x <- dplyr::select(x, -PreTarget.median, -bconset.time, -Trial_lead,
                         -PreTarget.median_z, -pupil_val_z)
      x <- dplyr::relocate(x, pupil_val_bc, .after = pupil_val)
      x <- dplyr::relocate(x, pupil_val_z_bc, .after = pupil_val_bc)
    }
  }

  x <- dplyr::as_tibble(x)
  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

    x <- dtplyr::lazy_dt(x)
    x <- baseline_correct(x, type)
    x <- dplyr::as_tibble(x)

    colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
    colnames(x)[which(colnames(x) == "pupil_val_bc")] <-
      stringr::str_replace(real_name, "Diameter.", "Diameter_bc.")
    colnames(x)[which(colnames(x) == "pupil_val_z_bc")] <-
      stringr::str_replace(real_name, "Diameter\\..*", "Diameter_bc.z")
  }

  x <- dplyr::select(x, -PreTarget)

  return(x)
}
