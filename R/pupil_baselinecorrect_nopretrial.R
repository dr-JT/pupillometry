#' Baseline correction (but when there is no pre-trial period)
#'
#' This function is used in pupil_baselinecorrect() when there is no pre-trial
#' period in the data file. That is, the trial starts at the stimulus onset.
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
#' @import data.table
#' @export
#'

pupil_baselinecorrect_nopretrial <- function(x, bc_onset_message = "",
                                  baseline_duration = 200, type = "subtractive",
                                  match = "exact") {

  x <- dplyr::as_tibble(x)

  k <- 0
  subj_data <- list()
  for (subj in unique(x$Subject)) {
    data_subj <- filter(x, Subject == subj)
    j <- 0
    block_data <- list()
    for (block in unique(data_subj$Block)) {
      data_block <- filter(data_subj, Block == block) |>
        mutate(trial_filter = min(Trial))
      i <- 0
      data_trial <- list()
      for (trial in unique(data_block$Trial)) {
        if (trial != data_block$trial_filter[1]) {
          i <- i + 1
          data_trial[[i]] <- data_block |>
            filter(Trial %in% c(trial - 1, trial)) |>
            mutate(Time = (row_number() * 20) - 20) |>
            mutate(.by = c(Trial, Stimulus),
                   bc_onsettime =
                     ifelse(Stimulus == "Stimulus_Onset" & Trial == trial,
                            first(Time) - 200, NA)) |>
            fill(bc_onsettime, .direction = "up") |>
            mutate(pre_stim = ifelse(Trial == trial - 1 & Time >= bc_onsettime, 1, 0),
                   Trial = ifelse(pre_stim == 1, Trial + 1, Trial),
                   Stimulus = ifelse(pre_stim == 1, "Baseline", Stimulus)) |>
            filter(Trial == trial) |>
            mutate(Pupil_z = scale(Pupil_Diameter.px)[,1]) |>
            mutate(.by = pre_stim,
                   baseline_pupil = median(Pupil_z, na.rm = TRUE),
                   baseline_pupil = ifelse(pre_stim == 1, baseline_pupil, NA)) |>
            fill(baseline_pupil, .direction = "down") |>
            mutate(Pupil_z_bc = Pupil_z - baseline_pupil) |>
            filter(Stimulus != "Baseline") |>
            mutate(Time = Time - min(Time))
        }
      }
      j <- j + 1
      block_data[[j]] <- bind_rows(data_trial)
    }
    k <- k + 1
    subj_data[[k]] <- bind_rows(block_data)
  }
  data_timeseries <- bind_rows(subj_data)

return(data_timeseries)
}

