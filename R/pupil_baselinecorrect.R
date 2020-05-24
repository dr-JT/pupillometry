#' Apply baseline correction
#'
#' This function applies a baseline correction on the data
#' @param x dataframe
#' @param bc_onset.message Message string(s) that marks the offset of
#'     baseline period(s)
#' @param pre.duration Duration baseline period(s) to use for correction
#' @param type Do you want to use "subtractive" or "divisive"
#'     baseline correction? (default: "subtractive")
#' @param match Is the message string an "exact" match or a "pattern" match?
#' @export
#'

pupil_baselinecorrect <- function(x, bc_onset.message = "", pre.duration = 200,
                                  type = "subtractive", match = "exact"){

  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")
  real_name_bc <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                         "Pupil_Diameter_bc.mm", "Pupil_Diameter_bc.px")

  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  baselines.n <- length(bc_onset.message)
  x <- dplyr::group_by(x, Trial, Stimulus)
  x <- dplyr::mutate(x, onset.time = min(Time, na.rm = TRUE))
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, PreTarget = 0, Target = 0)

  for (m in bc_onset.message){
    n <- match(m, bc_onset.message)
    if (match == "exact"){
      x <- dplyr::mutate(x,
                         bconset.time = ifelse(Stimulus == m, onset.time, NA))
    } else if (match == "pattern"){
      x <- dplyr::mutate(x,
                         bconset.time = ifelse(stringr::str_detect(Stimulus, m),
                                               onset.time, NA))
    }
    x <- dplyr::mutate(x,
                       min = min(bconset.time, na.rm = TRUE),
                       bconset.time = ifelse(is.na(bconset.time) |
                                               bconset.time != min,
                                             NA, bconset.time),
                       bconset.time = zoo::na.locf(bconset.time, na.rm = FALSE),
                       bconset.time = zoo::na.locf(bconset.time, na.rm = FALSE,
                                                   fromLast = TRUE),
                       bconset.time = ifelse(is.infinite(min),
                                             Inf, bconset.time),
                       PreTarget = ifelse(Time >= (bconset.time-pre.duration) &
                                            Time < bconset.time, n, PreTarget),
                       Target = ifelse(Time >= bconset.time, n, Target))
  }
  x <- dplyr::group_by(x, Trial, PreTarget)
  x <- dplyr::mutate(x,
                     PreTarget.median = median(pupil_val, na.rm = TRUE),
                     PreTarget.median = ifelse(is.na(PreTarget) |
                                                 PreTarget == 0,
                                               NA, PreTarget.median))
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x,
                     PreTarget.median =
                       zoo::na.locf(PreTarget.median, na.rm = FALSE))
  x <- dplyr::mutate(x,
                     PreTarget.median =
                       ifelse(PreTarget > Target, NA, PreTarget.median))
  x <- dplyr::mutate(x,
                     PreTarget.median =
                       zoo::na.locf(PreTarget.median, na.rm = FALSE))
  if (type=="subtractive"){
    x <- dplyr::mutate(x, pupil_val_bc = pupil_val - PreTarget.median)
  } else if (type=="divisive"){
    x <- dplyr::mutate(x,
                       pupil_val_bc = ((pupil_val - PreTarget.median) /
                                         PreTarget.median) * 100)
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::mutate(x,
                     Trial_Phase = ifelse(PreTarget > 0, "PreTarget",
                                          ifelse(Target > 0, "Target",
                                                 Trial_Phase)))
  x <- dplyr::select(x, -PreTarget.median, -bconset.time, -min, -onset.time)
  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name
  colnames(x)[which(colnames(x) == "pupil_val_bc")] <- real_name_bc

  return(x)
}

