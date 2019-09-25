#' Apply baseline correction
#'
#' This function applies a pre-trial baseline correction on the data
#' @param x dataframe
#' @param message Message string(s) that marks the offset of baseline period(s)
#' @param pre.duration Duration baseline period(s) to use for correction
#' @param type Do you want to use "subtractive" or "divisive" baseline correction? (default: "subtractive")
#' @param match Should the message string be an "exact" match or a "pattern" match?
#' @keywords baseline
#' @export
#'

pupil_baselinecorrect <- function(x, message = "", pre.duration = 200,
                                  type = "subtractive", match = "exact"){
  baselines.n <- length(message)
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, PreTarget=0, Target=0)
  for (m in message){
    n <- match(m, message)
    if (match=="exact"){
      x <- dplyr::mutate(x, baselineoffset.time = ifelse(Message==m, Time, NA))
    } else if (match=="pattern"){
      x <- dplyr::mutate(x, baselineoffset.time = ifelse(stringr::str_detect(Message, m), Time, NA))
    }
    x <- dplyr::mutate(x,
                       min = min(baselineoffset.time, na.rm = TRUE),
                       baselineoffset.time = ifelse(is.na(baselineoffset.time) | baselineoffset.time!=min,NA,baselineoffset.time),
                       baselineoffset.time = zoo::na.locf(baselineoffset.time, na.rm = FALSE),
                       baselineoffset.time = zoo::na.locf(baselineoffset.time, na.rm = FALSE, fromLast = TRUE),
                       baselineoffset.time = ifelse(is.infinite(min), Inf, baselineoffset.time),
                       PreTarget = ifelse(Time >= (baselineoffset.time-pre.duration) & Time < baselineoffset.time, n, PreTarget),
                       Target = ifelse(Time >= baselineoffset.time, n, Target))
  }
  x <- dplyr::group_by(x, Trial, PreTarget)
  x <- dplyr::mutate(x,
                     PreTarget.median = median(Pupil_Diameter.mm, na.rm = TRUE),
                     PreTarget.median = ifelse(is.na(PreTarget) | PreTarget==0, NA, PreTarget.median))
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, PreTarget.median = zoo::na.locf(PreTarget.median, na.rm = FALSE))
  x <- dplyr::mutate(x, PreTarget.median = ifelse(PreTarget > Target, NA, PreTarget.median))
  x <- dplyr::mutate(x, PreTarget.median = zoo::na.locf(PreTarget.median, na.rm = FALSE))
  if (type=="subtractive"){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = Pupil_Diameter.mm - PreTarget.median)
  } else if (type=="divisive"){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = ((Pupil_Diameter.mm - PreTarget.median)/PreTarget.median)*100)
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -PreTarget.median, -baselineoffset.time, -min)
  x <- dplyr::select(x, Subject, Trial, PreTrial, Time, Stimulus,
                     Pupil_Diameter.mm, Pupil_Diameter_bc.mm, PreTarget, Target,
                     Pupils.r, Event, Gaze_Position.x, Gaze_Position.y,
                     Gaze.quality, Head_Dist.cm)
  return(x)
}

