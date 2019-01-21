#' Apply baseline correction
#'
#' This function applies a pre-trial baseline correction on the data
#' @param x dataframe
#' @param message Message string(s) that marks the offset of baseline period(s)
#' @param duration Duration baseline period(s) to use for correction
#' @param type Do you want to use "subtractive" or "divisive" baseline correction? (default: "subtractive")
#' @param match Should the message string be an "exact" match or a "pattern" match?
#' @keywords baseline
#' @export
#' @examples
#' pupil_baselinecorrect(file = "path/filename", baseline.duration = 2000, start.trial = "# Message: Target")

pupil_baselinecorrect <- function(x, message = "", duration = 200, type = "subtractive", match = "exact"){
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
                       PreTarget = ifelse(Time >= (baselineoffset.time-duration) & Time < baselineoffset.time, n, PreTarget),
                       Target = ifelse(Time >= baselineoffset.time, n, Target))
  }
  x <- dplyr::group_by(x, Trial, PreTarget)
  x <- dplyr::mutate(x,
                     PreTarget.mean = mean(Pupil_Diameter.mm, na.rm = TRUE),
                     PreTarget.mean = ifelse(is.na(PreTarget) | PreTarget==0, NA, PreTarget.mean))
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, PreTarget.mean = zoo::na.locf(PreTarget.mean, na.rm = FALSE))
  x <- dplyr::mutate(x, PreTarget.mean = ifelse(PreTarget > Target, NA, PreTarget.mean))
  x <- dplyr::mutate(x, PreTarget.mean = zoo::na.locf(PreTarget.mean, na.rm = FALSE))
  if (type=="subtractive"){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = Pupil_Diameter.mm - PreTarget.mean)
  } else if (type=="divisive"){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = ((Pupil_Diameter.mm - PreTarget.mean)/PreTarget.mean)*100)
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -PreTarget.mean, -baselineoffset.time, -min)
  return(x)
}

