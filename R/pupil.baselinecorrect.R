#' Apply baseline correction
#'
#' This function applies a pre-trial baseline correction on the data
#' @param x dataframe
#' @param baselineoffset.message Message string(s) that marks the offset of baseline period(s)
#' @param bc.duration Duration baseline period(s) to use for correction
#' @param bc.type Do you want to use "subtractive" or "divisive" baseline correction? (default: "subtractive")
#' @param match Should the message string be an "exact" match or a "pattern" match?
#' @keywords baseline
#' @export
#' @examples
#' pupil.baselinecorrect(file = "path/filename", baseline.duration = 2000, start.trial = "# Message: Target")

pupil.baselinecorrect <- function(x, baselineoffset.message = "", bc.duration = 200, bc.type = "subtractive", match = "exact"){
  baselines.n <- length(baselineoffset.message)
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, PreTarget=0, Target=0)
  for (message in baselineoffset.message){
    n <- match(message, baselineoffset.message)
    if (match=="exact"){
      x <- dplyr::mutate(x, baselineoffset.time = ifelse(Message==message, Time, NA))
    } else if (match=="pattern"){
      x <- dplyr::mutate(x, baselineoffset.time = ifelse(stringr::str_detect(Message, message), Time, NA))
    }
    x <- dplyr::mutate(x,
                       min = min(baselineoffset.time, na.rm = TRUE),
                       baselineoffset.time = ifelse(is.na(baselineoffset.time) | baselineoffset.time!=min,NA,baselineoffset.time),
                       baselineoffset.time = zoo::na.locf(baselineoffset.time, na.rm = FALSE),
                       baselineoffset.time = zoo::na.locf(baselineoffset.time, na.rm = FALSE, fromLast = TRUE),
                       baselineoffset.time = ifelse(is.infinite(min), Inf, baselineoffset.time),
                       PreTarget = ifelse(Time >= (baselineoffset.time-bc.duration) & Time < baselineoffset.time, n, PreTarget),
                       Target = ifelse(Time >= baselineoffset.time, n, Target))
  }
  x <- dplyr::group_by(x, Trial, PreTarget)
  x <- dplyr::mutate(x,
                     PreTarget.mean = mean(Pupil_Diameter.mm, na.rm = TRUE),
                     PreTarget.mean = ifelse(is.na(PreTarget) | PreTarget==0 | Target > 0, NA, PreTarget.mean))
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, PreTarget.mean = zoo::na.locf(PreTarget.mean, na.rm = FALSE))
  if (bc.type=="subtractive"){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = Pupil_Diameter.mm - PreTarget.mean)
  } else if (bc.type=="divisive"){
    x <- dplyr::mutate(x, Pupil_Diameter_bc.mm = ((Pupil_Diameter.mm - PreTarget.mean)/PreTarget.mean)*100)
  }

  x <- dplyr::ungroup(x)
  x <- dplyr::select(x, -PreTarget.mean, -baselineoffset.time, -min)
  return(x)
}

