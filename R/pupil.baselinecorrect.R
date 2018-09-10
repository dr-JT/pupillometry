#' Pupil Data Function
#'
#' This function applies a pre-trial baseline correction on the data
#' @param x dataframe
#' @param bc.duration Duration pre-trial baseline to use for correction
#' @param start.target Stimulus that identifies the target onset
#' @param iteration the nth time this data has been baseline corrected
#' @keywords baseline
#' @export
#' @examples
#' pupil.baselinecorrect(file = "path/filename", baseline.duration = 2000, start.trial = "# Message: Target")

pupil.baselinecorrect <- function(x, bc.duration = 200, start.target = "", iteration = 1){
  start.timestamps <- x$Time[(grep(start.target, x$Message))]
  trials <- x$Trial[(grep(start.target, x$Message))]
  x <- dplyr::group_by(x, Trial)
  if (iteration>1){
    x <- dplyr::mutate(x, PreTarget = ifelse(!(Trial%in%trials), 0,
                                             ifelse(Time>=start.timestamps[match(Trial,trials)]-bc.duration & Time<start.timestamps[match(Trial,trials)],
                                                    1,0)),
                       Target = ifelse(!(Trial%in%trials), 0,
                                       ifelse(Target>=1,Target+1,ifelse(Time>=start.timestamps[match(Trial,trials)]
                                                                        ,1,0))))
  } else{
    x <- dplyr::mutate(x, PreTarget = ifelse(!(Trial%in%trials), 0,
                                             ifelse(Time>=start.timestamps[match(Trial,trials)]-bc.duration & Time<start.timestamps[match(Trial,trials)],
                                                    1,0)),
                       Target = ifelse(!(Trial%in%trials),0,
                                       ifelse(Time>=start.timestamps[match(Trial,trials)],
                                              1,0)))
  }
  z <- dplyr::filter(x, PreTarget==1)
  z <- dplyr::group_by(z, Trial)
  z <- dplyr::summarise(z, PreTarget.mean = mean(Pupil_Diameter.mm, na.rm = TRUE))
  z <- dplyr::ungroup(z)
  x <- merge(x, z, by = "Trial", all = TRUE)
  x <- dplyr::mutate(x, Pupil_Diameter.mm = ifelse(Target==1, Pupil_Diameter.mm - PreTarget.mean, Pupil_Diameter.mm))
  x <- dplyr::arrange(x, Subject, Trial, Time)
  x <- dplyr::select(x, -PreTarget.mean)
  return(x)
}

