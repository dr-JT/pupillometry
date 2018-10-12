#' Testing Function
#'
#' This function will print ggplot figures of na.removed, method.first = interpolate, and method.first = smooth
#' @param import Directory with preprocessed data files for comparison
#' @param trial Which trial to filter on
#' @keywords
#' @export plot.comparison
#' @examples

plot.comparison <- function(import, trial){
  filelist <- list.files(import, pattern = "PupilData")
  i <- filelist[stringr::str_which(filelist, "interpolated.smoothed")]
  s <- filelist[stringr::str_which(filelist, "smoothed.interpolated")]
  na <- filelist[stringr::str_which(filelist, "naremoved")]

  if (length(i)>0){
    i <- readr::read_delim(paste(import, i, sep = "/"),
                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
    i <- dplyr::filter(i, Trial==trial)
    i <- dplyr::mutate(i, First = "Interpolate")
    i <- dplyr::select(i, Time, First, Pupil_Diameter.mm)
  } else {
    i <- data.frame(Time = NA, First = NA, Pupil_Diameter.mm = NA)
  }
  if (length(s)>0){
    s <- readr::read_delim(paste(import, s, sep = "/"),
                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
    s <- dplyr::filter(s, Trial==trial)
    s <- dplyr::mutate(s, First = "Smooth")
    s <- dplyr::select(s, Time, First, Pupil_Diameter.mm)
  } else {
    s <- data.frame(Time = NA, First = NA, Pupil_Diameter.mm = NA)
  }
  if (length(na)>0){
    na <- readr::read_delim(paste(import, na, sep = "/"),
                           delim = "\t", escape_double = FALSE, trim_ws = TRUE)
    na <- dplyr::filter(na, Trial==trial)
    na <- dplyr::mutate(na, First = "NA.Removed")
    na <- dplyr::select(na, Time, First, Pupil_Diameter.mm)
  } else {
    na <- data.frame(Time = NA, First = NA, Pupil_Diameter.mm = NA)
  }

  plot <- dplyr::bind_rows(list(i, s, na))
  plot <- dplyr::filter(!is.na(Time))
  plot <- ggplot2::ggplot(plot, aes(x = Time, y = Pupil_Diameter.mm, color = First)) +
    geom_point()
  return(plot)
}
