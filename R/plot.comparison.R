#' Testing Function
#'
#' This function will print ggplot figures of na.removed, method.first = interpolate, and method.first = smooth
#' @param import Import directory
#' @param trial Which trial to filter on
#' @keywords
#' @export
#' @examples

plot.comparison <- function(import, trial){
  i <- readr::read_delim(paste(import, "memoryload_11_PupilData_interpolated.smoothed.bc.ds.txt", sep = "/"),
                         delim = "\t", escape_double = FALSE, trim_ws = TRUE)
  s <- readr::read_delim(paste(import, "memoryload_11_PupilData_smoothed.interpolated.bc.ds.txt", sep = "/"),
                         delim = "\t", escape_double = FALSE, trim_ws = TRUE)
  na <- readr::read_delim(paste(import, "memoryload_11_PupilData_naremoved.bc.ds.txt", sep = "/"),
                          delim = "\t", escape_double = FALSE, trim_ws = TRUE)

  i <- dplyr::filter(i, Trial==trial)
  i <- dplyt::mutate(i, First = "Interpolate")
  i <- dplyr::select(i, Time, First, Pupil_Diameter.mm)
  s <- dplyr::filter(s, Trial==trial)
  s <- dplyr::mutate(s, First = "Smooth")
  s <- dplyr::select(s, Time, First, Pupil_Diameter.mm)
  na <- dplyr::filter(na, Trial==trial)
  na <- dplyr::mutate(na, First = "NA.Removed")
  na <- dplyr::select(na, Time, First, Pupil_Diameter.mm)

  plot <- dplyr::bind_rows(list(i, s, na))
  plot <- ggplot2::ggplot(plot, aes(x = Time, y = Pupil_Diameter.mm, color = First)) +
    geom_point()
  return(plot)
}
