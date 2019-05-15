#' Apply interpolation
#'
#' This function performs a linear interpolation over missing values.
#' The 'zoo' package is a dependency
#' @param x dataframe
#' @param type What type of interpolation to use? (default: cubic-spline)
#' @param maxgap Maximum number of NAs to interpolate over. Anything gaps over this value will not be interpolated.
#' @param hz The recording frequency (used to calculate window size)
#' @keywords interpolate
#' @export
#' @examples
#' pupil_interpolate(x, type = "cubic-spline", eye.recorded = "both")

pupil_interpolate <- function(x, type = "cubic-spline", maxgap = Inf, hz = ""){
  if (maxgap != Inf){
    maxgap <- round(maxgap / (1000 / hz))
  }
  x <- dplyr::group_by(x, Trial)
  if (type=="cubic-spline"){
    x <- dplyr::mutate(x,
                       index = ifelse(is.na(Pupil_Diameter.mm), NA, dplyr::row_number()),
                       no.data = ifelse(sum(index, na.rm = TRUE) == 0, 1, 0),
                       index = zoo::na.approx(index, na.rm = FALSE),
                       Pupil_Diameter.mm =
                         ifelse(no.data == 0,
                                zoo::na.spline(Pupil_Diameter.mm,
                                               na.rm = FALSE,
                                               x = index,
                                               maxgap = maxgap),
                                Pupil_Diameter.mm))
    x <- dplyr::select(x, -index)
  } else if (type=="linear"){
    x <- dplyr::mutate(x,
                       Pupil_Diameter.mm = zoo::na.approx(Pupil_Diameter.mm, na.rm = FALSE, maxgap = maxgap))
  }
  x <- dplyr::ungroup(x)
  return(x)
}
