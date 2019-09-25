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
#'

pupil_interpolate <- function(x, type = "cubic-spline", maxgap = Inf, hz = ""){
  if (maxgap != Inf){
    maxgap <- round(maxgap / (1000 / hz))
  }
  x <- dplyr::group_by(x, Trial)
  if (type=="cubic-spline"){
    x <- dplyr::mutate(x,
                       Missing.Total = ifelse(is.na(Pupil_Diameter.mm), 1, 0),
                       Missing.Total = sum(Missing.Total, na.rm = TRUE)/dplyr::n(),
                       index = ifelse(is.na(Pupil_Diameter.mm), NA, dplyr::row_number()),
                       index = zoo::na.approx(index, na.rm = FALSE),
                       Pupil_Diameter.mm = ifelse(Missing.Total > .9, 999, Pupil_Diameter.mm),
                       Pupil_Diameter.mm = zoo::na.spline(Pupil_Diameter.mm,
                                                          na.rm = FALSE,
                                                          x = index,
                                                          maxgap = maxgap),
                       Pupil_Diameter.mm = ifelse(Missing.Total > .9, NA, Pupil_Diameter.mm))
    x <- dplyr::select(x, -index, -Missing.Total)
  } else if (type=="linear"){
    x <- dplyr::mutate(x,
                       Pupil_Diameter.mm = zoo::na.approx(Pupil_Diameter.mm, na.rm = FALSE, maxgap = maxgap))
  }
  x <- dplyr::ungroup(x)
  return(x)
}
