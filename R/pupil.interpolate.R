#' Pupil Preprocessing Function
#'
#' This function performs a linear interpolation over missing values.
#' The 'zoo' package is a dependency
#' @param x xframe
#' @param type What type of interpolation to use? (default: cubic-spline)
#' @param maxgap Maximum number of NAs to interpolate over. Anything gaps over this value will not be interpolated.
#' @param hz The recording frequency (used to calculate window size)
#' @keywords interpolate
#' @export
#' @examples
#' pupil.interpolate(x, type = "cubic-spline", eye.recorded = "both")

pupil.interpolate <- function(x, type = "cubic-spline", maxgap = Inf, hz = ""){
  if (maxgap!=Inf){
    if (hz==""){
      hz <- x$Hz[1]
    }
    maxgap <- maxgap/(hz/1000)
  }
  x <- dplyr::group_by(x, Trial)
  if (type=="cubic-spline"){
    x <- dplyr::mutate(x,
                       index = ifelse(is.na(Pupil_Diameter.mm), NA, dplyr::row_number()),
                       index = zoo::na.approx(index, na.rm = FALSE),
                       Pupil_Diameter.mm = ifelse(Missing.Total<1,
                                                  zoo::na.spline(Pupil_Diameter.mm, na.rm = FALSE, x = index, maxgap = maxgap),
                                                  NA))
    x <- dplyr::select(x, -index)
  } else if (type=="linear"){
    x <- dplyr::mutate(x,
                       Pupil_Diameter.mm = zoo::na.approx(Pupil_Diameter.mm, na.rm = FALSE, maxgap = maxgap))
  }
  x <- dplyr::ungroup(x)
  return(x)
}
