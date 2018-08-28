#' Pupil Preprocessing Function
#'
#' This function performs a linear interpolation over missing values.
#' The 'zoo' package is a dependency
#' @param x xframe
#' @param type What type of interpolation to use? (default: cubic-spline)
#' @param maxgap Maximum number of NAs to interpolate over. Anything gaps over this value will not be interpolated.
#' @param hz The recording frequency (used to calculate window size)
#' @param eye.recorded Is there pupil x for the left, right, or both eyes?
#' @keywords interpolate
#' @export
#' @examples
#' pupil.interpolate(x, type = "cubic-spline", eye.recorded = "both")

pupil.interpolate <- function(x, type = "cubic-spline", maxgap = Inf, hz = "", eye.recorded = ""){
  if (maxgap!=Inf){
    if (hz==""){
      hz <- x$hz[1]
    }
    maxgap <- maxgap/(hz/1000)
  }
  x <- dplyr::group_by(x, Trial)
  if (eye.recorded=="both"){
    if (type=="cubic-spline"){
      x <- dplyr::mutate(x,
                         index_left = ifelse(is.na(L_Pupil_Diameter.mm), NA, dplyr::row_number()),
                         index_left = zoo::na.approx(index_left, na.rm = FALSE),
                         index_right = ifelse(is.na(R_Pupil_Diameter.mm), NA, dplyr::row_number()),
                         index_right = zoo::na.approx(index_right, na.rm = FALSE),
                         L_Pupil_Diameter.mm = ifelse(L_Missing.Total<1,
                                                      zoo::na.spline(L_Pupil_Diameter.mm, na.rm = FALSE, x = index_left, maxgap = maxgap),
                                                      NA),
                         R_Pupil_Diameter.mm = ifelse(R_Missing.Total<1,
                                                      zoo::na.spline(R_Pupil_Diameter.mm, na.rm = FALSE, x = index_right, maxgap = maxgap),
                                                      NA))
      x <- dplyr::select(x, -index_left, -index_right)
    } else if (type=="linear"){
      x <- dplyr::mutate(x,
                         L_Pupil_Diameter.mm = zoo::na.approx(L_Pupil_Diameter.mm, na.rm = FALSE, maxgap = maxgap),
                         R_Pupil_Diameter.mm = zoo::na.approx(R_Pupil_Diameter.mm, na.rm = FALSE, maxgap = maxgap))
    }

  } else if (eye.recorded=="left"){
    if (type=="cubic-spline"){
      x <- dplyr::mutate(x,
                         index_left = ifelse(is.na(L_Pupil_Diameter.mm), NA, dplyr::row_number()),
                         index_left = zoo::na.approx(index_left, na.rm = FALSE),
                         L_Pupil_Diameter.mm = ifelse(L_Missing.Total<1,
                                                      zoo::na.spline(L_Pupil_Diameter.mm, na.rm = FALSE, x = index_left, maxgap = maxgap),
                                                      NA))
      x <- dplyr::select(x, -index_left)
    } else if (type=="linear"){
      x <- dplyr::mutate(x,
                         L_Pupil_Diameter.mm = zoo::na.approx(L_Pupil_Diameter.mm, na.rm = FALSE, maxgap = maxgap))
    }

  } else if (eye.recorded=="right"){
    if (type=="cubic-spline"){
      x <- dplyr::mutate(x,
                         index_right = ifelse(is.na(R_Pupil_Diameter.mm), NA, dplyr::row_number()),
                         index_right = zoo::na.approx(index_right, na.rm = FALSE),
                         R_Pupil_Diameter.mm = ifelse(R_Missing.Total<1,
                                                      zoo::na.spline(R_Pupil_Diameter.mm, na.rm = FALSE, x = index_right, maxgap = maxgap),
                                                      NA))
      x <- dplyr::select(x, -index_right)
    } else if (type=="linear"){
      x <- dplyr::mutate(x,
                         R_Pupil_Diameter.mm = zoo::na.approx(R_Pupil_Diameter.mm, na.rm = FALSE, maxgap = maxgap))
    }
  }
  x <- dplyr::ungroup(x)
  return(x)
}
