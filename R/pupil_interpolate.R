#' Apply interpolation
#'
#' This function performs an interpolation over missing values.
#' The 'zoo' package is a dependency
#' @param x dataframe
#' @param type What type of interpolation to use? "linear" or "cubic-spline"
#' @param maxgap Maximum number of NAs to interpolate over.
#'     Any gaps over this value will not be interpolated.
#' @param plot Logical. Inspect a plot of how pupil values changed?
#' @param hz The recording frequency (used to calculate window size)
#' @export
#'

pupil_interpolate <- function(x, type = "cubic-spline",
                              maxgap = Inf, hz = "", plot = FALSE){
  x_before <- x

  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")

  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"
  if (maxgap != Inf){
    maxgap <- round(maxgap / (1000 / hz))
  }
  x <- dplyr::group_by(x, Trial)
  if (type == "cubic-spline"){
    x <- dplyr::mutate(x,
                       Missing.Total = ifelse(is.na(pupil_val), 1, 0),
                       Missing.Total =
                         sum(Missing.Total, na.rm = TRUE) / dplyr::n(),
                       index =
                         ifelse(is.na(pupil_val), NA, dplyr::row_number()),
                       index = zoo::na.approx(index, na.rm = FALSE),
                       pupil_val = ifelse(Missing.Total > .9, 999, pupil_val),
                       pupil_val = zoo::na.spline(pupil_val,
                                                  na.rm = FALSE,
                                                  x = index,
                                                  maxgap = maxgap),
                       pupil_val = ifelse(Missing.Total > .9, NA, pupil_val))
    x <- dplyr::select(x, -index, -Missing.Total)
  } else if (type == "linear"){
    x <- dplyr::mutate(x,
                       pupil_val = zoo::na.approx(pupil_val, na.rm = FALSE,
                                                  maxgap = maxgap))
  }
  x <- dplyr::arrange(x, Time)
  x <- dplyr::ungroup(x)
  colnames(x)[which(colnames(x) == "pupil_val")] <- real_name

  if (plot == TRUE) pupil_plot(x_before, x)

  return(x)
}
