#' Correlate pupil values
#'
#' Calculate the correlation between left and right pupil values. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Adds a column `Pupil.r`
#'
#' @param x dataframe.
#' @export
#'

pupil_cor <- function(x){

  x <- dtplyr::lazy_dt(x)

  l_pupil <- ifelse("L_Pupil_Diameter.mm" %in% colnames(x),
                    "L_Pupil_Diameter.mm", "L_Pupil_Diameter.px")
  r_pupil <- ifelse("R_Pupil_Diameter.mm" %in% colnames(x),
                    "R_Pupil_Diameter.mm", "R_Pupil_Diameter.px")

  x <- dplyr::mutate(x, Pupils.r = stats::cor(get(l_pupil), get(r_pupil),
                                              use = "pairwise.complete.obs"))

  x <- dplyr::as_tibble(x)
  return(x)
}
