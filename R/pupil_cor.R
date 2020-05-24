#' Add column that is the correlation between left and rigth eyes
#'
#' This function will simply calculate the correlation between
#' left and right eyes
#' @param x dataframe
#' @export
#'

pupil_cor <- function(x){

  l_pupil <- ifelse("L_Pupil_Diameter.mm" %in% colnames(data),
                    "L_Pupil_Diameter.mm", "L_Pupil_Diameter.px")
  r_pupil <- ifelse("R_Pupil_Diameter.mm" %in% colnames(data),
                    "R_Pupil_Diameter.mm", "R_Pupil_Diameter.px")

  x <- dplyr::mutate(x, Pupils.r = stats::cor(get(l_pupil), get(r_pupil),
                                              use = "pairwise.complete.obs"),
                     Pupils.r = ifelse(is.na(Pupils.r), 0, Pupils.r))

  return(x)
}
