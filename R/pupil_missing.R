#' Trial removal
#'
#' This function will remove trials that have too much missing data. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' Removes rows for trials with too much missing data. Adds a `Pupil_Missing`
#' or `Pupil_Missing_1` and `Pupil_Missing_2` columns.
#'
#' @section Usage:
#'
#' This can be used:
#'
#' 1) after artifact removal and before smoothing / interpolation
#'
#' 2) after smoothing / interpolation
#'
#' or
#'
#' 3) both
#'
#' @param x dataframe.
#' @param missing_allowed What proportion of missing data is allowed, per trial?
#' @import data.table
#' @export
#'

pupil_missing <- function(x, missing_allowed = 1) {

  x <- dtplyr::lazy_dt(x)

  eyes <- eyes_detect(x)

  for (eye in eyes) {
    real_name <- eye
    eye_prefix <- stringr::str_split(real_name, "_")[[1]][1]
    missing_name <- ifelse(eye_prefix == "Pupil",
                           paste(eye_prefix, "_Missing", sep = ""),
                           paste(eye_prefix, "_Pupil_Missing", sep = ""))

    if (missing_name %in% x[["vars"]]) {
      x[["vars"]][which(x[["vars"]] == missing_name)] <-
        paste(missing_name, "_FirstPass", sep = "")
    }

    x[["vars"]][which(x[["vars"]] == real_name)] <- "pupil_val"

    x <- dplyr::group_by(x, Trial)
    message(x["vars"])
    x <- dplyr::mutate(x,
                       Missing = ifelse(is.na(.data$pupil_val), 1, 0),
                       Missing =
                         sum(Missing, na.rm = TRUE) / dplyr::n())
    x <- dplyr::ungroup(x)
    x <- dplyr::filter(x, Missing <= missing_allowed)

    x[["vars"]][which(x[["vars"]] == "Missing")] <- missing_name
    if (paste(missing_name, "_FirstPass", sep = "") %in% x[["vars"]]) {
      x[["vars"]][which(x[["vars"]] == missing_name)] <-
        paste(missing_name, "_SecondPass", sep = "")
    }
    x[["vars"]][which(x[["vars"]] == "pupil_val")] <- real_name
  }

  x <- dplyr::as_tibble(x)
  return(x)
}
