#' Preprocessnig Function
#'
#' If both eyes were recorded from, then this function will correlate the timeseries pupil data from both eyes
#' and select only one eye data to keep for further preprocessing and output.
#' This function will also remove trials with too much missing data, specified by `missing.allowed`.
#' @param x xframe
#' @param eye.recorded Is there pupil x for the left, right, or both eyes?
#' @param eye.use Which eye to use? Left or right
#' @param missing.allowed What proportion of missing data is allowed, per trial?
#' @keywords pupil
#' @export
#' @examples
#'

pupil.eye <- function(x, eye.recorded = "", eye.use = "", missing.allowed = 1){

  ## Correlate and select Eyes
  if (eye.recorded == "both"){
    # correlate eyes
    x <- pupil.cor(x)
    # remove either left or right eye
    if (eye.use=="left"){
      x <- dplyr::rename(x, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                            Missing.Total = L_Missing.Total,
                            Eye_Event = L_Event)
    } else if (eye.use=="right"){
      x <- dplyr::rename(x, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                            Missing.Total = R_Missing.Total, Eye_Event = R_Event)
    }
    x <- dplyr::select(x, -L_Pupil_Diameter.mm, -R_Pupil_Diameter.mm,
                          -L_Missing.Total, -R_Missing.Total, -L_Event, -R_Event)
  } else if (eye.recorded=="left"){
    x <- dplyr::rename(x, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                          Missing.Total = L_Missing.Total,
                          Eye_Event = L_Event)
    x <- dplyr::select(x, -L_Pupil_Diameter.mm, -L_Missing.Total, -L_Event)
  } else if (eye.recorded=="right"){
    x <- dplyr::rename(x, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                          Missing.Total = R_Missing.Total,
                          Eye_Event = R_Event)
    x <- dplyr::select(x, -R_Pupil_Diameter.mm, -R_Missing.Total, -R_Event)
  }

  x <- dplyr::filter(x, Missing.Total<=missing.allowed)

  return(x)
}
