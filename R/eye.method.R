#' Preprocessnig Function
#'
#' This function is for preprocessing baseline pupil data. It takes as input a data file that has been processed
#' with the .convert() functions. The output of this function is a preprocessed data file. Preprocessing steps can
#' include interpolation and smoothing
#' @param x dataframe
#' @param eye.recorded Is there pupil data for the left, right, or both eyes?
#' @param method Which method of using left and right eye data? Average, missing, left, or right.
#' @param cor.criteria If using the average method, then what is the criteria of how highly the two eyes need to correlate?
#' @keywords eye
#' @export
#' @examples
#' eye.method(x)

eye.method <- function(x, eye.recorded = "", method = "average", cor.criteria = .9){
  x <- dplyr::group_by(x, Trial)
  x <- dplyr::mutate(x, pupils.cor = stats::cor(L_Pupil_Diameter.mm, R_Pupil_Diameter.mm, use = "pairwise.complete.obs"),
                     pupils.cor = ifelse(is.na(pupils.cor), 0, pupils.cor))
  if (method=="average"){
    x <- dplyr::mutate(x, Pupil_Diameter.mm = ifelse(!is.na(L_Pupil_Diameter.mm) & !is.na(R_Pupil_Diameter),
                                                     ifelse(pupils.cor>=cor.criteria,
                                                            sum(L_Pupil_Diameter.mm, R_Pupil_Diameter.mm)/2,
                                                            ifelse(L_Missing.Total>=R_Missing.Total,
                                                                   L_Pupil_Diameter.mm, R_Pupil_Diameter.mm)),
                                                     ifelse(!is.na(L_Pupil_Diameter.mm), L_Pupil_Diameter,
                                                            ifelse(!is.na(R_Pupil_Diameter.mm), R_Pupil_Diameter.mm,
                                                                   NA))),
                       Eye = ifelse(!is.na(L_Pupil_Diameter.mm) & !is.na(R_Pupil_Diameter), "average",
                                    ifelse(!is.na(L_Pupil_Diameter.mm), "left",
                                           ifelse(!is.na(R_Pupil_Diameter.mm), "right",
                                                  NA))))
  } else if (method=="missing"){
    x <- dplyr::mutate(x, Pupil_Diameter.mm = ifelse(L_Missing.Total>=R_Missing.Total,
                                                     L_Pupil_Diameter.mm, R_Pupil_Diameter.mm),
                       Eye = ifelse(L_Missing.Total>=R_Missing.Total,
                                    "left", "right"))
  } else if (method=="left"){
    x <- dplyr::mutate(x, Pupil_Diameter.mm = L_Pupil_Diameter.mm,
                       Eye = "left")
    x <- dplyr::rename(x, Missing.Total = L_Missing.Total, Eye_Event = L_Event)
  } else if (method=="right"){
    x <- dplyr::mutate(x, Pupil_Diameter.mm = R_Pupil_Diameter.mm,
                       Eye = "right")
    x <- dplyr::rename(x, Missing.Total = R_Missing.Total, Eye_Event = R_Event)
  }

  x <- dplyr::select(x, -L_Pupil_Diameter.mm, -R_Pupil_Diameter.mm,
                     -L_Missing.Total, -R_Missing.Total, -L_Event, -R_Event)
  x <- dplyr::ungroup(x)
  return(x)
}
