#' Evaluate the amount of missing data
#'
#' This function will deblink pupil data with the option to extend
#'     blinks before and after blink detection
#' @param x dataframe
#' @param extend How many milliseconds to extend blinks
#'     before and after blink detection
#' @keywords deblink
#' @export

pupil_deblink <- function(x, extend = 100){
  x <- dplyr::mutate(x,
                     blink = ifelse(!is.na(Event) & Event == "Blink", 1, 0),
                     blink.lag = dplyr::lag(blink),
                     blink.lead = dplyr::lead(blink),
                     blink.start = ifelse(blink == 1 &
                                            !is.na(blink.lag) &
                                            blink.lag == 0, Time, NA),
                     blink.end = ifelse(blink == 1 &
                                          !is.na(blink.lead) &
                                          blink.lead == 0, Time, NA),
                     blink = ifelse(Time >= blink.start - extend &
                                      Time <= blink.start, 1, blink),
                     blink = ifelse(Time <= blink.end + extend &
                                      Time >= blink.end, 1, blink),
                     Pupil_Diameter.mm = ifelse(is.na(Event) |
                                                  Pupil_Diameter.mm == 0 |
                                                  blink == 1,
                                                NA, Pupil_Diameter.mm))
  x <- dplyr::select(-blink, -blink.lag, -blink.lead, -blink.start, -blink.end)
  return(x)
}
