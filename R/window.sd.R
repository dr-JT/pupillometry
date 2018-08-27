#' Pupil Data Function
#'
#' This is a sliding time window function to calculate the standard
#' deviation of each time window over a vector of pupil data. You can
#' also use this to calculate the overall standard deviation by not.
#' specifying a window size.
#' @param x vector of pupil data
#' @param size size of sliding time window in seconds (default: 0)
#' @param Hz the frequency at which pupil samples were collected (default: 250)
#' @keywords window sd
#' @export window.sd
#' @examples
#' window.sd(x, size = 30, Hz = 250)

window.sd <- function(x, size = 0, Hz = 250){
  if (size==0){
    dv <- sd(x, na.rm = TRUE)
  } else {
    index <- 1
    samples <- size*Hz
    dv <- c()
    for (time in seq(1, length(x), by = samples)){
      dv[index] <- sd(x[time:(time+samples-1)], na.rm = TRUE)
      index <- index+1
    }
  }
  return(dv)
}
