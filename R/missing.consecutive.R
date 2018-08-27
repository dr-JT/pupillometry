#' Missing Data Function
#'
#' This function returns the longest consecutive string of missing data in an array as a proportion of the total number of samples in the array
#' @param x vector
#' @keywords missing
#' @export
#' @examples
#' missing.consecutive(x)

missing.consecutive <- function(x){
  missing <- length(is.na(x)[is.na(x)==TRUE])/length(is.na(x))
  if (missing==0){
    missingLength <- 0
  } else {
    missingLength <- is.na(x)
    missingLength[(missingLength==FALSE)] <- NA
    missingLength <- length(na.contiguous(missingLength))/length(is.na(x))
  }
}
