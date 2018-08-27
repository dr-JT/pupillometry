#' Missing Data Function
#'
#' This function returns the total number of missing data in an array as a proportion of the total number of samples in the array
#' @param x vector
#' @keywords missing
#' @export
#' @examples
#' missing.total(x)

missing.total <- function(x){
  length(is.na(x)[is.na(x)==TRUE])/length(is.na(x))
}
