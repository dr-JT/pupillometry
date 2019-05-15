#' Merge multiple files together
#'
#' This function merges multiple files together.
#' @param path Folder location of files to be merged
#' @param pattern Pattern to identify files to be merged
#' @param output.file File name and path to be saved to.
#' @export

pupil_merge <- function(path = "", pattern = "", output.file = ""){
  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  import <- list()
  for (i in seq_along(filelist)){
    import[[i]] <- readr::read_csv(filelist[[i]])
  }
  bound <- dplyr::bind_rows(import)
  readr::write_csv(bound, output.file)
  return(bound)
}
