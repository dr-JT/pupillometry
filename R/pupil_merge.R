#' Merge preprocessed files
#'
#' Merge all the preprocessed data files into one large file. See
#' https://dr-jt.github.io/pupillometry/ for more information.
#'
#' @section Output:
#'
#' A merged data file saved on computer
#'
#' @param path Folder location of files to be merged
#' @param pattern Pattern to identify files to be merged
#' @param output_file File name and path to be saved to.
#' @export
#'

pupil_merge <- function(path = "", pattern = "", output_file = "") {

  filelist <- list.files(path = path, pattern = pattern, full.names = TRUE)
  import <- list()
  for (i in seq_along(filelist)) {
    import[[i]] <- readr::read_csv(filelist[[i]])
    import[[i]]$Subject <- factor(import[[i]]$Subject)
  }
  bound <- dplyr::bind_rows(import)

  if (output_file != "") {
    readr::write_csv(bound, output_file)
  }

  return(bound)
}
