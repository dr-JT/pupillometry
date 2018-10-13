#' Testing Function
#'
#' This function will print ggplot figures of na.removed, method.first = interpolate, and method.first = smooth
#' @param import Directory with preprocessed data files for comparison
#' @param files Vector of files to be compared
#' @param labels Vector of labels for plotting.
#' @param trial Which trial to filter on
#' @param title Title the graph
#' @param legend.title Title the legend
#' @keywords
#' @export plot.comparison
#' @examples

plot.comparison <- function(import, files = c(), labels = c(), trial, title = "", legend.title = "Labels"){
  data <- list()
  for (i in seq_along(files)){
    data[[i]] <- readr::read_delim(paste(import, files[i], sep = "/"), delim = "\t",
                                   escape_double = FALSE, trim_ws = TRUE)
    data[[i]] <- dplyr::filter(data[[i]], Trial==trial)
    data[[i]] <- dplyr::mutate(data[[i]], Labels = labels[i])
    data[[i]] <- dplyr::select(data[[i]], Time, Labels, Pupil_Diameter.mm)
  }
  data <- dplyr::bind_rows(data)

  plot <- dplyr::filter(data, !is.na(Time))
  plot <- ggplot2::ggplot(plot, ggplot2::aes(x = Time, y = Pupil_Diameter.mm, color = Labels, shape = Labels)) +
    ggplot2::geom_point(alpha = .35) +
    ggplot2::ggtitle(title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(color = legend.title, shape = legend.title) +
    ggplot2::xlab("Time (ms)")

  return(plot)
}
