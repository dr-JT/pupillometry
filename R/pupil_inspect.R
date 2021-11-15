#' Plot and inspect how preprocessing steps are changing the pupil data.
#' An exploratory function only.
#'
#' This function will print ggplot figures of either trial level or aggregated pupil data
#' @param x dataframe
#' @param aggregate column name to aggregate the plots by.
#'     For individual trial plots leave as NULL.
#' @export plot_comparison
#'

plot_comparison <- function(x, aggregate = NULL){
  real_name <- ifelse("Pupil_Diameter.mm" %in% colnames(x),
                      "Pupil_Diameter.mm", "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val"

  if (is.null(aggregate)) {
    for (trial in unique(x$Trial)) {
      x_trial <- dplyr::filter(x, Trial == trial)
      plot <- ggplot2::ggplot(x_trial, ggplot2::aes(Time, pupil_value)) +
        ggplot2::geom_point(alpha = .35) +
        ggplot2::ggtitle(paste("Trial: ", x$Trial[1], sep = "")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(y = real_name, x = "Time (ms)")
      # Print plot
      grid::grid.newpage()
      grid::grid.draw(plot)
    }
  } else {
    for (group in unique(x[[aggregate]])) {
      x_group <- dplyr::filter(x, get(aggregate) == group)
      plot <- ggplot2::ggplot(x_group, ggplot2::aes(Time, pupil_value,
                                                    group = get(aggregate))) +
        ggplot2::stat_summary(fun = mean, na.rm = TRUE, geom = "point") +
        ggplot2::ggtitle(paste("Group: ", x[1,aggregate], sep = "")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(y = real_name, x = "Time (ms)")
      # Print plot
      grid::grid.newpage()
      grid::grid.draw(plot)
    }
  }
}
