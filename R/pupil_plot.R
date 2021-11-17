#' Plot and inspect how preprocessing steps are changing the pupil data.
#' An exploratory function only.
#'
#' This function will print ggplot figures of either trial level or aggregated pupil data
#' @param x data before preprocessing step
#' @param y data after preprocessing step
#' @param aggregate column name to aggregate the plot by.
#'     For individual trial plots leave as NULL.
#' @export pupil_plot
#'

pupil_plot <- function(x, y, aggregate = NULL) {
  real_name <- dplyr::case_when("Pupil_Diameter.mm" %in% colnames(x) ~
                                  "Pupil_Diameter.mm",
                                "Pupil_Diameter.px" %in% colnames(x) ~
                                  "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val_before"
  colnames(y)[which(colnames(y) == real_name)] <- "pupil_val_after"
  x <- dplyr::select(x, Trial, Time, pupil_val_before, dplyr::any_of(aggregate))
  y <- dplyr::select(y, Trial, Time, pupil_val_after, dplyr::any_of(aggregate))
  data_plot <- merge(x, y, by = c("Trial", "Time"), all = TRUE)

  if (is.null(aggregate)) {
    for (trial in unique(x$Trial)) {
      data_trial <- dplyr::filter(data_plot, Trial == trial)
      plot <- ggplot2::ggplot(data_trial, ggplot2::aes(Time)) +
        ggplot2::geom_line(ggplot2::aes(y = pupil_val_before), alpha = .35) +
        ggplot2::geom_line(ggplot2::aes(y = pupil_val_after)) +
        ggplot2::ggtitle(paste("Trial: ", data_trial$Trial[1], sep = "")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(y = "Pupil Size", x = "Time (ms)")
      # Print plot
      grid::grid.draw(plot)
    }
  } else {
    for (group in unique(x[[aggregate]])) {
      data_group <- dplyr::filter(data_plot, aggregate == group)
      plot <- ggplot2::ggplot(data_group,
                              ggplot2::aes(Time)) +
        ggplot2::stat_summary(ggplot2::aes(y = pupil_val_before),
                              fun = mean, na.rm = TRUE,
                              geom = "line", alpha = .35) +
        ggplot2::stat_summary(ggplot2::aes(y = pupil_val_after),
                              fun = mean, na.rm = TRUE,
                              geom = "line") +
        ggplot2::ggtitle(paste("Group: ", data_group[1,aggregate], sep = "")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::labs(y = "Pupil Size", x = "Time (ms)")
      # Print plot
      grid::grid.draw(plot)
    }
  }
}
