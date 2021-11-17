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
  theme_spacious <- function(font.size = 14, bold = TRUE){
    key.size <- trunc(font.size*.8)
    if (bold == TRUE) {
      face.type <- "bold"
    } else {
      face.type <- "plain"
    }

    ggplot2::theme(text = ggplot2::element_text(size = font.size),
                   axis.title.x =
                     ggplot2::element_text(margin =
                                             ggplot2::margin(t = 20, r = 0,
                                                             b = 0, l = 0),
                                           face = face.type),
                   axis.title.y =
                     ggplot2::element_text(margin =
                                             ggplot2::margin(t = 0, r = 15,
                                                             b = 0, l = 0),
                                           face = face.type),
                   legend.title = ggplot2::element_text(face = face.type),
                   legend.spacing = ggplot2::unit(20, "pt"),
                   legend.text = ggplot2::element_text(size = key.size))
  }

  real_name <- dplyr::case_when("Pupil_Diameter.mm" %in% colnames(x) ~
                                  "Pupil_Diameter.mm",
                                "Pupil_Diameter.px" %in% colnames(x) ~
                                  "Pupil_Diameter.px")
  colnames(x)[which(colnames(x) == real_name)] <- "pupil_val_before"
  colnames(y)[which(colnames(y) == real_name)] <- "pupil_val_after"
  x <- dplyr::select(x, Trial, Time, pupil_val_before, dplyr::any_of(aggregate))
  y <- dplyr::select(y, Trial, Time, pupil_val_after, dplyr::any_of(aggregate))


  if (is.null(aggregate)) {
    data_plot <- merge(x, y, by = c("Trial", "Time"), all = TRUE)
    for (trial in unique(x$Trial)) {
      data_trial <- dplyr::filter(data_plot, Trial == trial)
      plot <- ggplot2::ggplot(data_trial, ggplot2::aes(Time)) +
        ggplot2::geom_point(ggplot2::aes(y = pupil_val_before),
                            stroke = .5, size = .75, color = "gray", alpha = 35) +
        ggplot2::geom_point(ggplot2::aes(y = pupil_val_after),
                            stroke = .5, size = .75, ) +
        ggplot2::ggtitle(paste("Trial: ", data_trial$Trial[1], sep = "")) +
        ggplot2::labs(y = "Pupil Size", x = "Time (ms)") +
        ggplot2::theme_linedraw() + theme_spacious() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      # Print plot
      grid::grid.draw(plot)
    }
  } else {
    data_plot <- merge(x, y, by = c("Trial", "Time", aggregate), all = TRUE)
    for (condition in unique(x[[aggregate]])) {
      data_group <- dplyr::filter(data_plot, get(aggregate) == condition)
      plot <- ggplot2::ggplot(data_group, ggplot2::aes(Time)) +
        ggplot2::stat_summary(ggplot2::aes(y = pupil_val_before),
                              fun = mean, na.rm = TRUE,
                              geom = "point",
                              stroke = .5, size = .75, color = "gray", alpha = .5) +
        ggplot2::stat_summary(ggplot2::aes(y = pupil_val_after),
                              fun = mean, na.rm = TRUE,
                              geom = "point",
                              stroke = .5, size = .75) +
        ggplot2::ggtitle(paste("Group: ", data_group[1,aggregate], sep = "")) +
        ggplot2::labs(y = "Pupil Size", x = "Time (ms)") +
        ggplot2::theme_linedraw() + theme_spacious() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      # Print plot
      grid::grid.draw(plot)
    }
  }
}
