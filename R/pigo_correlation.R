#' Correlations for PIGO
#'
#' @param data A data frame
#' @param x X-axis variable. Has to be a character vector.
#' @param y Y-axis variable. Has to be a character vector.
#' @param filename Path file to save the plot, defaults to NULL.
#'
#' @return A ggplot.
#' @export
#'
ggplot_correlation_cliar <- function(data, x, y, filename = NULL){
    plot <- data |>
      ggplot(
        aes(
          y = .data[[y]],
          x = .data[[x]],
          color = region
        )
      ) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "black", linetype = "dashed") +
      labs(
        y = y,
        x = paste(x, "(2019-2023)"),
        color = "Region"
      ) +
      scale_color_brewer(palette = "Paired") +
      scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 5),
        limits = c(min(data[[y]]), max(data[[y]]))
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 20),
        plot.margin = margin(t = 10, r = 10, b = 15, l = 10),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 18),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16)
      ) +
      ggtitle(
        label = paste0("Correlation between: ", y),
        subtitle = paste("and", x)
      )

    if(!purrr::is_null(filename)){
      ggsave(
        plot = plot,
        filename = filename,
        width = 12,
        height = 12,
        dpi = 300,
        bg = "white"
      )
    }

    return(plot)
  }
