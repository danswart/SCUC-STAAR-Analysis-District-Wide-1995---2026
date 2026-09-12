# Reusable presentation helpers for SCUC ISD "Did It Work?" charts.
# These functions affect presentation only; they do not recalculate chart data
# or Policycraft control limits.

did_it_work_chart_theme <- function() {
  policycraft::policycraft_chart_theme() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

did_it_work_percent_scale <- function() {
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1))
}

did_it_work_subject_end_layer <- function(data) {
  line_end <- data |>
    dplyr::filter(!is.na(value)) |>
    dplyr::slice_max(date, n = 1, with_ties = FALSE)

  ggplot2::geom_text(
    data = line_end,
    ggplot2::aes(label = subject),
    colour = "darkgray",
    size = 5,
    fontface = "bold",
    hjust = -0.15,
    vjust = 0.5,
    inherit.aes = TRUE,
    show.legend = FALSE
  )
}

did_it_work_superintendent_layers <- function(dates) {
  terms <- data.frame(
    superintendent = c(
      "Dr. Byron P. Steele II",
      "Dr. Edward \"Ed\" West",
      "Dr. Greg Gibson",
      "Dr. Clark C. Ealy",
      "Mrs. Paige A. Meloni"
    ),
    term_label = c(
      "1976–2001", "2001–2009", "2010–2020", "2020–2024", "2024–Present"
    ),
    start = as.Date(c(
      "1976-01-01", "2001-01-01", "2010-01-01", "2020-01-01", "2024-01-01"
    )),
    end = as.Date(c(
      "2001-01-01", "2010-01-01", "2020-01-01", "2024-01-01", NA
    ))
  )

  visible_min <- min(dates, na.rm = TRUE)
  visible_max <- as.Date(paste0(max(format(dates, "%Y"), na.rm = TRUE), "-12-31"))
  terms$visible_start <- pmax(terms$start, visible_min)
  terms$end[is.na(terms$end)] <- visible_max
  terms$visible_end <- pmin(terms$end, visible_max)
  terms <- terms[terms$visible_start <= terms$visible_end, , drop = FALSE]
  terms$midpoint <- terms$visible_start +
    as.numeric(terms$visible_end - terms$visible_start) / 2
  terms$label <- paste0(terms$superintendent, "\n", terms$term_label)
  terms$label_angle <- ifelse(
    as.numeric(terms$visible_end - terms$visible_start) < 365.25 * 4,
    90,
    0
  )
  terms$label_hjust <- ifelse(terms$label_angle == 90, 1.05, 0.5)

  list(
    ggplot2::geom_segment(
      data = terms,
      ggplot2::aes(
        x = visible_start,
        xend = visible_end,
        y = Inf,
        yend = Inf
      ),
      inherit.aes = FALSE,
      colour = "#34495E",
      linewidth = 1.15,
      arrow = grid::arrow(
        angle = 25,
        length = grid::unit(0.14, "inches"),
        ends = "both",
        type = "closed"
      ),
      show.legend = FALSE
    ),
    ggplot2::geom_text(
      data = terms,
      ggplot2::aes(
        x = midpoint,
        y = Inf,
        label = label,
        angle = label_angle,
        hjust = label_hjust
      ),
      inherit.aes = FALSE,
      colour = "#263746",
      size = 5,
      fontface = "bold",
      lineheight = 0.9,
      vjust = 1.75,
      check_overlap = FALSE,
      show.legend = FALSE
    )
  )
}

save_did_it_work_chart <- function(chart, filename) {
  ggplot2::ggsave(
    filename = filename,
    plot = chart,
    width = 16,
    height = 9,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}
