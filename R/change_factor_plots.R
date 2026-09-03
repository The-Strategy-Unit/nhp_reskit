#' Generate overall change factor ("waterfall") chart
#'
#' @param change_factor_data tibble, as produced by [compile_change_factor_data]
#' @export
make_overall_cf_plot <- function(change_factor_data) {
  if (nrow(change_factor_data) == 0) {
    return(make_no_data_plot(no_data_reason(change_factor_data)))
  }
  x_axis_label <- create_measure_label(unique(change_factor_data[["measure"]]))
  change_factor_data |>
    dplyr::mutate(
      colour = dplyr::case_when(
        .data[["change_factor"]] == "baseline" ~ "#686f73",
        .data[["change_factor"]] == "estimate" ~ "#ec6555",
        .data[["value"]] >= 0 ~ "#f9bf07",
        .default = "#2c2825"
      ),
      dplyr::across("change_factor", \(x) {
        sub("Beddays", "Bed days", uppercase_init(gsub("_", " ", x)))
      }),
      dplyr::across("change_factor", forcats::fct_inorder)
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data[["hide"]],
        xend = .data[["total"]],
        y = .data[["change_factor"]],
        yend = .data[["change_factor"]],
        colour = .data[["colour"]]
      ),
      # dynamic: bigger if fewer bars (130 is relative to 600px plot height)
      lwd = 130 / nrow(change_factor_data)
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(5),
      labels = scales::label_comma()
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = x_axis_label, y = NULL) +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}


#' Generate bar charts by change factor at individual TPMA level
#'
#' @param tpma_impact_data tibble, as produced by [compile_tpma_impact_data]
#' @export
make_tpma_impact_plot <- function(tpma_impact_data) {
  if (nrow(tpma_impact_data) == 0) {
    return(make_no_data_plot(no_data_reason(tpma_impact_data)))
  }
  x_axis_label <- create_measure_label(unique(tpma_impact_data[["measure"]]))
  tpma_impact_data |>
    dplyr::mutate(
      dplyr::across("change_factor", \(x) {
        sub("Beddays", "Bed days", uppercase_init(gsub("_", " ", x)))
      })
    ) |>
    ggplot2::ggplot(ggplot2::aes(.data[["value"]], .data[["tpma_label"]])) +
    ggplot2::geom_col(fill = "#2c2825") +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(5),
      labels = scales::label_comma()
    ) +
    ggplot2::labs(x = x_axis_label, y = NULL) +
    ggplot2::facet_wrap(
      dplyr::vars(.data[["change_factor"]]),
      scales = "free_y",
      axes = "all_x",
      axis.labels = "all_x",
      ncol = 1
    ) +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}
