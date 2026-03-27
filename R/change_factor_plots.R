#' Generate overall change factor ("waterfall") chart
#'
#' @param principal_change_factor_data data frame. As produced by
#'  [compile_change_factor_data]
#' @export
make_overall_cf_plot <- function(principal_change_factor_data) {
  principal_change_factor_data |>
    dplyr::mutate(
      colour = dplyr::case_when(
        .data[["change_factor"]] == "baseline" ~ "#686f73",
        .data[["change_factor"]] == "estimate" ~ "#ec6555",
        .data[["value"]] >= 0 ~ "#f9bf07",
        .default = "#2c2825"
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data[["hidden"]],
        xend = .data[["total"]],
        y = .data[["change_factor"]],
        yend = .data[["change_factor"]],
        colour = .data[["colour"]]
      ),
      # dynamic: bigger if fewer bars (130 is relative to 600px plot height)
      lwd = 130 / nrow(principal_change_factor_data)
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(5),
      labels = scales::label_comma()
    ) +
    ggplot2::scale_y_discrete(limits = rev, labels = snakecase::to_title_case) +
    ggplot2::labs(x = NULL, y = NULL)
}


#' Generate bar charts by change factor at individual TPMA level
#'
#' @param indiv_change_factor_data data frame. As produced by
#'  [compile_indiv_change_factor_data]
#' @export
make_individual_cf_plot <- function(indiv_change_factor_data) {
  prepared_dat <- indiv_change_factor_data |>
    dplyr::mutate(
      dplyr::across(c("measure", "change_factor"), \(x) {
        uppercase_init(sub("_", " ", x))
      })
    )
  x_axis_label <- unique(prepared_dat[["measure"]])
  prepared_dat |>
    ggplot2::ggplot(ggplot2::aes(.data[["value"]], .data[["tpma_label"]])) +
    ggplot2::geom_col(fill = "#2c2825") +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(5),
      labels = scales::label_comma()
    ) +
    ggplot2::labs(x = x_axis_label, y = NULL) +
    ggplot2::facet_wrap(
      dplyr::vars(.data[["change_factor"]]),
      scales = "free_y",
      axes = "all_x",
      axis.labels = "all_x",
      ncol = 1
    )
}
