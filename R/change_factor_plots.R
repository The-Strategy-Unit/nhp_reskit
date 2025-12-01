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


make_individual_cf_plot <- function(
  data,
  measure,
  title,
  x_axis_label
) {
  data |>
    dplyr::filter(.data[["change_factor"]] == .env[["measure"]]) |>
    dplyr::mutate(
      tooltip = scales::label_comma(1)(.data[["value"]]),
      dplyr::across("tooltip", \(x) paste0(.data[["mitigator_name"]], ": ", x))
    ) |>
    # require_rows() |> # Shiny only
    ggplot2::ggplot(
      ggplot2::aes(
        .data[["value"]],
        .data[["mitigator_name"]],
        text = .data[["tooltip"]]
      )
    ) +
    ggplot2::geom_col(fill = "#2c2825") +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(5),
      labels = scales::label_comma()
    ) +
    ggplot2::labs(title = title, x = x_axis_label, y = NULL)
}
