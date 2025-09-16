principal_detailed_table <- function(data, aggregation, final_year) {
  data |>
    dplyr::mutate(
      dplyr::across("sex", \(.x) ifelse(.x == 1, "Male", "Female")),
      dplyr::across(
        "final",
        \(.x) gt_bar(.x, scales::comma_format(1), "#686f73", "#686f73")
      ),
      dplyr::across("change", \(.x) gt_bar(.x, scales::comma_format(1))),
      dplyr::across("change_pcnt", \(.x) gt_bar(.x, scales::percent_format(1)))
    ) |>
    gt::gt(groupname_col = "sex") |>
    gt::cols_label(
      agg = dplyr::case_match(
        aggregation,
        "age_group" ~ "Age Group",
        "tretspef" ~ "Treatment Specialty",
        .default = aggregation
      ),
      baseline = "Baseline",
      final = paste0("Final (", final_year, ")"),
      change = "Change",
      change_pcnt = "Percent Change",
    ) |>
    gt::fmt_integer(c("baseline")) |>
    gt::cols_width(
      .data$final ~ gt::px(150),
      .data$change ~ gt::px(150),
      .data$change_pcnt ~ px(150)
    ) |>
    gt::cols_align(
      align = "left",
      columns = c("agg", "final", "change", "change_pcnt")
    ) |>
    gt_theme()
}


principal_change_factor_effects_summarised <- function(
  data,
  measure,
  include_baseline
) {
  data <- data |>
    dplyr::filter(
      .data$measure == .env$measure,
      include_baseline | .data$change_factor != "baseline",
      .data$value != 0
    ) |>
    tidyr::drop_na("value") |>
    dplyr::mutate(
      dplyr::across(
        "change_factor",
        \(.x) forcats::fct_reorder(.x, -.data$value)
      ),
      # baseline may now not be the first item, move it back to start
      dplyr::across(
        "change_factor",
        \(.x) forcats::fct_relevel(.x, "baseline")
      )
    )

  cfs <- data |>
    dplyr::group_by(.data$change_factor) |>
    dplyr::summarise(dplyr::across("value", \(.x) sum(.x, na.rm = TRUE))) |>
    dplyr::mutate(cuvalue = cumsum(.data$value)) |>
    dplyr::mutate(
      hidden = tidyr::replace_na(
        dplyr::lag(.data$cuvalue) + pmin(.data$value, 0),
        0
      ),
      colour = dplyr::case_when(
        .data$change_factor == "Baseline" ~ "#686f73",
        .data$value >= 0 ~ "#f9bf07",
        TRUE ~ "#2c2825"
      ),
      dplyr::across("value", abs)
    ) |>
    dplyr::select(-"cuvalue")

  levels <- unique(c(
    "baseline",
    levels(forcats::fct_drop(cfs$change_factor)),
    "Estimate"
  ))
  if (!include_baseline) {
    levels <- levels[-1]
  }

  cfs |>
    dplyr::bind_rows(
      dplyr::tibble(
        change_factor = "Estimate",
        value = sum(data$value),
        hidden = 0,
        colour = "#ec6555"
      )
    ) |>
    tidyr::pivot_longer(c("value", "hidden")) |>
    dplyr::mutate(
      dplyr::across("colour", \(.x) ifelse(.data$name == "hidden", NA, .x)),
      dplyr::across("name", \(.x) forcats::fct_relevel(.x, "hidden", "value")),
      dplyr::across("change_factor", \(.x) factor(.x, rev(levels)))
    )
}


principal_change_factor_effects_cf_plot <- function(data) {
  # Reorient data for geom_segment
  data_reoriented <- data |>
    tidyr::pivot_wider(
      id_cols = tidyselect::all_of("change_factor"),
      names_from = tidyselect::all_of("name"),
      values_from = tidyselect::all_of("value")
    ) |>
    dplyr::mutate(colour = data[["colour"]][!is.na(data[["colour"]])])

  data_reoriented |>
    dplyr::mutate(
      xstart = .data[["hidden"]],
      xend = .data[["hidden"]] + .data[["value"]]
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["xstart"]],
        xend = .data[["xend"]],
        y = .data[["change_factor"]],
        yend = .data[["change_factor"]], # plotly errors if yend not included
        colour = .data[["colour"]]
      )
    ) +
    ggplot2::geom_segment(
      # dynamic: bigger if fewer bars (130 is relative to 600px plot height)
      lwd = 130 / nrow(data_reoriented)
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(5),
      labels = scales::comma
    ) +
    ggplot2::scale_y_discrete(labels = snakecase::to_title_case) +
    ggplot2::labs(x = "", y = "")
}


principal_change_factor_effects_ind_plot <- function(
  data,
  change_factor,
  colour,
  title,
  x_axis_label
) {
  data |>
    dplyr::filter(.data$change_factor == .env$change_factor) |>
    dplyr::mutate(
      tooltip = glue::glue(
        "{mitigator_name}: {scales::comma(value, accuracy = 1)}"
      )
    ) |>
    # require_rows() |> # Shiny only
    ggplot2::ggplot(
      ggplot2::aes(.data$value, .data$mitigator_name, text = .data[["tooltip"]])
    ) +
    ggplot2::geom_col(fill = "#2c2825") +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(5),
      labels = scales::comma
    ) +
    ggplot2::labs(title = title, x = x_axis_label, y = "")
}
