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
  dat,
  measure,
  include_baseline
) {
  bsline_filtered <- dplyr::filter(dat, .data[["change_factor"]] != "baseline")
  dat_prepared <- if (include_baseline) dat else bsline_filtered

  measure_data <- dat_prepared |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x == {{ measure }}) &
        dplyr::if_any("value", \(x) x != 0)
    ) |>
    dplyr::summarise(dplyr::across("value", sum), .by = "change_factor") |>
    dplyr::mutate(
      dplyr::across("change_factor", \(x) {
        forcats::fct_reorder(x, .data[["value"]], .desc = TRUE)
      })
    )

  bl_first <- measure_data |>
    dplyr::mutate(
      # baseline may now not be the first item, move it back to start
      dplyr::across("change_factor", \(x) forcats::fct_relevel(x, "baseline"))
    )
  measure_data <- if (include_baseline) bl_first else measure_data

  estimate_row <- tibble::tibble_row(
    change_factor = "Estimate",
    hidden = 0,
    total = sum(measure_data[["value"]])
  )

  measure_data |>
    dplyr::mutate(
      cuvalue = cumsum(.data[["value"]]),
      hidden = dplyr::lag(.data[["cuvalue"]], 1, 0) + pmin(.data[["value"]], 0)
    ) |>
    dplyr::select(c("change_factor", "hidden", total = "value")) |>
    dplyr::bind_rows(estimate_row)
  # tidyr::pivot_longer(c("value", "hidden"))
}


principal_change_factor_effects_plot <- function(pcfe_data) {
  pcfe_data |>
    dplyr::mutate(
      colour = dplyr::case_when(
        .data[["change_factor"]] == "baseline" ~ "#686f73",
        .data[["change_factor"]] == "Estimate" ~ "#ec6555",
        .data[["value"]] >= 0 ~ "#f9bf07",
        .default = "#2c2825"
      ),
      dplyr::across("value", abs), # ?
      # dplyr::across("name", \(x) forcats::fct(x, c("hidden", "value"))),
      dplyr::across("change_factor", forcats::fct_rev),
      dplyr::across("change_factor", \(x) forcats::fct_relevel(x, "Estimate")),
      dplyr::across("total", \(x) x + .data[["hidden"]])
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["hidden"]],
        xend = .data[["total"]],
        y = .data[["change_factor"]],
        yend = .data[["change_factor"]],
        colour = .data[["colour"]]
      )
    ) +
    ggplot2::geom_segment(
      # dynamic: bigger if fewer bars (130 is relative to 600px plot height)
      lwd = 130 / nrow(pcfe_data)
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(5),
      labels = scales::label_comma()
    ) +
    ggplot2::scale_y_discrete(labels = snakecase::to_title_case) +
    ggplot2::labs(x = NULL, y = NULL)
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
