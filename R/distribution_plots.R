#' Generate a "beeswarm" chart showing the distribution of model run values
#'
#' @param distrib_plot_data data frame. As produced by
#'  [compile_distribution_plot_data]
#' @param show_zero Boolean. Whether to extend the chart to show a zero value,
#'  for context.
#' @export
make_beeswarm_distrib_plot <- function(distrib_plot_data, show_zero = FALSE) {
  baseline_value <- unique(distrib_plot_data[["baseline"]])
  principal_value <- unique(distrib_plot_data[["principal"]])
  stopifnot(all(lengths(c(baseline_value, principal_value)) == 1))
  min_value <- min(distrib_plot_data[["value"]])
  min_x_value <- ifelse(show_zero, 0, min(baseline_value, min_value))
  plot_red <- "red"
  interim_plot <- distrib_plot_data |>
    ggplot2::ggplot(ggplot2::aes(.data[["value"]], 1)) +
    ggbeeswarm::geom_quasirandom(
      ggplot2::aes(
        text = glue::glue("Value: {scales::comma(value, accuracy = 1)}")
      ),
      colour = plot_red,
      orientation = "y",
      size = 3,
      alpha = 0.5
    ) |>
      # `text` aesthetic causes warnings when not used with plotly
      suppressWarnings()
  interim_plot +
    ggplot2::geom_vline(
      xintercept = baseline_value,
      colour = "grey50",
      linewidth = 1.2
    ) +
    ggplot2::geom_vline(
      xintercept = principal_value,
      linetype = "dashed",
      colour = plot_red,
      linewidth = 1.2
    ) +
    ggplot2::expand_limits(x = ifelse(show_zero, 0, min_x_value)) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_pretty(8),
      labels = scales::label_comma(),
      expand = ggplot2::expansion(0.01)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(0.15)) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      # keep y-axis labels to help line up with ecdf plot, but make 'invisible'
      axis.text.y = ggplot2::element_text(colour = "white"),
      axis.title.y = ggplot2::element_text(colour = "white")
    )
}

#' Generate a cumulative distribution function (ecdf) chart of model run values
#'
#' @inheritParams make_beeswarm_distrib_plot
#' @export
make_cumulative_distrib_plot <- function(distrib_plot_data, show_zero = FALSE) {
  baseline_value <- unique(distrib_plot_data[["baseline"]])
  principal_value <- unique(distrib_plot_data[["principal"]])
  stopifnot(all(lengths(c(baseline_value, principal_value)) == 1))
  min_value <- min(distrib_plot_data[["value"]])
  ecdf_fn <- stats::ecdf(distrib_plot_data[["value"]])
  quantiles <- c(0.1, 0.9)
  x_quantiles <- stats::quantile(ecdf_fn, quantiles)
  x_vals <- sort(distrib_plot_data[["value"]])
  y_vals <- sort(ecdf_fn(distrib_plot_data[["value"]]))
  principal_diffs <- abs(principal_value - x_vals)
  principal_match_value <- which.min(principal_diffs)
  principal_pct <- y_vals[[principal_match_value]]
  min_x_value <- ifelse(show_zero, 0, min(baseline_value, min_value))
  plot_red <- "red"
  plot_blue <- "cornflowerblue"

  line_guides <- tibble::tibble(
    x_start = c(rep(min_x_value, 3), x_quantiles, principal_value),
    x_end = rep(c(x_quantiles, principal_value), 2),
    y_start = c(quantiles, principal_pct, rep(0, 3)),
    y_end = rep(c(quantiles, principal_pct), 2),
    colour = rep(c(rep(plot_red, 2), plot_blue), 2)
  )
  interim_plot <- tibble::tibble(x = x_vals, y = y_vals) |>
    ggplot2::ggplot(ggplot2::aes(.data[["x"]], .data[["y"]])) +
    # points not really visible, but necessary for plotly tooltips to be used
    ggplot2::geom_point(
      ggplot2::aes(
        text = glue::glue(
          "Percentage: {scales::percent(y_vals, accuracy = 1)}\n",
          "Value: {scales::comma(x_vals, accuracy = 1)}"
        )
      ),
      alpha = 0.01,
      size = 0.01
    ) |>
      # `text` aesthetic causes warnings when not used with plotly
      suppressWarnings()
  interim_plot +
    ggplot2::geom_step(colour = "grey50", linewidth = 1.2) +
    ggplot2::geom_segment(
      data = line_guides,
      ggplot2::aes(
        x = .data[["x_start"]],
        y = .data[["y_start"]],
        xend = .data[["x_end"]],
        yend = .data[["y_end"]]
      ),
      # colour removed as an explicit aesthetic because plotly doesn't respect
      # `show.legend = FALSE`
      colour = line_guides[["colour"]],
      linetype = "dashed",
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(
      xintercept = baseline_value,
      colour = "grey50",
      linewidth = 1.2
    ) +
    ggplot2::expand_limits(x = min_x_value) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(8),
      labels = scales::label_comma(),
      expand = ggplot2::expansion(0.01)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.1),
      labels = scales::percent,
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::labs(y = "Percentage of model runs") +
    ggplot2::theme(axis.title.x = ggplot2::element_blank())
}
