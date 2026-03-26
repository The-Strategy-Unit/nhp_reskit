#' Calculate mean, median, p10 and p90 values from 256 projection model runs
#'
#' @param tbl A suitably prepared results data table
#' @param group_cols Set of variables to group by, varying slightly for
#'  different inputs/outputs
#' @returns A table with baseline values and principal projection summary values
#' @keywords internal
calculate_principal_stats <- function(tbl, group_cols) {
  stat_cols <- c("mean", "median", "p10", "p90")
  tbl |>
    check_single_row_groups(group_cols) |>
    dplyr::mutate(
      stage = dplyr::if_else(.data[["model_run"]] == 0, "baseline", "principal")
    ) |>
    dplyr::summarise(
      mean = mean(.data[["value"]]),
      median = stats::quantile(.data[["value"]], 0.5),
      p10 = stats::quantile(.data[["value"]], 0.1),
      p90 = stats::quantile(.data[["value"]], 0.9),
      .by = tidyselect::all_of(swap_modelrun_for_stage(group_cols))
    ) |>
    tidyr::pivot_longer(tidyselect::any_of(stat_cols), names_to = "stat") |>
    tidyr::pivot_wider(names_from = "stage")
}


default_group_cols <- \(x = NULL) c("pod_label", "sitetret", x, "model_run")
swap_modelrun_for_stage <- \(x) sub("^model_run$", "stage", x)


check_single_row_groups <- function(tbl, group_cols) {
  out <- tbl |>
    dplyr::summarise(n = dplyr::n(), .by = tidyselect::all_of(group_cols))
  csrg <- "check_single_row_groups"
  msg <- "{.fn {csrg}}: The grouping columns do not group to single rows"
  azkit::check_that(out, \(x) all(x[["n"]] == 1L), msg)
  tbl
}
