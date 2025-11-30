#' Calculate mean, median, p10 and p90 values from 256 projection model runs
#'
#' @param tbl A suitably prepared results data table
#' @param group_cols Set of variables to group by, varying slightly for
#'  different inputs/outputs
#' @returns A table with baseline values and principal projection summary values
#' @keywords internal
calculate_principal_stats <- function(tbl, group_cols) {
  summary_cols <- c("mean", "median", "p10", "p90")
  tbl |>
    # probably unneeded step as should already be just a single row for each
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = tidyselect::all_of(group_cols)
    ) |>
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
    tidyr::pivot_longer(tidyselect::any_of(summary_cols), names_to = "stat") |>
    tidyr::pivot_wider(names_from = "stage")
}


default_group_cols <- \(x = NULL) c("pod_label", "sitetret", x, "model_run")
detailed_group_cols <- \(x) c("sitetret", "sex", x, "model_run")
swap_modelrun_for_stage <- \(x) sub("^model_run$", "stage", x)
