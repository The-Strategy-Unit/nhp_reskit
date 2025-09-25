calculate_principal <- function(default_tbl, group_cols) {
  default_tbl |>
    # probably unneeded step as should already be just a single row for each
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = tidyselect::all_of(group_cols)
    ) |>
    dplyr::mutate(
      stage = dplyr::if_else(.data[["model_run"]] == 0, "baseline", "principal")
    ) |>
    dplyr::summarise(
      dplyr::across("value", mean),
      # p10 = quantile(.data[["value"]], 0.1),
      # p90 = quantile(.data[["value"]], 0.9),
      .by = tidyselect::all_of(swap_modelrun_for_stage(group_cols))
    ) |>
    tidyr::pivot_wider(names_from = "stage")
}


def_group_cols <- \(x = NULL) c("pod", "sitetret", x, "measure", "model_run")
los_group_cols <- \() def_group_cols(c("tretspef_raw", "los_group"))
swap_modelrun_for_stage <- \(x) sub("^model_run$", "stage", x)
