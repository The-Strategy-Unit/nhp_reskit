calculate_default_principal <- function(default_tbl) {
  default_tbl |>
    # probably unneeded step as should already be just a single row for each
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = tidyselect::all_of(def_group_cols())
    ) |>
    dplyr::mutate(
      stage = dplyr::if_else(.data[["model_run"]] == 0, "baseline", "principal")
    ) |>
    dplyr::summarise(
      dplyr::across("value", mean),
      # p10 = quantile(.data[["value"]], 0.1),
      # p90 = quantile(.data[["value"]], 0.9),
      .by = tidyselect::all_of(swapmrun4stage(def_group_cols()))
    ) |>
    tidyr::pivot_wider(names_from = "stage")
}


calculate_los_principal <- function(los_tbl) {
  los_tbl |>
    # probably unneeded step as should already be just a single row for each
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = tidyselect::all_of(los_group_cols())
    ) |>
    dplyr::mutate(
      stage = dplyr::if_else(.data[["model_run"]] == 0, "baseline", "principal")
    ) |>
    dplyr::summarise(
      dplyr::across("value", mean),
      # p10 = quantile(.data[["value"]], 0.1),
      # p90 = quantile(.data[["value"]], 0.9),
      .by = tidyselect::all_of(swapmrun4stage(los_group_cols()))
    ) |>
    tidyr::pivot_wider(names_from = "stage")
}


def_group_cols <- \(x = NULL) c("pod", "sitetret", x, "measure", "model_run")
los_group_cols <- \() def_group_cols(c("tretspef_raw", "los_group"))
swapmrun4stage <- \(x) sub("^model_run$", "stage", x)
