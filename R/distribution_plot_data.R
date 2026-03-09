#' Compile data to support the "activity distribution summary" tables
#'
#' @inheritParams compile_principal_pod_data
#' @inheritParams compile_change_factor_data
#' @returns A tibble
#' @export
compile_distribution_plot_data <- function(
  results,
  measure,
  activity_type = c("ip", "op", "aae"),
  pods = NULL,
  sites = NULL
) {
  activity_type <- rlang::arg_match(activity_type)
  results[["default"]] |>
    get_activity_type_from_pod() |>
    filter_principal_data(measure, activity_type, pods) |>
    prepare_distribution_plot_data() |>
    filter_to_selected_sites(sites) |>
    dplyr::summarise(
      dplyr::across(c("value", "baseline", "principal"), sum),
      .by = "model_run"
    )
}

#' Preparation of site-level data for the main summary table
#'
#' @inheritParams prepare_principal_cf_data
#' @returns A tibble
#' @keywords internal
prepare_distribution_plot_data <- function(dat) {
  key_cols <- c("measure", "activity_type_label")
  group_cols <- default_group_cols(key_cols)
  fill_cols <- c("pod_label", "sitetret", key_cols)

  dat |>
    filter_to_main_measures() |>
    exclude_op_teleatt_procedures() |>
    inner_join_for_labels(get_detailed_pods()) |>
    check_single_row_groups(group_cols) |>
    dplyr::mutate(
      stage = dplyr::if_else(.data[["model_run"]] == 0, "baseline", "principal")
    ) |>
    dplyr::mutate(
      mean = mean(.data[["value"]]),
      .by = tidyselect::all_of(swap_modelrun_for_stage(group_cols))
    ) |>
    tidyr::pivot_wider(names_from = "stage", values_from = "mean") |>
    tidyr::fill("baseline", .by = tidyselect::all_of(fill_cols)) |>
    dplyr::filter(dplyr::if_any("model_run", \(x) x > 0)) |>
    dplyr::mutate(dplyr::across("model_run", forcats::as_factor))
}
