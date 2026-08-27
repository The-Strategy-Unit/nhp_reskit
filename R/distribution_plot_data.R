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
  pod_lookup = get_detailed_pods(),
  sites = NULL
) {
  check_measure(measure)
  activity_type <- rlang::arg_match(activity_type)
  # Guard against an unmatched `sites` value producing an empty result
  selected_sites_data <- filter_to_selected_sites(results[["default"]], sites)
  if (nrow(selected_sites_data) == 0) {
    return(empty_result(
      proto_distribution_plot_data(),
      "No results data for the selected sites."
    ))
  }
  # Guard against filter steps producing a zero-row tibble
  filtered_data <- selected_sites_data |>
    get_activity_type_from_pod() |>
    filter_principal_data(measure, activity_type, pods)
  if (nrow(filtered_data) == 0) {
    empty_result(
      proto_distribution_plot_data(),
      "No results data for the selected measure/activity type/pods."
    )
  } else {
    filtered_data |>
      prepare_distribution_plot_data(pod_lookup) |>
      dplyr::summarise(
        dplyr::across(c("value", "baseline", "principal"), sum),
        .by = "model_run"
      )
  }
}


#' Zero-row prototype for the [compile_distribution_plot_data] output
#'
#' The column names and types here must match what
#'  [compile_distribution_plot_data] returns when rows are present;
#'  `test-empty_results.R` asserts this.
#' @returns A zero-row tibble
#' @keywords internal
proto_distribution_plot_data <- function() {
  tibble::tibble(
    model_run = integer(),
    value = integer(),
    baseline = numeric(),
    principal = numeric()
  )
}

#' Preparation of site-level data for the main summary table
#'
#' @inheritParams compile_change_factor_data
#' @returns A tibble
#' @keywords internal
prepare_distribution_plot_data <- function(dat, pod_lookup) {
  key_cols <- c("measure", "activity_type_label")
  group_cols <- default_group_cols(key_cols)
  fill_cols <- c("pod_label", "sitetret", key_cols)

  dat |>
    filter_to_main_measures() |>
    exclude_op_teleatt_procedures() |>
    join_for_labels(pod_lookup) |>
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
    dplyr::filter(dplyr::if_any("model_run", \(x) x > 0))
}
