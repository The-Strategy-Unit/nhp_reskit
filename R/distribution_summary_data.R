#' Compile data to support the "activity distribution summary" tables
#'
#' @inheritParams compile_principal_pod_data
#' @param value_type string Either "median" or "principal"
#' @returns A tibble
#' @export
compile_distribution_summary_data <- function(
  results,
  value_type = c("median", "principal"),
  pod_lookup = get_detailed_pods(),
  sites = NULL
) {
  value_type <- rlang::arg_match(value_type)
  remove_col <- setdiff(c("median", "principal"), value_type)

  # Guard against an unmatched `sites` value producing an empty result
  selected_sites_data <- filter_to_selected_sites(results[["default"]], sites)
  if (nrow(selected_sites_data) == 0) {
    return(empty_result(
      proto_distribution_summary_data(value_type),
      "No results data for the selected sites."
    ))
  }

  # fmt: skip
  m_levels <- c(
      "Admissions", "Bed days", "Attendances", "Tele-attendances",
      "Ambulance", "Walk-in"
    )
  selected_sites_data |>
    prepare_distribution_summary_data(pod_lookup) |>
    summarise_for_all_sites() |>
    tidyr::pivot_wider(names_from = "stat", values_from = "principal") |>
    dplyr::rename(principal = "mean", lower = "p10", upper = "p90") |>
    dplyr::select(!{{ remove_col }}) |>
    dplyr::mutate(
      change = .data[[value_type]] - .data[["baseline"]],
      change_pct = .data[["change"]] / .data[["baseline"]],
      .before = "lower"
    ) |>
    dplyr::mutate(dplyr::across("measure", \(x) forcats::fct(x, m_levels))) |>
    dplyr::arrange(dplyr::desc(dplyr::pick("baseline"))) |>
    dplyr::arrange(dplyr::pick("measure"))
}


#' Zero-row prototype for the [compile_distribution_summary_data] output
#'
#' The column names and types here must match what
#'  [compile_distribution_summary_data] returns when rows are present;
#'  `test-empty_results.R` asserts this.
#' @returns A zero-row tibble
#' @keywords internal
proto_distribution_summary_data <- function(value_type = "median") {
  tibble::tibble(
    pod_label = factor(),
    activity_type_label = factor(),
    measure = factor(),
    baseline = numeric(),
    !!value_type := numeric(),
    change = numeric(),
    change_pct = numeric(),
    lower = numeric(),
    upper = numeric()
  )
}

#' Initial preparation of site-level data for the main summary table
#'
#' @inheritParams prepare_principal_pod_data
#' @returns A tibble
#' @keywords internal
prepare_distribution_summary_data <- function(default_tbl, pod_lookup) {
  grp_cols <- c("activity_type_label", "measure")
  default_tbl |>
    filter_to_main_measures() |>
    exclude_op_teleatt_procedures() |>
    join_for_labels(pod_lookup) |>
    calculate_principal_stats(default_group_cols(grp_cols)) |>
    dplyr::mutate(
      dplyr::across("measure", \(x) {
        sub("Beddays", "Bed days", uppercase_init(gsub("_", "-", x)))
      })
    )
}


#' Prepare a site-level summary of activity distribution summary data
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_distribution_summary_data
#' @returns A tibble
#' @export
export_distribution_summary_data <- function(
  results,
  pod_lookup = get_detailed_pods(),
  sites = NULL
) {
  # Guard against an unmatched `sites` value producing an empty result
  selected_sites_data <- filter_to_selected_sites(results[["default"]], sites)
  if (nrow(selected_sites_data) == 0) {
    return(empty_result(
      proto_distribution_summary_data(),
      "No results data for the selected sites."
    ))
  }
  selected_sites_data |>
    prepare_distribution_summary_data(pod_lookup) |>
    filter_to_selected_sites(sites) |>
    tidyr::pivot_wider(names_from = "stat", values_from = "principal")
}
