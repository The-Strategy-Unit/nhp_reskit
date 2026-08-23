#' Prepare data from default results table for displaying as summary table
#'
#' @param results A named list containing NHP results tables
#' @param pod_lookup A tibble, or a function that returns a tibble, containing
#'  columns named `activity_type_label`, `pod` and `pod_label`. This provides
#'  friendly labels for POD variables in the data. `pod` is the key column used
#'  for joining to data tables.
#' @param sites Either `NULL` (the default) or a vector of site codes to filter
#'  to. `NULL` means don't filter; include all sites present in the data
#' @returns A filtered and sorted tibble of principal projections of results,
#'  by activity type and point of delivery (PoD)
#' @export
compile_principal_pod_data <- function(
  results,
  pod_lookup = get_principal_pods(),
  sites = NULL
) {
  # Guard on the user's selection before doing any preparation work: an
  # unmatched `sites` value is much the most likely source of an empty result.
  # Every grouping in prepare_principal_pod_data() includes `sitetret`, so
  # filtering here rather than afterwards leaves the statistics unchanged.
  selected_data <- filter_to_selected_sites(results[["default"]], sites)
  if (nrow(selected_data) == 0) {
    return(empty_result(
      proto_principal_pod_data(),
      "No principal PoD data for the selected sites."
    ))
  }

  # A second guard is still needed: prepare_principal_pod_data() drops rows
  # via filter_to_main_measures() and keep_mean_only(), so it can empty a
  # non-empty input.
  init_data <- prepare_principal_pod_data(selected_data, pod_lookup)
  if (nrow(init_data) == 0) {
    return(empty_result(
      proto_principal_pod_data(),
      "No main-measure activity found in the `default` results table."
    ))
  }

  at_levels <- c(
    "Inpatient Admissions",
    "Inpatient Bed Days",
    "Outpatient",
    "A&E"
  )
  init_data |>
    summarise_for_all_sites() |>
    add_change_cols() |>
    dplyr::mutate(
      dplyr::across("activity_type_label", \(x) forcats::fct(x, at_levels)),
      # display pods in descending order of baseline value, by activity type
      dplyr::across("pod_label", \(x) {
        forcats::fct_reorder(x, .data[["baseline"]], sum, .desc = TRUE)
      })
    ) |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_label")))
}

#' Zero-row prototype for the [compile_principal_pod_data] output
#'
#' The column names and types here must match what [compile_principal_pod_data]
#'  returns when rows are present; `test-empty_results.R` asserts this.
#' @returns A zero-row tibble
#' @keywords internal
proto_principal_pod_data <- function() {
  tibble::tibble(
    pod_label = factor(),
    activity_type_label = factor(),
    baseline = numeric(),
    principal = numeric(),
    change = numeric(),
    change_pct = numeric()
  )
}


#' Initial preparation of site-level data for the main summary table
#'
#' @param default_tbl the "default" table from NHP results
#' @returns A tibble
#' @keywords internal
prepare_principal_pod_data <- function(default_tbl, pod_lookup) {
  default_tbl |>
    filter_to_main_measures() |>
    exclude_op_teleatt_procedures() |>
    combine_all_aae_pods() |>
    join_for_labels(pod_lookup) |>
    relabel_pods() |>
    relabel_ip_activity_types() |>
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = tidyselect::all_of(default_group_cols("activity_type_label"))
    ) |>
    calculate_principal_stats(default_group_cols("activity_type_label")) |>
    keep_mean_only()
}


#' Prepare a site-level summary of main projection results
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_principal_pod_data
#' @returns A tibble
#' @export
export_principal_pod_data <- function(
  results,
  pod_lookup = get_principal_pods(),
  sites = NULL
) {
  results[["default"]] |>
    prepare_principal_pod_data(pod_lookup) |>
    filter_to_selected_sites(sites) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_label")))
}


combine_all_aae_pods <- function(tbl) {
  dplyr::mutate(tbl, dplyr::across("pod", \(x) sub("^aae.*$", "aae", x)))
}
