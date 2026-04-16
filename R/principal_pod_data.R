#' Prepare data from default results table for displaying as summary table
#'
#' @param results A named list containing NHP results tables
#' @param sites Either `NULL` (the default) or a vector of site codes to filter
#'  to. `NULL` means don't filter; include all sites present in the data
#' @returns A filtered and sorted tibble of principal projections of results,
#'  by activity type and point of delivery (PoD)
#' @export
compile_principal_pod_data <- function(results, sites = NULL) {
  init_data <- results[["default"]] |>
    prepare_principal_pod_data() |>
    filter_to_selected_sites(sites)
  if (nrow(init_data) == 0) {
    init_data
  } else {
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
}

#' Initial preparation of site-level data for the main summary table
#'
#' @param default_tbl the "default" table from NHP results
#' @returns A tibble
#' @keywords internal
prepare_principal_pod_data <- function(default_tbl) {
  default_tbl |>
    filter_to_main_measures() |>
    exclude_op_teleatt_procedures() |>
    combine_all_aae_pods() |>
    inner_join_for_labels(get_principal_pods()) |>
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
export_principal_pod_data <- function(results, sites = NULL) {
  results[["default"]] |>
    prepare_principal_pod_data() |>
    filter_to_selected_sites(sites) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_label")))
}


combine_all_aae_pods <- function(tbl) {
  dplyr::mutate(tbl, dplyr::across("pod", \(x) sub("^aae.*$", "aae", x)))
}
