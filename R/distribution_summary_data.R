#' Compile data to support the "activity distribution summary" tables
#'
#' @inheritParams compile_principal_pod_data
#' @param value_type string Either "median" or "principal"
#' @returns A tibble
#' @export
compile_distribution_summary_data <- function(
  results,
  value_type = c("median", "principal"),
  sites = NULL
) {
  value_type <- rlang::arg_match(value_type)
  remove_col <- setdiff(c("median", "principal"), value_type)
  # fmt: skip
  arr_levels <- c(
    "Admissions", "Bed days", "Attendances", "Tele-attendances",
    "Ambulance", "Walk-in"
  )

  results[["default"]] |>
    prepare_distribution_summary_data() |>
    filter_to_selected_sites(sites) |>
    summarise_for_all_sites() |>
    tidyr::pivot_wider(names_from = "stat", values_from = "principal") |>
    dplyr::rename(principal = "mean", lower = "p10", upper = "p90") |>
    dplyr::select(!{{ remove_col }}) |>
    dplyr::mutate(
      change = .data[[value_type]] - .data[["baseline"]],
      change_pct = .data[["change"]] / .data[["baseline"]],
      .before = "lower"
    ) |>
    dplyr::mutate(dplyr::across("measure", \(x) forcats::fct(x, arr_levels))) |>
    dplyr::arrange(dplyr::across("baseline", dplyr::desc)) |>
    dplyr::arrange(dplyr::pick("measure"))
}


#' Initial preparation of site-level data for the main summary table
#'
#' @inheritParams prepare_principal_pod_data
#' @returns A tibble
#' @keywords internal
prepare_distribution_summary_data <- function(default_tbl) {
  default_tbl |>
    filter_to_main_measures() |>
    exclude_op_teleatt_procedures() |>
    inner_join_for_labels(get_detailed_pods()) |>
    dplyr::mutate(
      dplyr::across("pod_label", \(x) {
        paste0(.data[["activity_type_label"]], " ", x)
      }),
      .keep = "unused"
    ) |>
    calculate_principal_stats(default_group_cols("measure")) |>
    dplyr::mutate(
      dplyr::across("measure", \(x) {
        sub("_", "-", sub("Beddays", "Bed days", uppercase_init(x)))
      })
    )
}


#' Prepare a site-level summary of activity distribution summary data
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_distribution_summary_data
#' @returns A tibble
#' @export
export_distribution_summary_data <- function(results, sites = NULL) {
  results[["default"]] |>
    prepare_distribution_summary_data() |>
    filter_to_selected_sites(sites) |>
    tidyr::pivot_wider(names_from = "stat", values_from = "principal")
}
