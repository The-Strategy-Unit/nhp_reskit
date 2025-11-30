#' Prepare data from tretspef+los_group results for displaying as summary table
#'
#' @param los_tbl the "tretspef+los_group" table from NHP results
#' @param measure Either "admissions" or "beddays". The measure to focus on for
#'  the output table
#' @inheritParams compile_principal_pod_data
#' @returns A filtered and sorted tibble of principal projections of results,
#'  by point of delivery and grouped length of stay
#' @export
compile_principal_los_data <- function(los_tbl, measure, sites = NULL) {
  summary_los_data <- prepare_site_level_principal_los_data(los_tbl, measure) |>
    filter_to_selected_sites(sites) |>
    summarise_for_all_sites() |>
    add_change_cols()

  los_groups <- unique(summary_los_data[["los_group"]])
  init_digits <- as.integer(sub("^(\\d+)(.*)", "\\1", los_groups))
  los_groups_ordered <- los_groups[match(sort(init_digits), init_digits)]

  summary_los_data |>
    dplyr::mutate(
      # display pods in desc order of baseline level of admissions/beddays
      dplyr::across("pod_label", \(x) {
        forcats::fct_reorder(x, .data[["baseline"]], sum, .desc = TRUE)
      }),
      # correctly sort LoS group factor levels numerically
      dplyr::across("los_group", \(x) forcats::fct(x, los_groups_ordered))
    ) |>
    dplyr::arrange(dplyr::pick(c("pod_label", "los_group")))
}


#' Initial preparation of data for the main LoS summary table
#'
#' @inheritParams compile_principal_los_data
#' @returns A tibble
#' @keywords internal
prepare_site_level_principal_los_data <- function(los_tbl, selected_measure) {
  los_tbl |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x == .env[["selected_measure"]])
    ) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |> # expects activity_type_label col to be present
    dplyr::select(!"activity_type_label") |>
    calculate_principal_stats(default_group_cols("los_group")) |>
    dplyr::filter(dplyr::if_any("stat", \(x) x == "mean")) |>
    dplyr::select(!"stat")
}


#' Prepare a site-level summary of main projection results by PoD and LoS
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_principal_los_data
#' @returns A tibble
#' @export
export_site_level_principal_los_data <- function(los_tbl, sites = NULL) {
  prepare_site_level_principal_los_data(los_tbl) |>
    filter_to_selected_sites(sites) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_label")))
}
