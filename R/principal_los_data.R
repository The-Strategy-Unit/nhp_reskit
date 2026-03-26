#' Prepare data from tretspef+los_group results for displaying as summary table
#'
#' @param measure Either "admissions" or "beddays". The measure to focus on for
#'  the output table
#' @inheritParams compile_principal_pod_data
#' @returns A filtered and sorted tibble of principal projections of results,
#'  by point of delivery and grouped length of stay
#' @export
compile_principal_los_data <- function(results, measure, sites = NULL) {
  summary_los_data <- results[["tretspef+los_group"]] |>
    dplyr::filter(dplyr::if_any("measure", \(x) x == .env[["measure"]])) |>
    filter_to_selected_sites(sites) |>
    prepare_principal_los_data() |>
    summarise_for_all_sites() |>
    add_change_cols()

  los_groups <- unique(summary_los_data[["los_group"]])
  init_digits <- as.integer(sub("^(\\d+)(.*)", "\\1", los_groups))
  los_groups_ordered <- los_groups[match(sort(init_digits), init_digits)]

  summary_los_data |>
    dplyr::select(!c("activity_type_label", "measure")) |>
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


#' Preparation of site-level data for the main LoS summary table
#'
#' @inheritParams prepare_principal_cf_data
#' @returns A tibble
#' @keywords internal
prepare_principal_los_data <- function(dat) {
  grp_by <- default_group_cols(c("activity_type_label", "measure", "los_group"))
  dat |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |>
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = tidyselect::all_of(grp_by)
    ) |>
    calculate_principal_stats(grp_by) |>
    keep_mean_only()
}


#' Prepare a site-level summary of main projection results by PoD and LoS
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_principal_los_data
#' @returns A tibble
#' @export
export_principal_los_data <- function(results, sites = NULL) {
  results[["tretspef+los_group"]] |>
    filter_to_selected_sites(sites) |>
    prepare_principal_los_data() |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_label")))
}
