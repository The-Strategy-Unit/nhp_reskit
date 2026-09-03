#' Prepare data from tretspef+los_group results for displaying as summary table
#'
#' @param measure Either "admissions" or "beddays". The measure to focus on for
#'  the output table
#' @inheritParams compile_principal_pod_data
#' @returns A filtered and sorted tibble of principal projections of results,
#'  by point of delivery and grouped length of stay
#' @export
compile_principal_los_data <- function(
  results,
  measure,
  pod_lookup = get_principal_pods(),
  sites = NULL
) {
  check_measure(measure)
  los_tbl <- results[["tretspef+los_group"]]
  # Guard against an unmatched `sites` value producing an empty result
  selected_data <- filter_to_selected_sites(los_tbl, sites)
  if (nrow(selected_data) == 0) {
    return(empty_result(
      proto_principal_los_data(),
      "No principal LoS data for the selected sites."
    ))
  }
  # Guard against filter steps producing a zero-row tibble
  filtered_data <- selected_data |>
    dplyr::filter(dplyr::if_any("measure", \(x) x %in% .env[["measure"]]))
  if (nrow(filtered_data) == 0) {
    return(empty_result(
      proto_principal_los_data(),
      "No principal LoS data for the selected measure."
    ))
  }
  summary_los_data <- filtered_data |>
    prepare_principal_los_data(pod_lookup) |>
    # measure not previously dropped purely because it's useful for export*()
    dplyr::select(!"measure") |>
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

#' Zero-row prototype for the [compile_principal_los_data] output
#'
#' The column names and types here must match what [compile_principal_los_data]
#'  returns when rows are present; `test-empty_results.R` asserts this.
#' @returns A zero-row tibble
#' @keywords internal
proto_principal_los_data <- function() {
  tibble::tibble(
    pod_label = factor(),
    los_group = factor(),
    baseline = numeric(),
    principal = numeric(),
    change = numeric(),
    change_pct = numeric()
  )
}


#' Preparation of site-level data for the main LoS summary table
#'
#' @inheritParams compile_change_factor_data
#' @returns A tibble
#' @keywords internal
prepare_principal_los_data <- function(filtered_data, pod_lookup) {
  grp_cols <- default_group_cols(c("measure", "los_group"))
  filtered_data |>
    join_for_labels(pod_lookup) |>
    relabel_pods() |>
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = tidyselect::all_of(grp_cols)
    ) |>
    calculate_principal_stats(grp_cols) |>
    keep_mean_only()
}


#' Prepare a site-level summary of main projection results by PoD and LoS
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_principal_los_data
#' @returns A tibble
#' @export
export_principal_los_data <- function(
  results,
  pod_lookup = get_principal_pods(),
  sites = NULL
) {
  los_tbl <- results[["tretspef+los_group"]]
  # Guard against an unmatched `sites` value producing an empty result
  selected_data <- filter_to_selected_sites(los_tbl, sites)
  if (nrow(selected_data) == 0) {
    return(empty_result(
      proto_principal_los_data(),
      "No principal LoS data for the selected sites."
    ))
  }
  selected_data |>
    prepare_principal_los_data(pod_lookup) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick("pod_label"))
}
