#' Prepare data from the `step_counts` results table for display as charts
#'
#' @param measure The measure to focus on for the output table. Valid values
#'  depend on which activity_type is selected
#' @param activity_type string. One of "ip", "op", "aae". "ip" is the default.
#' @param pods character vector. PoD labels to filter data to. The default
#'  value of `NULL` means no PoDs will be filtered out
#' @param tpma_lookup A tibble, or a function that returns a tibble, containing
#'  a column named `strategy` (used as a key for joining to the `step_counts`
#'  table) and a column named `tpma_label` that provides friendly labels for
#'  all TPMAs/strategies
#' @param include_baseline Boolean. Whether to include baseline data
#' @inheritParams compile_principal_los_data
#' @returns A prepared tibble of step count changes for each included TPMA
#' @export
compile_change_factor_data <- function(
  results,
  measure,
  activity_type = c("ip", "op", "aae"),
  pods = NULL,
  sites = NULL,
  pod_lookup = get_detailed_pods(),
  tpma_lookup = get_tpma_label_lookup(),
  include_baseline = TRUE
) {
  check_measure(measure)
  activity_type <- rlang::arg_match(activity_type)
  sc_tbl <- results[["step_counts"]]
  # Guard against an unmatched `sites` value producing an empty result
  selected_sites_data <- filter_to_selected_sites(sc_tbl, sites)
  if (nrow(selected_sites_data) == 0) {
    return(empty_result(
      proto_change_factor_data(),
      "No step count data for the selected sites."
    ))
  }
  # Guard against filter steps producing a zero-row tibble
  filtered_data <- selected_sites_data |>
    filter_principal_data(measure, activity_type, pods)
  if (nrow(filtered_data) == 0) {
    return(empty_result(
      proto_change_factor_data(),
      "No step count data for the selected measure/activity type/pods."
    ))
  }

  interim <- filtered_data |>
    prepare_change_factor_data(pod_lookup, tpma_lookup) |>
    summarise_for_all_sites() |>
    dplyr::summarise(
      dplyr::across("value", sum),
      # altho we've already filtered to 1 measure, we need to retain the column
      .by = c("change_factor", "activity_type_label", "measure")
    ) |>
    # Here we need to sort by decreasing value (biggest increases in activity
    # (+ve 'value's) at the top), and then we need to ensure that the 'baseline'
    # row, if any, is at the top so that the cumsum() step works correctly.
    dplyr::arrange(dplyr::desc(dplyr::pick("value")))
  if (include_baseline) {
    next_tbl <- move_baseline_row_to_top(interim)
  } else {
    next_tbl <- dplyr::filter(interim, .data[["change_factor"]] != "baseline")
  }
  estimate_row <- tibble::tibble_row(
    change_factor = "estimate",
    activity_type_label = unique(next_tbl[["activity_type_label"]]),
    measure = .env[["measure"]],
    value = sum(next_tbl[["value"]]),
    hide = 0,
    total = .data[["value"]]
  )
  next_tbl |>
    dplyr::mutate(
      cmvalue = cumsum(.data[["value"]]),
      hide = dplyr::lag(.data[["cmvalue"]], 1, 0) + pmin(.data[["value"]], 0),
      total = abs(.data[["value"]]) + .data[["hide"]]
    ) |>
    dplyr::select(!"cmvalue") |>
    dplyr::bind_rows(estimate_row) |>
    dplyr::mutate(dplyr::across("change_factor", forcats::fct_inorder))
}


#' Prepare data from `step_counts` results table for display as charts
#'
#' @inheritParams compile_change_factor_data
#' @param sort_by string, one of "value" or "tpma_label". The former sorts
#'  the output table by the value of the change, the latter alphabetically by
#'  the TPMA label
#' @returns A prepared tibble of projected negative changes in activity, by TPMA
#' @export
compile_tpma_impact_data <- function(
  results,
  measure,
  activity_type = c("ip", "op", "aae"),
  pods = NULL,
  sites = NULL,
  pod_lookup = get_detailed_pods(),
  tpma_lookup = get_tpma_label_lookup(),
  sort_by = c("value", "tpma_label")
) {
  check_measure(measure)
  activity_type <- rlang::arg_match(activity_type)
  sort_by <- rlang::arg_match(sort_by)
  impact_factors <- c("activity_avoidance", "efficiencies")
  sc_tbl <- results[["step_counts"]]

  # Guard against an unmatched `sites` value producing an empty result
  selected_sites_data <- filter_to_selected_sites(sc_tbl, sites)
  if (nrow(selected_sites_data) == 0) {
    return(empty_result(
      proto_tpma_impact_data(),
      "No TPMA impact data for the selected sites."
    ))
  }
  # Guard against filter steps producing a zero-row tibble
  filtered_data <- selected_sites_data |>
    filter_principal_data(measure, activity_type, pods)
  if (nrow(filtered_data) == 0) {
    return(empty_result(
      proto_tpma_impact_data(),
      "No TPMA impact data for the selected measure/activity type/pods."
    ))
  }

  interim_data <- filtered_data |>
    prepare_change_factor_data(pod_lookup, tpma_lookup) |>
    summarise_for_all_sites() |>
    dplyr::summarise(
      dplyr::across("value", sum),
      # altho we've already filtered to 1 measure, we need to retain the column
      .by = c("change_factor", "activity_type_label", "measure", "tpma_label")
    ) |>
    dplyr::filter(
      dplyr::if_any("change_factor", \(x) x %in% {{ impact_factors }}),
      dplyr::if_any("tpma_label", \(x) x != "-"),
      # we only want to show TPMAs that _reduce_ the activity measure
      dplyr::if_any("value", \(x) x < 0)
    )
  # Return an empty tibble and a message if no TPMAs had an impact
  if (nrow(interim_data) == 0) {
    return(empty_result(
      proto_tpma_impact_data(),
      "No TPMAs had an impact on activity for this set of parameters."
    ))
  }

  interim_data |>
    dplyr::arrange(dplyr::desc(dplyr::pick(tidyselect::all_of(sort_by)))) |>
    dplyr::mutate(dplyr::across("tpma_label", forcats::fct_inorder))
}


#' Zero-row prototype for the [compile_change_factor_data] output
#'
#' The column names and types here must match what [compile_change_factor_data]
#'  returns when rows are present; `test-empty_results.R` asserts this.
#' @returns A zero-row tibble
#' @keywords internal
proto_change_factor_data <- function() {
  tibble::tibble(
    change_factor = factor(),
    activity_type_label = factor(),
    measure = character(),
    value = numeric(),
    hide = numeric(),
    total = numeric()
  )
}


#' Zero-row prototype for the [compile_tpma_impact_data] output
#'
#' The column names and types here must match what [compile_tpma_impact_data]
#'  returns when rows are present; `test-empty_results.R` asserts this.
#' @returns A zero-row tibble
#' @keywords internal
proto_tpma_impact_data <- function() {
  tibble::tibble(
    change_factor = character(),
    activity_type_label = factor(),
    measure = character(),
    tpma_label = factor(),
    value = numeric()
  )
}


prepare_change_factor_data <- function(filtered_data, pod_lookup, tpma_lookup) {
  tpma_lookup <- dplyr::select(tpma_lookup, c("strategy", "tpma_label"))
  filtered_data |>
    dplyr::filter_out(dplyr::if_any("model_run", \(x) x == 0)) |>
    join_for_labels(pod_lookup) |>
    relabel_pods() |>
    dplyr::left_join(tpma_lookup, "strategy") |>
    dplyr::select(!"strategy") |>
    dplyr::mutate(
      dplyr::across("measure", \(x) {
        dplyr::if_else(.data[["activity_type"]] == "aae", "arrivals", x)
      }),
      dplyr::across("tpma_label", \(x) tidyr::replace_na(x, "-"))
    ) |>
    # calculate the mean of all model runs for each combination of variables
    dplyr::summarise(
      dplyr::across("value", mean),
      .by = tidyselect::all_of(change_factor_sort_vars())
    )
}


move_baseline_row_to_top <- function(dat, var = "change_factor") {
  stopifnot(!("rn" %in% colnames(dat)))
  # add row_number column to ensure we don't lose any rows in setdiff below
  dat <- dplyr::mutate(dat, rn = dplyr::row_number())
  baseline_row <- dplyr::filter(dat, .data[[var]] == "baseline")
  dplyr::bind_rows(baseline_row, dplyr::setdiff(dat, baseline_row)) |>
    dplyr::select(!"rn")
}

#' Prepare a site-level summary table of change_factor results
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_change_factor_data
#' @returns A tibble
#' @export
export_principal_cf_data <- function(
  results,
  sites = NULL,
  pod_lookup = get_detailed_pods(),
  tpma_lookup = get_tpma_label_lookup()
) {
  sc_tbl <- results[["step_counts"]]
  selected_data <- filter_to_selected_sites(sc_tbl, sites)
  if (nrow(selected_data) == 0) {
    return(empty_result(
      proto_change_factor_data(),
      "No step count data for the selected sites."
    ))
  }
  selected_data |>
    prepare_change_factor_data(pod_lookup, tpma_lookup) |>
    dplyr::arrange(dplyr::pick(tidyselect::all_of(change_factor_sort_vars())))
}


change_factor_sort_vars <- function() {
  # fmt: skip
  c(
    "activity_type_label", "change_factor", "pod_label",
    "measure", "sitetret", "tpma_label"
  )
}
