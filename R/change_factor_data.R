#' Prepare data from the `step_counts` results table for display as charts
#'
#' @param dat The "step_counts" table from NHP results
#' @param tpma_lookup A tibble, or a function that returns a tibble, containing
#'  a column named `strategy` (used as a key for joining to the `step_counts`
#'  table) and a column named `tpma_label` that provides friendly labels for
#'  all TPMAs/strategies
#' @param activity_type string. One of "Inpatient", "Outpatient", "A&E"
#' @param pods character vector. PoD labels to filter data to. The default
#'  value of `NULL` means no PoDs will be filtered out
#' @param include_baseline Boolean. Whether to include baseline data
#' @inheritParams compile_principal_los_data
#' @returns A prepared tibble of step count changes for each included TPMA
#' @export
compile_change_factor_data <- function(
  dat,
  measure,
  tpma_lookup = get_tpma_label_lookup(),
  activity_type = c("Inpatient", "Outpatient", "A&E"),
  pods = NULL,
  sites = NULL,
  include_baseline = TRUE
) {
  activity_type <- rlang::arg_match(activity_type)
  prepared_data <- prepare_principal_cf_data(dat, tpma_lookup, include_baseline)
  pods <- pods %||% unique(prepared_data[["pod_label"]])

  interim_data <- prepared_data |>
    filter_to_selected_sites(sites) |>
    summarise_for_all_sites() |>
    filter_principal_cf_data(activity_type, pods, measure) |>
    dplyr::summarise(dplyr::across("value", sum), .by = "change_factor") |>
    # Here we need to sort by decreasing value (biggest increases in activity
    # (positive 'value's) at the top), and we need to ensure that the 'baseline'
    # row, if any, is at the top so that the cumsum() step works correctly.
    dplyr::arrange(dplyr::desc(dplyr::pick("value"))) |>
    move_baseline_row_to_top() |>
    dplyr::mutate(
      cmvalue = cumsum(.data[["value"]]),
      hidden = dplyr::lag(.data[["cmvalue"]], 1, 0) + pmin(.data[["value"]], 0),
      total = abs(.data[["value"]]) + .data[["hidden"]]
    ) |>
    dplyr::select(!"cmvalue")

  estimate_row <- tibble::tibble_row(
    change_factor = "estimate",
    value = sum(interim_data[["value"]]),
    hidden = 0,
    total = .data[["value"]]
  )
  interim_data |>
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
compile_indiv_change_factor_data <- function(
  dat,
  tpma_lookup = get_tpma_label_lookup(),
  activity_type = c("Inpatient", "Outpatient", "A&E"),
  pods,
  measure,
  sort_by = c("value", "tpma_label"),
  sites = NULL
) {
  activity_type <- rlang::arg_match(activity_type)
  sort_by <- rlang::arg_match(sort_by)
  impact_factors <- c("activity_avoidance", "efficiencies")
  table_data <- prepare_principal_cf_data(dat, tpma_lookup, FALSE) |>
    filter_to_selected_sites(sites) |>
    summarise_for_all_sites() |>
    filter_principal_cf_data(activity_type, pods, measure) |>
    dplyr::filter(
      dplyr::if_any("change_factor", \(x) x %in% {{ impact_factors }})
    )
  if (nrow(table_data) == 0) {
    NULL
  } else {
    table_data <- table_data |>
      dplyr::summarise(
        dplyr::across("value", sum),
        .by = c("change_factor", "tpma_label")
      ) |>
      dplyr::filter(
        dplyr::if_any("tpma_label", \(x) x != "-") &
          # we only want to show TPMAs that _reduce_ the activity measure
          dplyr::if_any("value", \(x) x < 0)
      )
    # I would like to apologise for nesting ifs
    if (nrow(table_data) == 0) {
      NULL
    } else {
      table_data |>
        dplyr::arrange(dplyr::desc(dplyr::pick(tidyselect::all_of(sort_by)))) |>
        dplyr::mutate(dplyr::across("tpma_label", forcats::fct_inorder))
    }
  }
}


export_sites_principal_cf_data <- function(dat, sites = NULL) {
  prepare_principal_cf_data(dat, include_baseline = TRUE) |>
    filter_to_selected_sites(sites) |>
    dplyr::arrange(dplyr::pick(tidyselect::all_of(change_factor_sort_vars())))
}


filter_principal_cf_data <- function(
  dat,
  activity_type,
  selected_pods,
  selected_measure
) {
  dat |>
    dplyr::filter(
      dplyr::if_any("activity_type_label", \(x) x == .env[["activity_type"]]) &
        dplyr::if_any("pod_label", \(x) x %in% .env[["selected_pods"]]) &
        dplyr::if_any("measure", \(x) x == .env[["selected_measure"]])
    )
}


#' Initial data preparation step for `change_factor` data
#'
#' @inheritParams compile_change_factor_data
#' @returns A tibble ready to be filtered by activity_type, pod, measure, sites
#' @keywords internal
prepare_principal_cf_data <- function(dat, tpma_lookup, include_baseline) {
  bsline_filtered <- dplyr::filter(dat, .data[["change_factor"]] != "baseline")
  dat_prepared <- if (include_baseline) dat else bsline_filtered
  keep_measures <- c("admissions", "beddays")
  dat_prepared |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x %in% {{ keep_measures }}) &
        dplyr::if_any("value", \(x) x != 0)
    ) |>
    dplyr::mutate(
      dplyr::across("pod", \(x) sub("^aae.*$", "aae", x)),
      dplyr::across("measure", uppercase_init)
    ) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |>
    dplyr::left_join(tpma_lookup, "strategy") |>
    dplyr::select(!"strategy") |>
    dplyr::mutate(
      dplyr::across("tpma_label", \(x) tidyr::replace_na(x, "-"))
    ) |>
    dplyr::summarise(
      dplyr::across("value", mean),
      .by = change_factor_sort_vars()
    )
}


change_factor_sort_vars <- function() {
  # fmt: skip
  c(
    "activity_type_label", "change_factor", "pod_label",
    "measure", "sitetret", "tpma_label"
  )
}


move_baseline_row_to_top <- function(dat, var = "change_factor") {
  stopifnot(!("rn" %in% colnames(dat)))
  #  add row_number column to ensure we don't lose any rows in setdiff below
  dat <- dplyr::mutate(dat, rn = dplyr::row_number())
  baseline_row <- dplyr::filter(dat, .data[[var]] == "baseline")
  dplyr::bind_rows(baseline_row, dplyr::setdiff(dat, baseline_row)) |>
    dplyr::select(!"rn")
}
