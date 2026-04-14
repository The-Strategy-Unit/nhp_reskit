#' Prepare data from the `step_counts` results table for display as charts
#'
#' @param measure The measure to focus on for the output table. Valid values
#'  depend on which activity_type is selected
#' @param tpma_lookup A tibble, or a function that returns a tibble, containing
#'  a column named `strategy` (used as a key for joining to the `step_counts`
#'  table) and a column named `tpma_label` that provides friendly labels for
#'  all TPMAs/strategies
#' @param activity_type string. One of "ip", "op", "aae"
#' @param pods character vector. PoD labels to filter data to. The default
#'  value of `NULL` means no PoDs will be filtered out
#' @param include_baseline Boolean. Whether to include baseline data
#' @inheritParams compile_principal_los_data
#' @returns A prepared tibble of step count changes for each included TPMA
#' @export
compile_change_factor_data <- function(
  results,
  measure,
  tpma_lookup = get_tpma_label_lookup(),
  activity_type = c("ip", "op", "aae"),
  pods = NULL,
  sites = NULL,
  include_baseline = TRUE
) {
  activity_type <- rlang::arg_match(activity_type)
  init_data <- results[["step_counts"]] |>
    filter_principal_data(measure, activity_type, pods) |>
    filter_to_selected_sites(sites)
  if (nrow(init_data) == 0) {
    init_data
  } else {
    interim_data <- init_data |>
      prepare_principal_cf_data(tpma_lookup, include_baseline) |>
      summarise_for_all_sites() |>
      dplyr::summarise(dplyr::across("value", sum), .by = "change_factor") |>
      # Here we need to sort by decreasing value (biggest increases in activity
      # (+ve 'value's) at the top), and we need to ensure that the 'baseline'
      # row, if any, is at the top so that the cumsum() step works correctly.
      dplyr::arrange(dplyr::desc(dplyr::pick("value"))) |>
      move_baseline_row_to_top() |>
      dplyr::mutate(
        cmvalue = cumsum(.data[["value"]]),
        hide = dplyr::lag(.data[["cmvalue"]], 1, 0) + pmin(.data[["value"]], 0),
        total = abs(.data[["value"]]) + .data[["hide"]]
      ) |>
      dplyr::select(!"cmvalue")

    estimate_row <- tibble::tibble_row(
      change_factor = "estimate",
      value = sum(interim_data[["value"]]),
      hide = 0,
      total = .data[["value"]]
    )
    interim_data |>
      dplyr::bind_rows(estimate_row) |>
      dplyr::mutate(dplyr::across("change_factor", forcats::fct_inorder))
  }
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
  results,
  measure,
  tpma_lookup = get_tpma_label_lookup(),
  activity_type = c("ip", "op", "aae"),
  pods = NULL,
  sites = NULL,
  sort_by = c("value", "tpma_label")
) {
  activity_type <- rlang::arg_match(activity_type)
  sort_by <- rlang::arg_match(sort_by)
  impact_factors <- c("activity_avoidance", "efficiencies")

  init_data <- results[["step_counts"]] |>
    filter_principal_data(measure, activity_type, pods) |>
    filter_to_selected_sites(sites)
  if (nrow(init_data) == 0) {
    init_data
  } else {
    table_data <- init_data |>
      prepare_principal_cf_data(tpma_lookup, include_baseline = FALSE) |>
      summarise_for_all_sites() |>
      dplyr::filter(
        dplyr::if_any("change_factor", \(x) x %in% {{ impact_factors }})
      )
    if (nrow(table_data) == 0) {
      table_data
    } else {
      table_data <- table_data |>
        dplyr::summarise(
          dplyr::across("value", sum),
          .by = c("change_factor", "measure", "tpma_label")
        ) |>
        dplyr::filter(
          dplyr::if_any("tpma_label", \(x) x != "-") &
            # we only want to show TPMAs that _reduce_ the activity measure
            dplyr::if_any("value", \(x) x < 0)
        )
      # I would like to apologise for nesting ifs
      if (nrow(table_data) == 0) {
        table_data
      } else {
        table_data |>
          dplyr::arrange(
            dplyr::desc(dplyr::pick(tidyselect::all_of(sort_by)))
          ) |>
          dplyr::mutate(dplyr::across("tpma_label", forcats::fct_inorder))
      }
    }
  }
}


#' Data preparation step for `change_factor` data
#'
#' @param dat A tibble
#' @inheritParams compile_change_factor_data
#' @returns A tibble
#' @keywords internal
prepare_principal_cf_data <- function(dat, tpma_lookup, include_baseline) {
  bsline_filtered <- dplyr::filter(dat, .data[["change_factor"]] != "baseline")
  dat_prepared <- if (include_baseline) dat else bsline_filtered
  dat_prepared |>
    dplyr::filter(dplyr::if_any("value", \(x) x != 0)) |>
    dplyr::mutate(dplyr::across("pod", \(x) sub("^aae.*$", "aae", x))) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |>
    dplyr::left_join(tpma_lookup, "strategy") |>
    dplyr::select(!"strategy") |>
    dplyr::mutate(
      dplyr::across("tpma_label", \(x) tidyr::replace_na(x, "-"))
    ) |>
    dplyr::summarise(
      dplyr::across("value", mean),
      .by = tidyselect::all_of(change_factor_sort_vars())
    )
}


#' Prepare a site-level summary table of change_factor results
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_change_factor_data
#' @returns A tibble
#' @export
export_principal_cf_data <- function(results, sites = NULL) {
  results[["step_counts"]] |>
    filter_to_selected_sites(sites) |>
    prepare_principal_cf_data(
      tpma_lookup = get_tpma_label_lookup(),
      include_baseline = TRUE
    ) |>
    dplyr::arrange(dplyr::pick(tidyselect::all_of(change_factor_sort_vars())))
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
