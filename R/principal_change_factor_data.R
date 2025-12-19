# with "step_counts" table
compile_change_factor_data <- function(
  dat,
  activity_type,
  pods,
  measure,
  sites = NULL,
  include_baseline = TRUE
) {
  prepared_data <- prepare_principal_cf_data(dat, include_baseline)

  interim_data <- prepared_data |>
    filter_to_selected_sites(sites = NULL) |>
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


compile_indiv_change_factor_data <- function(
  dat,
  activity_type,
  pods,
  measure,
  sort_by = c("value", "tpma_label"),
  sites = NULL
) {
  sort_by <- match.arg(sort_by)
  impact_factors <- c("activity_avoidance", "efficiencies")
  table_data <- prepare_principal_cf_data(dat, FALSE) |>
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
          # we only want to include TPMAs that _reduce_ the measure
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


prepare_principal_cf_data <- function(dat, include_baseline) {
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
    dplyr::left_join(get_tpma_label_lookup(), "strategy") |>
    dplyr::select(!"strategy") |>
    dplyr::mutate(
      dplyr::across("tpma_label", \(x) dplyr::if_else(is.na(x), "-", x))
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
  #  add row_number column to ensure we don't lose any rows
  dat <- dplyr::mutate(dat, rn = dplyr::row_number())
  baseline_row <- dplyr::filter(dat, .data[[var]] == "baseline")
  dplyr::bind_rows(baseline_row, dplyr::setdiff(dat, baseline_row)) |>
    dplyr::select(!"rn")
}
