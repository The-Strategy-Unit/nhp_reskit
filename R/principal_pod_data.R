#' Prepare data from default results table for displaying as summary table
#'
#' @param default_tbl the "default" table from NHP results
#' @param sites Either `NULL` (the default) or a vector of site codes to filter
#'  to. `NULL` means don't filter; include all sites present in the data.
#' @returns A filtered and sorted tibble of principal projections of results,
#'  by activity type and point of delivery (PoD)
#' @export
compile_principal_pod_data <- function(default_tbl, sites = NULL) {
  at_levels <- c(
    "Inpatient Admissions",
    "Inpatient Bed Days",
    "Outpatient",
    "A&E"
  )
  prepare_sites_principal_pod_data(default_tbl) |>
    filter_to_selected_sites(sites) |>
    summarise_for_all_sites() |>
    add_change_cols() |>
    dplyr::mutate(
      dplyr::across("activity_type_label", \(x) forcats::fct(x, at_levels)),
      # display pods in desc order of baseline level of admissions/beddays
      dplyr::across("pod_label", \(x) {
        forcats::fct_reorder(x, .data[["baseline"]], sum, .desc = TRUE)
      })
    ) |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_label")))
}

#' Initial preparation of site-level data for the main summary table
#'
#' @inheritParams compile_principal_pod_data
#' @returns A tibble
#' @keywords internal
prepare_sites_principal_pod_data <- function(default_tbl) {
  # only "procedures" excluded from full list of measures, but we will do a
  # "positive" filter in rather than a filter out
  keep_measures <- c(
    "admissions",
    "ambulance",
    "attendances",
    "beddays",
    "tele_attendances",
    "walk-in"
  )
  default_tbl |>
    dplyr::filter(dplyr::if_any("measure", \(x) x %in% {{ keep_measures }})) |>
    dplyr::filter(
      # exclude outpatient procedures from tele-attendances count only
      dplyr::if_any("measure", \(x) x != "tele_attendances") |
        dplyr::if_any("pod", \(x) x != "op_procedure")
    ) |>
    dplyr::mutate(dplyr::across("pod", \(x) sub("^aae.*$", "aae", x))) |>
    inner_join_for_labels(get_principal_pods()) |>
    relabel_pods() |>
    calculate_principal_stats(default_group_cols("activity_type_label")) |>
    dplyr::filter(dplyr::if_any("stat", \(x) x == "mean")) |>
    dplyr::select(!"stat")
}

#' Prepare a site-level summary of main projection results
#'
#' Intended to be used to create a table to be exported to .csv/.xlsx
#' @inheritParams compile_principal_pod_data
#' @returns A tibble
#' @export
export_sites_principal_pod_data <- function(default_tbl, sites = NULL) {
  prepare_sites_principal_pod_data(default_tbl) |>
    filter_to_selected_sites(sites) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_label")))
}

#' Convert PoDs and activity types to more accurate labels
#' @keywords internal
relabel_pods <- function(tbl) {
  tbl |>
    dplyr::mutate(
      dplyr::across("pod_label", \(x) {
        dplyr::case_when(
          .data[["measure"]] == "tele_attendances" ~ sub("Att", "Tele-att", x),
          .data[["measure"]] == "beddays" ~ sub("Admission", "Bed Days", x),
          .default = x
        )
      }),
      dplyr::across("activity_type_label", \(x) {
        dplyr::case_when(
          .data[["measure"]] == "admissions" ~ paste0(x, " Admissions"),
          .data[["measure"]] == "beddays" ~ paste0(x, " Bed Days"),
          .default = x
        )
      })
    )
}

#' Use a lookup table to get more readable labels for PoDs
#' @keywords internal
inner_join_for_labels <- function(tbl, lookup) {
  tbl |>
    dplyr::inner_join(lookup, "pod") |>
    dplyr::relocate("pod_label") |>
    # we don't need to keep "pod" (we will use "pod_label" in the final tables)
    dplyr::select(!"pod")
}
