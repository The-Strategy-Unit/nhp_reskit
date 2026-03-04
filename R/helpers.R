summarise_for_all_sites <- function(dat, site_col = "sitetret") {
  dat |>
    dplyr::select(!tidyselect::all_of({{ site_col }})) |>
    dplyr::summarise(
      dplyr::across(tidyselect::where(is.numeric), sum),
      .by = !tidyselect::where(is.numeric)
    )
}


filter_to_selected_sites <- function(dat, sites, site_col = "sitetret") {
  if (is.null(sites)) {
    dat
  } else {
    dplyr::filter(dat, dplyr::if_any({{ site_col }}, \(x) x %in% {{ sites }}))
  }
}


#' Exclude outpatient procedures from tele-attendances count only
#' @param tbl A tibble
#' @keywords internal
exclude_op_teleatt_procedures <- function(tbl) {
  stopifnot(all(c("measure", "pod") %in% colnames(tbl)))
  tbl |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x != "tele_attendances") |
        dplyr::if_any("pod", \(x) x != "op_procedure")
    )
}


#' Add `change` and `change_pct` columns to a prepared results table
#'
#' @param tbl A tibble of appropriately prepared results
#' @returns A tibble
#' @export
add_change_cols <- function(tbl) {
  stopifnot(all(c("baseline", "principal") %in% colnames(tbl)))
  tbl |>
    dplyr::mutate(
      change = .data[["principal"]] - .data[["baseline"]],
      change_pct = .data[["change"]] / .data[["baseline"]]
    )
}


keep_mean_only <- function(tbl) {
  stopifnot("stat" %in% colnames(tbl))
  tbl |>
    dplyr::filter(dplyr::if_any("stat", \(x) x == "mean")) |>
    dplyr::select(!"stat")
}


#' Filter a table so the `measure` column only contains 6 selected measures
#'
#' Currently this contains 6 of the 7 possible values; it excludes "procedures".
#' This function is used in several places in reskit as a filter.
#' @param tbl A tibble
#' @keywords internal
filter_to_main_measures <- function(tbl) {
  # fmt: skip
  keep_measures <- c(
    "admissions", "ambulance", "attendances",
    "beddays", "tele_attendances", "walk-in"
  )
  dplyr::filter(tbl, dplyr::if_any("measure", \(x) x %in% {{ keep_measures }}))
}


#' From any results table, get list of all site codes for this scheme
#'
#' The "default" table is recommended
#' @param res_tbl A tibble from the results list
#' @param col string The name of the column containing site codes. `sitetret` by
#'  default
#' @returns A character vector
#' @export
get_trust_sites <- \(res_tbl, col = "sitetret") sort(unique(res_tbl[[col]]))


convert_sex_codes <- \(x) dplyr::if_else(x == 1L, "Male", "Female")


uppercase_init <- \(x) sub("^([[:alpha:]])(.+)", "\\U\\1\\E\\2", x, perl = TRUE)


#' Get a lookup of tretspef codes to descriptions
#'
#' Currently reads from a fixed location within the package.
#' @returns A 2-column tibble with columns `code` and `tretspef`
#' @export
get_tretspef_lookup <- function() {
  system.file("tx-lookup.json", package = "reskit") |>
    yyjsonr::read_json_file() |>
    tibble::as_tibble() |>
    dplyr::select(c(code = "Code", tretspef = "Description")) |>
    dplyr::mutate(
      dplyr::across("tretspef", \(x) sub(" Service$", "", x)),
      dplyr::across("tretspef", \(x) paste0(.data[["code"]], ": ", x))
    ) |>
    # as per HES dictionary
    tibble::add_row(code = "&", tretspef = "Not known")
}


#' Get a lookup of TPMA labels
#'
#' Currently reads from a fixed location within the package.
#' @returns A 2-column tibble, with columns `strategy` and `tpma_label`
#' @export
get_tpma_label_lookup <- function() {
  system.file("mitigators.json", package = "reskit") |>
    yyjsonr::read_json_file() |>
    purrr::imap(\(x, nm) tibble::tibble(strategy = nm, tpma_label = x)) |>
    purrr::list_rbind()
}
