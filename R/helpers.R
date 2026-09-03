filter_principal_data <- function(dat, measure, activity_type, pods = NULL) {
  pods <- pods %||% unique(dat[["pod"]])
  dat |>
    dplyr::filter(
      dplyr::if_any("pod", \(x) x %in% .env[["pods"]]) &
        dplyr::if_any("measure", \(x) x == .env[["measure"]]) &
        dplyr::if_any("activity_type", \(x) x == .env[["activity_type"]])
    )
}


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


#' Return a zero-row result that keeps the expected output shape
#'
#' Used by the `compile_*` functions when filtering leaves no rows. Returning a
#'  correctly shaped zero-row tibble, rather than whatever partially prepared
#'  object happened to be in hand, means the downstream `make_*` functions can
#'  rely on the output columns existing.
#' The `reskit_no_data` attribute lets those functions render an explicit
#'  "no data" panel rather than a blank chart or a cryptic missing-column error.
#' This function and its documentation were suggested by an LLM.
#' @param prototype A zero-row tibble giving the columns and types that the
#'  calling function returns when data are available
#' @param reason A string explaining why no rows remain, stored as an attribute
#' @returns `prototype`, carrying a `reskit_no_data` attribute
#' @keywords internal
empty_result <- function(prototype, reason = NULL) {
  stopifnot(nrow(prototype) == 0)
  reason <- reason %||% "No data available for the current selection."
  rlang::inform(reason, class = "reskit_no_data", use_cli_format = TRUE)
  attr(prototype, "reskit_no_data") <- reason
  prototype
}


#' Recover the explanation attached by [empty_result]
#' @param x A tibble returned by a `compile_*` function
#' @returns A string, or `NULL` if the result was not flagged as empty
#' @keywords internal
no_data_reason <- \(x) attr(x, "reskit_no_data", exact = TRUE)


#' Render a placeholder plot when there are no data to display
#'
#' The plot equivalent of [make_no_data_table]. Preferred over returning an
#'  empty ggplot, which renders as a blank panel and gives the reader no clue
#'  why it is blank. Returning a ggplot object (rather than, say, a table)
#'  keeps the return type of the `make_*_plot` functions consistent, so callers
#'  can still pass the result to `plotly::ggplotly()` or a patchwork layout.
#' @param reason A string explaining why there are no data, or `NULL`
#' @returns A ggplot object
#' @keywords internal
make_no_data_plot <- function(reason = NULL) {
  reason <- reason %||% "No data available for the current selection."
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = reason,
      size = 5,
      colour = "grey40"
    ) +
    ggplot2::theme_void()
}


#' Exclude outpatient procedures from tele-attendances count only
#' @param tbl A tibble
#' @keywords internal
exclude_op_teleatt_procedures <- function(tbl) {
  stopifnot(all(c("measure", "pod") %in% colnames(tbl)))
  tbl |>
    dplyr::filter_out(
      dplyr::if_any("measure", \(x) x == "tele_attendances") &
        dplyr::if_any("pod", \(x) x == "op_procedure")
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
#' Currently this contains 6 of 7 possible values in principal data; it
#'  excludes "procedures". ("arrivals" is found in the step counts file).
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


#' Use a lookup table to get more readable labels for PoDs
#' @param tbl A tibble
#' @param lookup A lookup table with `pod` and `pod_label` columns
#' @keywords internal
join_for_labels <- function(tbl, lookup) {
  tbl |>
    dplyr::left_join(lookup, "pod") |>
    dplyr::relocate(c("pod_label", "activity_type_label"), .after = "pod")
}


#' Give PoDs more accurate labels
#' @param tbl A tibble
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
      })
    )
}


#' Give activity types more accurate labels
#' @rdname relabel_pods
#' @keywords internal
relabel_ip_activity_types <- function(tbl) {
  tbl |>
    dplyr::mutate(
      dplyr::across("activity_type_label", \(x) {
        dplyr::if_else(
          x == "Inpatient",
          paste0(x, " ", uppercase_init(.data[["measure"]])),
          x
        )
      }),
      dplyr::across("activity_type_label", \(x) sub("Beddays", "Bed Days", x))
    )
}


#' Create a column called `activity_type` by extracting a substring from `pod`
#' @param tbl A tibble
#' @keywords internal
get_activity_type_from_pod <- function(tbl) {
  dplyr::mutate(tbl, activity_type = sub("^([a-z]*).*", "\\1", .data[["pod"]]))
}


convert_sex_codes <- \(x) dplyr::if_else(x == 1L, "Male", "Female")

create_measure_label <- \(x) uppercase_init(sub("dd", "d D", gsub("_", "-", x)))

uppercase_init <- \(x) sub("^([[:alpha:]])(.+)", "\\U\\1\\E\\2", x, perl = TRUE)


#' Get a lookup of tretspef codes to descriptions
#'
#' @returns A 2-column tibble with columns `code` and `tretspef`
#' @export
get_tretspef_lookup <- function() {
  json_data <- possibly_read_tx_lookup()
  msg <- "Unable to read tretspef data from GitHub"
  azkit::check_that(json_data, is_not_null, msg)
  tibble::as_tibble(json_data) |>
    dplyr::select(c(code = "Code", tretspef = "Description")) |>
    dplyr::mutate(
      dplyr::across("tretspef", \(x) sub(" Service$", "", x)),
      dplyr::across("tretspef", \(x) paste0(.data[["code"]], ": ", x))
    ) |>
    # as per HES dictionary
    tibble::add_row(code = "&", tretspef = "Not known")
}


#' Prepare a lookup table for all current TPMAs
#'
#' @returns A 4-column tibble, with columns `strategy`, `activity_type`,
#'  `change_factor` and `tpma_label`
#' @export
get_tpma_label_lookup <- function() {
  csv_data <- possibly_read_tpmas_lookup()
  msg <- "Unable to read TPMA lookup table from GitHub"
  azkit::check_that(csv_data, is_not_null, msg)
  csv_data |>
    dplyr::filter(dplyr::if_any("active_to", is.na)) |>
    dplyr::select(!"active_to") |>
    dplyr::mutate(
      tpma_label = glue::glue("{tpma_name} ({tpma_subtype})"),
      dplyr::across("tpma_type", \(x) tolower(sub(" ", "_", x))),
      dplyr::across("activity_type", convert_activity_type),
      .keep = "unused"
    ) |>
    dplyr::rename(change_factor = "tpma_type", strategy = "tpma_variable")
}


#' Converts "In|Outpatients", "A&E" strings to "ip", "op" and "aae"
#' @param x A character vector
#' @returns A character vector
#' @keywords internal
convert_activity_type <- function(x) {
  dplyr::recode_values(
    x,
    "A&E" ~ "aae",
    "Inpatients" ~ "ip",
    "Outpatients" ~ "op"
  )
}


check_measure <- function(measure) {
  measure_list <- rlang::set_names(potential_measures(), "*")
  measure_msg <- c("{.arg measure} must be one of ", measure_list)
  azkit::check_that(measure, \(x) x %in% potential_measures(), measure_msg)
}


potential_measures <- function() {
  # fmt: skip
  c(
    "admissions", "ambulance", "arrivals", "attendances", "beddays",
    "procedures", "tele_attendances", "walk-in"
  )
}


#' grepv a glued regex
#' @description Facilitates using regex in search/filter patterns, and puts the
#'  arguments "the right way round" (x first, then pattern), unlike [grepv]
#' @returns A character vector: all values of x that match the regex in rx
#' @keywords internal
gregv <- \(x, rx, g = parent.frame()) grepv(glue::glue_data(g, rx), x)

is_not_null <- \(x) !is.null(x)
