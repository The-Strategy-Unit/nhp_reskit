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


get_trust_sites <- \(res_tbl, col = "sitetret") sort(unique(res_tbl[[col]]))
convert_sex_codes <- \(x) dplyr::if_else(x == 1L, "Male", "Female")

uppercase_init <- \(x) sub("^([[:alpha:]])(.+)", "\\U\\1\\E\\2", x, perl = TRUE)


get_tretspef_lookup <- function() {
  yyjsonr::read_json_file(here::here("inst/tx-lookup.json")) |>
    tibble::as_tibble() |>
    dplyr::rename_with(tolower) |>
    dplyr::select(c("code", tretspef = "description")) |>
    dplyr::mutate(
      dplyr::across("tretspef", \(x) sub(" Service$", "", x)),
      dplyr::across("tretspef", \(x) paste0(.data[["code"]], ": ", x))
    ) |>
    # as per HES dictionary
    tibble::add_row(code = "&", tretspef = "Not known")
}


#' Get a lookup of TPMA labels from a 'strategy' variable
#'
#' @param file Path to JSON source file
#' @returns A 2-column tibble
#' @export
get_tpma_label_lookup <- function(file = here::here("inst/mitigators.json")) {
  yyjsonr::read_json_file(file) |>
    purrr::imap(\(x, nm) tibble::tibble(strategy = nm, tpma_label = x)) |>
    purrr::list_rbind()
}
