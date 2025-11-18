summarise_to_selected_sites <- function(dat, sites, site_col = "sitetret") {
  dat <- if (is.null(sites)) {
    dat
  } else {
    dplyr::filter(dat, dplyr::if_any({{ site_col }}, \(x) x %in% {{ sites }}))
  }
  dat |>
    dplyr::select(!tidyselect::all_of({{ site_col }})) |>
    dplyr::summarise(
      dplyr::across(tidyselect::where(is.numeric), sum),
      .by = tidyselect::where(\(x) !is.numeric(x))
    )
}

get_trust_sites <- \(res_tbl, col = "sitetret") sort(unique(res_tbl[[col]]))

convert_sex_codes <- \(x) dplyr::if_else(x == 1L, "Male", "Female")

uppercase_init <- \(x) sub("^([a-z])(.+)", "\\U\\1\\E\\2", x, perl = TRUE)
