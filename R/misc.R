summarise_to_site <- function(dat, site, sum_by, site_col = "sitetret") {
  dat |>
    filter_to_site(sites, site_col) |>
    dplyr::summarise(
      # was: across(where(is.numeric)) - trying being more specific here
      dplyr::across(c("baseline", "principal"), sum),
      .by = tidyselect::all_of(sum_by)
    )
}


filter_to_site <- function(dat, site, site_col = "sitetret") {
  dat <- if (is.null(site)) {
    dat
  } else {
    dplyr::filter(dat, dplyr::if_any({{ site_col }}, \(x) x == {{ site }}))
  }
  dplyr::select(dat, !tidyselect::all_of({{ site_col }}))
}


get_trust_sites <- \(res_tbl, col = "sitetret") sort(unique(res_tbl[[col]]))


uppercase_init <- \(x) sub("^([a-z])(.+)", "\\U\\1\\E\\2", x, perl = TRUE)
