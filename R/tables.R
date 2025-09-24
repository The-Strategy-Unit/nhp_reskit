gt_bar <- function(x, format_fn = NULL, colours = c("#ec6555", "#f9bf07")) {
  format_fn <- format_fn %||% identity
  if (length(colours) == 1) {
    colours <- rep(colours, 2)
  }
  stopifnot(length(colours) == 2)
  neg_colour <- colours[[1]]
  pos_colour <- colours[[2]]
  x_min <- min(min(x, na.rm = TRUE), 0) # if min(x) > 0, set x_min to 0
  x_max <- max(max(x, na.rm = TRUE), 0) # if max(x) < 0, set x_max to 0
  x_range <- x_max - x_min
  x_pmin <- pmin(x, 0)
  max_bar_wd <- 50 # max width (as %) of table col that a bar can take up (50%)

  create_bar_span <- function(...) {
    glue::glue(
      "<span style='display: inline-block; direction: ltr; border: 0; ",
      "background-color: {bar_colour}; width: {bar_width}%;'>&nbsp;</span>\n"
    )
  }
  glue_val_span <- function(value) {
    glue::glue("<span style='width: 50%;' align=right>{value}</span>")
  }

  empty_bar_tbl <- tibble::tibble(
    bar_colour = "transparent",
    bar_width = (abs(x_min - x_pmin) / x_range) * max_bar_wd
  )
  value_bar_tbl <- tibble::tibble(
    bar_colour = dplyr::if_else(x <= 0, neg_colour, pos_colour),
    bar_width = (abs(x) / x_range) * max_bar_wd
  )

  bar_spans <- paste0(
    purrr::pmap_chr(empty_bar_tbl, create_bar_span),
    purrr::pmap_chr(value_bar_tbl, create_bar_span),
    purrr::map_chr(format_fn(x), create_val_span)
  )

  purrr::map(paste("<div>", bar_spans, "</div>", sep = "\n"), gt::html)
}
