#' Create a gt table with data from [compile_principal_pod_data]
#'
#' @param principal_pod_data A suitable tibble
#' @returns A gt table
#' @export
make_principal_pod_table <- function(principal_pod_data) {
  principal_pod_data |>
    format_bar_cols() |>
    gt::gt(groupname_col = "activity_type_label") |>
    format_gt_core("pod_label") |>
    gt::cols_label(pod_label = "Point of Delivery") |>
    gt_theme()
}


#' Create a gt table with data from [compile_principal_los_data]
#'
#' @param principal_los_data A suitable tibble
#' @returns A gt table
#' @export
make_principal_los_table <- function(principal_los_data) {
  principal_los_data |>
    format_bar_cols() |>
    gt::gt(groupname_col = "pod_label") |>
    format_gt_core("los_group") |>
    gt::cols_label(los_group = "Length of Stay") |>
    gt_theme()
}


#' Create a gt table with data from [compile_detailed_activity_data]
#'
#' @param detailed_activity_data A suitable tibble
#' @param final_year string. The horizon year eg "2042/43"
#' @returns A gt table
#' @export
make_detailed_activity_table <- function(detailed_activity_data, final_year) {
  dat_cols <- colnames(detailed_activity_data)
  agg <- intersect(c("age_group", "tretspef"), dat_cols)
  agg_label <- ifelse(agg == "age_group", "Age Group", "Treatment Specialty")
  detailed_activity_data |>
    format_bar_cols() |>
    gt::gt(groupname_col = "sex") |>
    format_gt_core(agg) |>
    gt::cols_label(
      !!agg := agg_label,
      principal = glue::glue("Final ({final_year})")
    ) |>
    gt_theme()
}


#' Create a gt table with data from [compile_distribution_summary_data]
#'
#' @param distr_summary_data A suitable tibble
#' @returns A gt table
#' @export
make_distribution_summary_table <- function(distr_summary_data) {
  value_col <- intersect(c("median", "principal"), colnames(distr_summary_data))
  int_cols <- c("baseline", value_col, "change", "lower", "upper")
  distr_summary_data |>
    dplyr::mutate(dplyr::across("pod_label", \(x) {
      paste0(.data[["activity_type_label"]], " ", x)
    })) |>
    dplyr::select(!"activity_type_label") |>
    gt::gt(groupname_col = "pod_label") |>
    gt::fmt_integer(tidyselect::all_of(int_cols)) |>
    gt::fmt_percent("change_pct", decimals = 0) |>
    gt::cols_align("left", "measure") |>
    gt::tab_spanner("80% prediction interval", c("lower", "upper")) |>
    gt::cols_label_with(fn = uppercase_init) |>
    gt::cols_label(change_pct = gt::html("Percent<br />Change")) |>
    gt_theme()
}


#' Format horizontal `gt_bar`s within tables
#' @keywords internal
format_bar_cols <- function(tbl, p_col = "principal", p_clr = "#686f73") {
  tbl |>
    dplyr::mutate(
      dplyr::across({{ p_col }}, \(x) gt_bar(x, scales::label_comma(1), p_clr)),
      dplyr::across("change", \(x) gt_bar(x, scales::label_comma(1))),
      dplyr::across("change_pct", \(x) gt_bar(x, scales::label_percent(1)))
    )
}


#' Common helper function to handle standar formatting of gt tables
#' @keywords internal
format_gt_core <- function(gt_table, extra_col = NULL) {
  int_cols <- c("baseline", "principal", "change")
  bar_cols <- c("principal", "change", "change_pct")
  left_cols <- c({{ extra_col }}, bar_cols)
  gt_table |>
    gt::fmt_integer(tidyselect::all_of(int_cols)) |>
    gt::fmt_percent("change_pct", decimals = 0) |>
    gt::cols_width(c("principal", "change", "change_pct") ~ gt::px(150)) |>
    gt::cols_align("left", tidyselect::all_of(left_cols)) |>
    gt::cols_label_with(fn = uppercase_init) |>
    gt::cols_label(change_pct = "Percent Change")
}


#' Function to style gt tables
#' @keywords internal
gt_theme <- function(data) {
  data |>
    gt::tab_options(
      heading.subtitle.font.size = 12,
      heading.align = "left",
      column_labels.font.weight = "bold",
      row_group.border.top.width = gt::px(2),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      row_group.background.color = "#686f73",
      table_body.hlines.color = "white",
      table.border.top.color = "white",
      table.border.top.width = gt::px(2),
      table.border.bottom.color = "white",
      table.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = gt::px(1),
      summary_row.background.color = "#b2b7b9",
      grand_summary_row.background.color = "#343739"
    )
}


#' Function to handle the size and formatting of gt_bar elements in tables
#' @keywords internal
gt_bar <- function(x, format_fn = NULL, colours = c("#ec6555", "#f9bf07")) {
  format_fn <- format_fn %||% identity
  colours <- if (length(colours) == 1) rep(colours, 2) else colours
  stopifnot(length(colours) == 2)
  neg_colour <- colours[[1]]
  pos_colour <- colours[[2]]
  which_infinite <- which(is.infinite(x))
  x <- dplyr::if_else(is.infinite(x), 0, x)
  x_min <- min(min(x, na.rm = TRUE), 0) # if min(x) > 0, set x_min to 0
  x_max <- max(max(x, na.rm = TRUE), 0) # if max(x) < 0, set x_max to 0
  x_range <- x_max - x_min
  x_pmin <- pmin(x, 0)
  max_bar_wd <- 50 # max width (as %) of table col that a bar can take up (50%)

  create_bar_span <- function(bar_colour, bar_width) {
    glue::glue(
      "<span style='display: inline-block; direction: ltr; border: 0; ",
      "background-color: {bar_colour}; width: {bar_width}%;'>&nbsp;</span>\n"
    )
  }
  create_val_span <- function(value) {
    glue::glue("<span style='width: 50%;' align=right> {value}</span>")
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
  bar_spans[which_infinite] <- "<span><em>Inf.</em></span>"

  purrr::map(paste("<div>", bar_spans, "</div>", sep = "\n"), gt::html)
}
