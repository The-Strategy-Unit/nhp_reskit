make_principal_summary_table <- function(data, sites = NULL) {
  data |>
    format_bar_cols() |>
    dplyr::mutate(
      dplyr::across("activity_type_label", \(x) {
        dplyr::case_when(
          grepl("Admission", .data[["pod_name"]]) ~ paste0(x, " Admissions"),
          grepl("Bed Days", .data[["pod_name"]]) ~ paste0(x, " Bed Days"),
          .default = x
        )
      })
    ) |>
    gt::gt(groupname_col = "activity_type_label") |>
    format_gt_core("pod_name") |>
    gt::cols_label(pod_name = "Point of Delivery") |>
    gt_theme()
}


make_principal_summary_los_table <- function(data, sites = NULL) {
  data |>
    format_bar_cols() |>
    gt::gt(groupname_col = "pod_name") |>
    format_gt_core("los_group") |>
    gt::cols_label(los_group = "Length of Stay") |>
    gt_theme()
}


make_principal_detailed_table <- function(data, sites, agg_by, final_year) {
  agg_label <- dplyr::case_match(
    agg_by,
    "age_group" ~ "Age Group",
    "tretspef" ~ "Treatment Specialty",
    .default = uppercase_init(agg_by)
  )
  data |>
    summarise_to_selected_sites(sites) |>
    format_bar_cols() |>
    dplyr::mutate(dplyr::across("sex", convert_sex_codes)) |>
    gt::gt(groupname_col = "sex") |>
    format_gt_core("agg") |>
    gt::cols_label(
      agg = agg_label,
      principal = glue::glue("Final ({final_year})")
    ) |>
    gt_theme()
}


format_bar_cols <- function(tbl, p_col = "principal", p_clr = "#686f73") {
  tbl |>
    dplyr::mutate(
      dplyr::across({{ p_col }}, \(x) gt_bar(x, scales::label_comma(1), p_clr)),
      dplyr::across("change", \(x) gt_bar(x, scales::label_comma(1))),
      dplyr::across("change_pct", \(x) gt_bar(x, scales::label_percent(1)))
    )
}


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


gt_bar <- function(x, format_fn = NULL, colours = c("#ec6555", "#f9bf07")) {
  format_fn <- format_fn %||% identity
  colours <- if (length(colours) == 1) rep(colours, 2) else colours
  stopifnot(length(colours) == 2)
  neg_colour <- colours[[1]]
  pos_colour <- colours[[2]]
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
