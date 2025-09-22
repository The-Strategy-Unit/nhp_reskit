make_principal_summary_table <- function(principal_summary_data, site = NULL) {
  principal_summary_data |>
    filter_to_site(site) |>
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
    gt::fmt_integer(c(baseline, principal, change)) |>
    gt::fmt_percent(c(change_pct), decimals = 0) |>
    gt::cols_width(c(principal, change, change_pct) ~ gt::px(150)) |>
    gt::cols_align("left", c(baseline, principal, change, change_pct)) |>
    gt::cols_label(
      pod_name = "Point of Delivery",
      change_pct = "Percent Change"
    ) |>
    gt::cols_label_with(fn = to_sentence) |>
    gt_theme()
}


compile_principal_summary_data <- function(results) {
  main_measures <- c("admissions", "attendances", "walk-in", "ambulance")
  main_summary <- results |>
    get_principal_high_level_summary(main_measures)

  teleattendance_summary <- results |>
    get_principal_high_level_summary("tele_attendances") |>
    dplyr::mutate(
      dplyr::across("pod_name", \(x) sub("Attendance", "Tele-attendance", x))
    ) |>
    dplyr::filter(dplyr::if_any("pod_name", \(x) x != "Outpatient Procedure"))

  beddays_summary <- results |>
    get_principal_high_level_summary("beddays") |>
    dplyr::mutate(
      dplyr::across("pod_name", \(x) sub("Admission", "Bed Days", x))
    )

  list(main_summary, teleattendance_summary, beddays_summary) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      # dplyr::across("activity_type", \(x) {
      #   dplyr::case_match(x, "ip" ~ "Inp", "op" ~ "Outp", "aae" ~ "A&E") |>
      #     sub("p$", "patient", x = _)
      # }),
      dplyr::across("activity_type_label", \(x) {
        forcats::fct(x, levels = c("Inpatient", "Outpatient", "A&E"))
      }),
      change = .data[["principal"]] - .data[["baseline"]],
      change_pct = .data[["change"]] / .data[["baseline"]]
    ) |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_name")))
}


get_principal_high_level_summary <- function(results_tbl, measures) {
  results_tbl |>
    dplyr::filter(dplyr::if_any("measure", \(x) x %in% {{ measures }})) |>
    dplyr::mutate(dplyr::across("pod", \(x) sub("^aae.*$", "aae", x))) |>
    dplyr::summarise(
      dplyr::across(c("baseline", "principal"), sum),
      .by = c("pod", "sitetret")
    ) |>
    dplyr::inner_join(get_principal_pods(), "pod") |>
    dplyr::relocate("pod_name") |>
    # activity_type not needed for high-level table
    dplyr::select(!c("pod", "activity_type", "measure")) |>
    dplyr::distinct()
}
