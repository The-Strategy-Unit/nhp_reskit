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
    get_principal_high_level_summary(main_measures, sites)

  teleattendance_summary <- results |>
    get_principal_high_level_summary("tele_attendances", sites) |>
    dplyr::mutate(
      dplyr::across("pod_name", \(x) sub("Attendance", "Tele-attendance", x))
    ) |>
    dplyr::filter(!dplyr::if_any("pod_name", \(x) x == "Outpatient Procedure"))

  beddays_summary <- results |>
    get_principal_high_level_summary("beddays", sites) |>
    dplyr::mutate(
      dplyr::across("pod_name", \(x) sub("Admission", "Bed Days", x))
    )

  list(main_summary, teleattendance_summary, beddays_summary) |>
    dplyr::bind_rows() |>
    # uses same POD lookup as LoS summary
    dplyr::inner_join(principal_los_pods(), "pod") |>
    dplyr::select(c("pod_name", "activity_type", "baseline", "principal")) |>
    dplyr::mutate(
      dplyr::across("activity_type", \(x) {
        dplyr::case_match(x, "ip" ~ "Inp", "op" ~ "Outp", "aae" ~ "A&E") |>
          sub("p$", "patient", x = _)
      }),
      dplyr::across("activity_type", \(x) {
        factor(x, levels = c("Inpatient", "Outpatient", "A&E"))
      }),
      change = .data[["principal"]] - .data[["baseline"]],
      change_pcnt = .data[["change"]] / .data[["baseline"]]
    ) |>
    dplyr::arrange(dplyr::pick(c("activity_type", "pod_name")))
}


principal_summary_table <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        "principal",
        \(.x) gt_bar(.x, scales::comma_format(1), "#686f73", "#686f73")
      ),
      dplyr::across("change", \(.x) gt_bar(.x, scales::comma_format(1))),
      dplyr::across("change_pcnt", \(.x) gt_bar(.x, scales::percent_format(1)))
    ) |>
    dplyr::mutate(
      "activity_type" = as.character(.data$activity_type),
      "activity_type" = dplyr::case_when(
        # include admissions/beddays in gt groupnames
        stringr::str_detect(.data$pod_name, "Admission") ~
          glue::glue("{.data$activity_type} Admissions"),
        stringr::str_detect(.data$pod_name, "Bed Days") ~
          glue::glue("{.data$activity_type} Bed Days"),
        .default = .data$activity_type
      )
    ) |>
    gt::gt(groupname_col = "activity_type") |>
    gt::cols_align(align = "left", columns = "pod_name") |>
    gt::cols_label(
      "pod_name" = "Point of Delivery",
      "baseline" = "Baseline",
      "principal" = "Principal",
      "change" = "Change",
      "change_pcnt" = "Percent Change"
    ) |>
    gt::fmt_integer("baseline") |>
    gt::cols_width(
      .data$principal ~ gt::px(150),
      .data$change ~ gt::px(150),
      .data$change_pcnt ~ gt::px(150)
    ) |>
    gt::cols_align(
      align = "left",
      columns = c("baseline", "principal", "change", "change_pcnt")
    ) |>
    gt_theme()
}


principal_los_pods <- function() {
  get_activity_type_pod_measure_options() |>
    dplyr::filter(.data$activity_type != "aae") |>
    dplyr::distinct(.data$activity_type, .data$pod, .data$pod_name) |>
    dplyr::bind_rows(data.frame(
      activity_type = "aae",
      pod = "aae",
      pod_name = "A&E Attendance"
    )) |>
    dplyr::mutate(dplyr::across("pod_name", forcats::fct_inorder))
}


get_principal_high_level_summary <- function(results, measures, sites) {
  results |>
    purrr::pluck("default") |>
    dplyr::filter(dplyr::if_any("measure", \(x) x %in% {{ measures }})) |>
    dplyr::mutate(dplyr::across("pod", \(x) sub("^aae.*$", "aae", x))) |>
    dplyr::summarise(
      dplyr::across(c("baseline", "principal"), sum),
      .by = c("pod", "sitetret")
    ) |>
    trust_site_aggregation(sites)
}


get_activity_type_pod_measure_options <- function() {
  get_golem_config("pod_measures") |>
    purrr::map_dfr(
      \(.x) {
        .x$pods |>
          purrr::map_dfr(tibble::as_tibble, .id = "pod") |>
          dplyr::transmute(
            activity_type_name = .x$name,
            .data$pod,
            pod_name = .data$name,
            .data$measures
          )
      },
      .id = "activity_type"
    )
}
