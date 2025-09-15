principal_summary_los_data <- function(r, sites, measure) {
  pods <- principal_los_pods()

  summary_los <- r$results[["tretspef+los_group"]] |>
    dplyr::filter(.data$measure == .env$measure) |>
    dplyr::select(-"tretspef") |>
    trust_site_aggregation(sites) |>
    dplyr::inner_join(pods, by = "pod") |>
    dplyr::mutate(
      change = .data$principal - .data$baseline,
      change_pcnt = .data$change / .data$baseline
    ) |>
    dplyr::select(
      "pod_name",
      "los_group",
      "baseline",
      "principal",
      "change",
      "change_pcnt"
    )

  if (measure == "beddays") {
    summary_los <- summary_los |>
      dplyr::mutate(
        pod_name = forcats::fct_relabel(
          .data$pod_name,
          \(.x) stringr::str_replace(.x, "Admission", "Bed Days")
        )
      )
  }

  summary_los[order(summary_los$pod_name, summary_los$los_group), ]
}


principal_summary_los_table <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(
        "principal",
        \(.x) gt_bar(.x, scales::comma_format(1), "#686f73", "#686f73")
      ),
      dplyr::across("change", \(.x) gt_bar(.x, scales::comma_format(1))),
      dplyr::across("change_pcnt", \(.x) gt_bar(.x, scales::percent_format(1)))
    ) |>
    gt::gt(groupname_col = "pod_name") |>
    gt::cols_align(align = "left", columns = "los_group") |>
    gt::cols_label(
      "los_group" = "Length of Stay",
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


trust_site_aggregation <- function(data, sites) {
  data_filtered <- if (length(sites) == 0) {
    data
  } else {
    dplyr::filter(data, .data$sitetret %in% sites)
  }

  data_filtered |>
    dplyr::group_by(
      dplyr::across(
        c(
          tidyselect::where(is.character),
          tidyselect::where(is.factor),
          tidyselect::any_of(c("model_run", "year")),
          -"sitetret"
        )
      )
    ) |>
    dplyr::summarise(
      dplyr::across(where(is.numeric), \(.x) sum(.x, na.rm = TRUE)),
      .groups = "drop"
    )
}
