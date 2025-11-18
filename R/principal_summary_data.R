compile_principal_summary_data <- function(results, sites = NULL) {
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
      dplyr::across("activity_type_label", \(x) {
        forcats::fct(x, levels = c("Inpatient", "Outpatient", "A&E"))
      })
    ) |>
    summarise_to_selected_sites(sites) |>
    add_change_cols() |>
    dplyr::arrange(dplyr::pick(c("activity_type_label", "pod_name")))
}


# "tretspef+los_group" table after calculate_principal() has been applied
compile_principal_summary_los_data <- function(los_tbl, sites = NULL, measure) {
  summary_los_data <- los_tbl |>
    dplyr::filter(dplyr::if_any("measure", \(x) x == {{ measure }})) |>
    dplyr::inner_join(get_principal_pods(), "pod") |>
    dplyr::relocate("pod_name") |>
    dplyr::select(!c("pod", "activity_type", "measure")) |>
    summarise_to_selected_sites(sites) |>
    # dplyr::distinct() |>
    add_change_cols()

  if (measure == "beddays") {
    # can also be "admissions"
    summary_los_data <- summary_los_data |>
      dplyr::mutate(
        dplyr::across("pod_name", \(x) sub("Admission", "Bed Days", x))
      )
  }

  summary_los_data |>
    dplyr::arrange(dplyr::pick(c("pod_name", "los_group")))
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


add_change_cols <- function(tbl) {
  stopifnot(all(c("baseline", "principal") %in% colnames(tbl)))
  tbl |>
    dplyr::mutate(
      change = .data[["principal"]] - .data[["baseline"]],
      change_pct = .data[["change"]] / .data[["baseline"]]
    )
}
