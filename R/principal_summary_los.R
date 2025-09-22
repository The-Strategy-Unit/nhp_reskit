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
