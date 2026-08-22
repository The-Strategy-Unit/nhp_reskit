#' Function to create a "default" table with synthetic values
#'
#' The table contains synthetic (randomly created) values for a selection of
#'  PODs and measures, across 2 "sites", as if created by 64 model runs
#' @param seed integer, for reproducibility of outputs, a seed value to be used
#' @keywords internal
create_demo_default_tbl <- function(seed) {
  init_tbl1 <- create_base_tbl_with_sitetret("site1")
  init_tbl2 <- create_base_tbl_with_sitetret("site2")
  baseline_tbl1 <- add_baseline_values(init_tbl1, c(5e3L, 85e3L), 1e3L, seed)
  baseline_tbl2 <- add_baseline_values(init_tbl2, c(3e3L, 45e3L), 1e3L, seed)
  initial_means <- get_full_initial_means(0.8, 0.9, 1)
  horizon_tbl1 <- add_horizon_values(baseline_tbl1, initial_means, seed)
  horizon_tbl2 <- add_horizon_values(baseline_tbl2, initial_means, seed)
  dplyr::bind_rows(horizon_tbl1, horizon_tbl2) |>
    dplyr::arrange(dplyr::pick(c("pod", "sitetret", "measure", "model_run")))
}


#' Function to create a "tretspef + LoS group" table with synthetic values
#' The table contains synthetic (randomly created) values for a selection of
#'  PODs, tretspefs, LoS groups and measures, across 2 "sites", as if created
#'  by 64 model runs
#' @inheritParams create_demo_default_tbl
#' @keywords internal
create_demo_tretspef_losgroup_tbl <- function(seed) {
  tretspef_codes <- sort(sample(get_tretspef_lookup()[["code"]], 25)) |>
    withr::with_seed(seed = seed)
  los_groups <- paste0(
    c("0", "1", "2", "3", "4-7", "8-14", "15-21", "22+"),
    " days"
  ) |>
    sub("^1 days$", "1 day", x = _)
  init_tbl1 <- create_ip_base_tbl() |>
    dplyr::mutate(
      sitetret = "site1",
      tretspef = list(tretspef_codes),
      los_group = list(los_groups),
      .after = "pod"
    ) |>
    tidyr::unnest_longer("tretspef") |>
    tidyr::unnest_longer("los_group") |>
    tidyr::expand(
      tidyr::nesting(pod, measure, sitetret),
      tretspef,
      los_group
    ) |>
    dplyr::relocate("measure", .after = dplyr::last_col())
  init_tbl2 <- dplyr::mutate(init_tbl1, sitetret = "site2")
  baseline_tbl1 <- add_baseline_values(init_tbl1, c(100L, 5e3L), 100L, seed)
  baseline_tbl2 <- add_baseline_values(init_tbl2, c(100L, 2e3L), 100L, seed)
  horizon_tbl1 <- add_horizon_values(baseline_tbl1, 0.85, seed)
  horizon_tbl2 <- add_horizon_values(baseline_tbl2, 0.85, seed)
  dplyr::bind_rows(horizon_tbl1, horizon_tbl2) |>
    dplyr::arrange(dplyr::pick(c(
      "pod",
      "sitetret",
      "tretspef",
      "los_group",
      "measure",
      "model_run"
    )))
}


#' Function to create a "tretspef + age group" table with synthetic values
#' The table contains synthetic (randomly created) values for a selection of
#'  PODs, tretspefs, age groups and measures, across 2 "sites", as if created
#'  by 64 model runs
#' @inheritParams create_demo_default_tbl
#' @keywords internal
create_demo_sex_agegroup_tbl <- function(seed) {
  # fmt: skip
  age_groups <- c(
    "0", "1-4", "10-15", "16-17", "18-34", "35-49", "5-9",
    "50-64", "65-74", "75-84", "85+"
  )
  init_tbl1 <- create_base_tbl_with_sitetret("site1") |>
    dplyr::mutate(
      sex = list(seq(2)),
      age_group = list(age_groups),
      .after = "sitetret"
    ) |>
    tidyr::unnest_longer("sex") |>
    tidyr::unnest_longer("age_group") |>
    tidyr::expand(tidyr::nesting(pod, measure, sitetret), sex, age_group) |>
    dplyr::relocate("measure", .after = dplyr::last_col())
  init_tbl2 <- dplyr::mutate(init_tbl1, sitetret = "site2")
  baseline_tbl1 <- add_baseline_values(init_tbl1, c(2e3L, 5e4L), 100L, seed)
  baseline_tbl2 <- add_baseline_values(init_tbl2, c(1e3L, 2e4L), 100L, seed)
  multip <- length(seq(2)) * length(age_groups)
  initial_values <- rep(get_full_initial_means(0.9, 0.85, 0.8), each = multip)
  horizon_tbl1 <- add_horizon_values(baseline_tbl1, initial_values, seed)
  horizon_tbl2 <- add_horizon_values(baseline_tbl2, initial_values, seed)
  dplyr::bind_rows(horizon_tbl1, horizon_tbl2) |>
    dplyr::arrange(dplyr::pick(c(
      "pod",
      "sitetret",
      "sex",
      "age_group",
      "measure",
      "model_run"
    )))
}


#' Function to create a "sex + tretspef_grouped" table with synthetic values
#' The table contains synthetic (randomly created) values for 2 sexes, a
#'  selection of tretspef group codes, and measures, across 2 "sites", as if
#'  created by 64 model runs
#' @inheritParams create_demo_default_tbl
#' @keywords internal
create_demo_sex_tretspef_tbl <- function(seed) {
  # fmt: skip
  tretspef_groups <- c(
    "100", "101", "110", "120", "130", "140", "150", "160", "170",
    "300", "301", "320", "330", "340", "400", "410", "430", "Other"
  )
  init_tbl1 <- list(create_ip_base_tbl(), create_op_base_tbl()) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      sitetret = "site1",
      sex = list(seq(2)),
      tretspef_grouped = list(tretspef_groups),
      .after = "pod"
    ) |>
    tidyr::unnest_longer("sex") |>
    tidyr::unnest_longer("tretspef_grouped")
  init_tbl2 <- dplyr::mutate(init_tbl1, sitetret = "site2")
  baseline_tbl1 <- add_baseline_values(init_tbl1, c(500L, 4.5e3L), 100L, seed)
  baseline_tbl2 <- add_baseline_values(init_tbl2, c(100L, 2.2e3L), 100L, seed)
  multip <- length(seq(2)) * length(tretspef_groups)
  initial_values <- rep(get_ipop_initial_means(0.8, 0.85), each = multip)
  horizon_tbl1 <- add_horizon_values(baseline_tbl1, initial_values, seed)
  horizon_tbl2 <- add_horizon_values(baseline_tbl2, initial_values, seed)
  dplyr::bind_rows(horizon_tbl1, horizon_tbl2) |>
    dplyr::arrange(dplyr::pick(c(
      "pod",
      "sitetret",
      "sex",
      "tretspef_grouped",
      "measure",
      "model_run"
    )))
}


#' Function to create a "step_counts" table with synthetic values
#' The table contains synthetic (randomly created) values for a selection of
#'  64 model runs, providing random baselines, random sizes of mitigated
#'  activity across a subset of TPMAs, and random values for demographic growth
#' @inheritParams create_demo_default_tbl
#' @keywords internal
create_demo_stepcounts_tbl <- function(seed) {
  tpma_lookup <- get_tpma_label_lookup()
  # for this demo table, we can just include a subset of all strategies
  n_strategies <- 40
  strategies <- dplyr::slice_sample(tpma_lookup, n = n_strategies) |>
    withr::with_seed(seed = seed) |>
    dplyr::select(!"tpma_label")
  core_cols <- c("sitetret", "pod", "measure")
  init_tbl1 <- create_base_tbl_with_sitetret("site1") |>
    dplyr::mutate(dplyr::across("measure", \(x) {
      dplyr::if_else(x %in% c("walk-in", "ambulance"), "arrivals", x)
    })) |>
    dplyr::distinct() |>
    dplyr::filter_out(.data[["measure"]] == "procedures") |>
    get_activity_type_from_pod() |>
    dplyr::inner_join(
      strategies,
      by = "activity_type",
      relationship = "many-to-many"
    ) |>
    dplyr::filter_out(
      dplyr::when_any(
        .data[["measure"]] == "admissions" &
          .data[["change_factor"]] == "efficiencies",
        .data[["measure"]] == "tele_attendances" &
          grepl("^convert_to_tele", .data[["strategy"]])
      )
    ) |>
    tidyr::nest(.by = c("activity_type", tidyselect::all_of(core_cols)))
  init_tbl2 <- dplyr::mutate(init_tbl1, sitetret = "site2")
  init_tbl <- dplyr::bind_rows(init_tbl1, init_tbl2)
  demo_data <- create_demo_default_tbl(seed) |>
    dplyr::mutate(dplyr::across("measure", \(x) {
      dplyr::if_else(x %in% c("walk-in", "ambulance"), "arrivals", x)
    })) |>
    dplyr::summarise(dplyr::across("value", sum), .by = !"value")
  baseline_data <- demo_data |>
    dplyr::filter(.data[["model_run"]] == 0L) |>
    dplyr::select(!"model_run") |>
    dplyr::rename(baseline = "value")
  horizon_data <- demo_data |>
    dplyr::filter(.data[["model_run"]] > 0L) |>
    dplyr::rename(horizon = "value")
  baseline_tbl <- init_tbl |>
    dplyr::left_join(baseline_data, core_cols) |>
    dplyr::left_join(horizon_data, core_cols) |>
    dplyr::mutate(mitigation = .data[["horizon"]] - .data[["baseline"]])
  dga_values <- withr::with_seed(seed, stats::rnorm(nrow(baseline_tbl), 1, 0.1))
  dga_values <- dga_values * (baseline_tbl[["baseline"]] * 0.01)
  impact_tbl <- baseline_tbl |>
    dplyr::mutate(
      demographic_adjustment = dga_values,
      impact_data = purrr::map2(
        .data[["data"]],
        .data[["mitigation"]],
        \(x, y) create_sample_impacts(x, y, seed)
      )
    )
  cfs <- c("baseline", "demographic_adjustment", "model_interaction_term")
  id_cols <- c("activity_type", core_cols, "model_run", "change_factor")
  impact_tbl |>
    dplyr::select(!c("data")) |>
    tidyr::nest(
      horizon_data = c(
        "model_run",
        "horizon",
        "mitigation",
        "demographic_adjustment"
      )
    ) |>
    dplyr::mutate(dplyr::across("horizon_data", \(x) {
      purrr::map2(x, .data[["impact_data"]], \(x, y) {
        dplyr::inner_join(x, y, "model_run")
      })
    })) |>
    dplyr::select(!"impact_data") |>
    tidyr::unnest("horizon_data") |>
    dplyr::mutate(
      total_mitigation = sum(.data[["value"]]),
      model_interaction_term = .data[["mitigation"]] -
        (.data[["total_mitigation"]] + .data[["demographic_adjustment"]]),
      .by = tidyselect::all_of(c(core_cols, "model_run"))
    ) |>
    dplyr::select(!c("horizon", "mitigation", "total_mitigation")) |>
    tidyr::pivot_wider(names_from = "strategy") |>
    tidyr::pivot_longer(
      !tidyselect::all_of(id_cols),
      names_to = "strategy",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      dplyr::across("change_factor", \(x) {
        dplyr::if_else(.data[["strategy"]] %in% cfs, .data[["strategy"]], x)
      }),
      dplyr::across("strategy", \(x) dplyr::if_else(x %in% cfs, "-", x))
    )
}


#' Helper function
#' @keywords internal
create_sample_impacts <- function(dat, mitigation, seed, picks = 64) {
  n_rows <- nrow(dat)
  n_zeros <- round(0.2 * n_rows * picks)
  n_picks <- (n_rows * picks) - n_zeros
  mitigation_mean <- mitigation / n_rows
  mitigation_dist <- withr::with_seed(seed, stats::rnorm(n_picks, 1, 0.5))
  mitigation_dist <- mitigation_dist * mitigation_mean

  impacts <- sample(c(rep(0, n_zeros), mitigation_dist), n_rows * picks) |>
    withr::with_seed(seed = seed)
  impacts_tbl <- tibble::tibble(
    rn = rep(seq(n_rows), each = picks),
    model_run = rep(seq(picks), n_rows),
    value = impacts
  )
  dat |>
    dplyr::mutate(rn = dplyr::row_number()) |>
    dplyr::left_join(impacts_tbl, "rn") |>
    dplyr::select(!"rn")
}


#' Helper function
#' @param site A string giving the name/code of an imaginary site
#' @keywords internal
create_base_tbl_with_sitetret <- function(site) {
  list(create_ae_base_tbl(), create_ip_base_tbl(), create_op_base_tbl()) |>
    purrr::list_rbind() |>
    dplyr::mutate(sitetret = {{ site }}, .after = "pod")
}


#' Helper function
#' @keywords internal
add_baseline_values <- function(base_tbl, baseline_range, step = 100L, seed) {
  stopifnot(length(baseline_range) == 2)
  baseline_range <- as.integer(baseline_range)
  step <- as.integer(step)
  poss_baselines <- seq(min(baseline_range), max(baseline_range), step)
  baseline_values <- sample(poss_baselines, nrow(base_tbl), TRUE) |>
    withr::with_seed(seed = seed)
  dplyr::mutate(base_tbl, model_run = 0L, value = baseline_values)
}


#' Helper function
#' @keywords internal
add_horizon_values <- function(tbl_with_baseline, initial_means, seed) {
  distrib_values <- stats::rnorm(nrow(tbl_with_baseline), 1, 0.1) |>
    withr::with_seed(seed = seed)
  mitigations <- distrib_values * initial_means
  ghv_partial <- purrr::partial(generate_horizon_values, seed = seed)
  horizons_tbl <- tbl_with_baseline |>
    dplyr::select(!"model_run") |>
    dplyr::mutate(
      dplyr::across("value", \(x) purrr::map2(x, mitigations, ghv_partial))
    ) |>
    tidyr::unnest_longer("value", indices_to = "model_run") |>
    dplyr::mutate(dplyr::across("value", \(x) as.integer(round(x))))
  dplyr::bind_rows(tbl_with_baseline, horizons_tbl)
}

#' Helper function
#' @keywords internal
generate_horizon_values <- function(baseline, mitigation, seed, picks = 64) {
  distr <- withr::with_seed(seed, stats::rnorm(picks, 1, 0.1))
  as.integer(round(distr * mitigation * baseline))
}


#' Helper function
#' @keywords internal
create_ae_base_tbl <- function() {
  ae_pods <- paste0("aae_type-0", seq(5))
  ae_measures <- c("ambulance", "walk-in")
  tidyr::expand_grid(pod = ae_pods, measure = ae_measures)
}

#' Helper function
#' @keywords internal
create_ip_base_tbl <- function() {
  ip_pods <- c(
    paste0("ip_", c("elective", "maternity", "non-elective"), "_admission"),
    paste0("ip_regular_", c("night", "day"), "_attender")
  )
  ip_measures <- c("admissions", "beddays", "procedures")
  tidyr::expand_grid(pod = ip_pods, measure = ip_measures)
}

#' Helper function
#' @keywords internal
create_op_base_tbl <- function() {
  op_pods <- paste0("op_", c("first", "follow-up", "procedure"))
  op_measures <- paste0(c("", "tele_"), "attendances")
  tidyr::expand_grid(pod = op_pods, measure = op_measures)
}

#' Helper function
#' @keywords internal
get_full_initial_means <- function(ae_mean, ip_mean, op_mean) {
  c(
    rep(ae_mean, nrow(create_ae_base_tbl())),
    rep(ip_mean, nrow(create_ip_base_tbl())),
    rep(op_mean, nrow(create_op_base_tbl()))
  )
}


#' Helper function
#' @keywords internal
get_ipop_initial_means <- function(ip_mean, op_mean) {
  c(
    rep(ip_mean, nrow(create_ip_base_tbl())),
    rep(op_mean, nrow(create_op_base_tbl()))
  )
}
