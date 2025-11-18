compile_overall_cf_data <- function(
  dat,
  measure,
  include_baseline
) {
  bsline_filtered <- dplyr::filter(dat, .data[["change_factor"]] != "baseline")
  dat_prepared <- ifelse(include_baseline, dat, bsline_filtered)

  measure_data <- dat_prepared |>
    dplyr::filter(
      dplyr::if_any("measure", \(x) x == {{ measure }}) &
        dplyr::if_any("value", \(x) x != 0)
    ) |>
    dplyr::summarise(dplyr::across("value", sum), .by = "change_factor") |>
    dplyr::mutate(
      dplyr::across("change_factor", \(x) {
        forcats::fct_reorder(x, .data[["value"]], .desc = TRUE)
      })
    )

  bl_first <- measure_data |>
    dplyr::mutate(
      # baseline may now not be the first item, move it back to start
      dplyr::across("change_factor", \(x) forcats::fct_relevel(x, "baseline"))
    )
  measure_data <- if (include_baseline) bl_first else measure_data

  estimate_row <- tibble::tibble_row(
    change_factor = "Estimate",
    hidden = 0,
    total = sum(measure_data[["value"]])
  )

  measure_data |>
    dplyr::mutate(
      cuvalue = cumsum(.data[["value"]]),
      hidden = dplyr::lag(.data[["cuvalue"]], 1, 0) + pmin(.data[["value"]], 0)
    ) |>
    dplyr::select(c("change_factor", "hidden", total = "value")) |>
    dplyr::bind_rows(estimate_row)
  # tidyr::pivot_longer(c("value", "hidden"))
}
