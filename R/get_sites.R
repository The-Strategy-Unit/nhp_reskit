get_sites <- function(scheme_code, container = NULL, sites_file = "sites") {
  container_name <- container %||% Sys.getenv("AZ_SUPPORT_CONTAINER")
  sites <- azkit::get_container(container_name) |>
    azkit::read_azure_json(sites_file)
  scheme_sites <- purrr::keep_at(sites, scheme_code)

  if (length(scheme_sites) == 0) {
    cli::cli_abort(
      "No sites listed for {.var {scheme_code}} in {.file {sites_file}}."
    )
  }

  scheme_sites[[scheme_code]][["sites"]] # list with elements 'aae', 'ip', 'op'
}
