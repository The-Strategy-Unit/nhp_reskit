# reskit ![R](https://www.r-project.org/favicon-32x32.png) 📦🍪🦺

<!-- badges: start -->
![GitHub License][gh_licence]
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable release][repostatus_svg]][repostatus_info]
[![Lifecycle: experimental][lifecycle_svg]][lifecycle]
![GitHub R package version][gh_ver]

[gh_licence]: https://img.shields.io/github/license/The-Strategy-Unit/nhp_reskit
[gh_ver]: https://img.shields.io/github/r-package/v/The-Strategy-Unit/nhp_reskit
[repostatus_info]: https://www.repostatus.org/#wip
[repostatus_svg]: https://www.repostatus.org/badges/latest/wip.svg
[lifecycle]: https://lifecycle.r-lib.org/articles/stages.html#experimental
[lifecycle_svg]: https://img.shields.io/badge/lifecycle-experimental-orange.svg
<!-- badges: end -->

An R package (just called `{reskit}`) helping process NHP model results.

## Status

This is just a framework repository at present, while the package scope and
design are being explored.
This README will be fleshed out as the package is developed.

## Installation

```r
# install.packages("pak") # if not already installed
pak::pak("The-Strategy-Unit/nhp_reskit")
```

## Usage

You will need certain environment variables to be available (see section below).

Some key functions and workflows you might want to use include:

```r
# Compiles a table of metadata for each model run for each scheme
compile_run_metadata_tbl()

# Get the Azure storage container for results data
get_results_container() # uses the environment variable "AZ_RESULTS_CONTAINER"

# Return a vector of provider codes from the supporting data container
get_providers() # requires the environment variable "AZ_SUPPORT_CONTAINER"

# Get the path to a folder of results data
# (the below example will succeed only if there is a single scenario and a
# single model run within the particular version and scheme combination below)
get_results_folder_path(version = "v4.0", scheme = "national", scenario = "test")

# Read all parquet data files from a location
results_dir <- get_results_folder_path(version = "v4.0", scheme = "national")
read_results_parquet_files(results_dir)

# Read only selected results files from multiple scenarios
c("scenario1", "scenario2") |>
  purrr::map(\(x) get_results_folder_path("v3.6", "RZZ", scenario = x)) |>
  purrr::map(\(x) read_results_parquet_files(x, tables = c("default", "age")))
```


## Environment variables

To access Azure Storage you need to add some variables to a
[`.Renviron` file][posit_env] in your project.

⚠️These values are sensitive and should not be exposed to anyone outside The
Strategy Unit.
Make sure you include `.Renviron` in [the `.gitignore` file][github] for
your project.

Your `.Renviron` file should contain the variables below.
Ask a member of [the Data Science team][suds] for the necessary values.

```
AZ_STORAGE_EP =
AZ_SUPPORT_CONTAINER =
AZ_RESULTS_CONTAINER =
AZ_RESULTS_DIRECTORY =
```

An example `.Renviron.example` file is provided in this repository.
Copy it, renamed as just `.Renviron`, into the root of any project folder where
you are using {reskit}.

## Getting help

Please use the [Issues][issues] feature on GitHub to report any bugs, ideas
or problems, including with the package documentation.

Alternatively, to ask any questions about the package you may contact
[Fran Barton](mailto:francis.barton@nhs.net).

[posit_env]: https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html#renviron
[github]: https://docs.github.com/en/get-started/getting-started-with-git/ignoring-files
[suds]: https://the-strategy-unit.github.io/data_science/about.html
[issues]: https://github.com/The-Strategy-Unit/nhp_reskit/issues
