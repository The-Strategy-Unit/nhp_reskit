# reskit ![R](https://www.r-project.org/favicon-32x32.png) 📦🍪🦺

<!-- badges: start -->
![GitHub License][gh_licence]
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable release][repostatus_svg]][repostatus_info]
[![Lifecycle: experimental][lifecycle_svg]][lifecycle]
![GitHub R package version][gh_ver]

[gh_licence]: https://img.shields.io/github/license/The-Strategy-Unit/nhp_reskit
[gh_ver]: https://img.shields.io/github/r-package/v/The-Strategy-Unit/nhp_reskit
[repostatus_info]: https://www.repostatus.org/#project-statuses
[repostatus_svg]: https://www.repostatus.org/badges/latest/wip.svg
[lifecycle]: https://lifecycle.r-lib.org/articles/stages.html#experimental
[lifecycle_svg]: https://img.shields.io/badge/lifecycle-experimental-orange.svg
<!-- badges: end -->

An R package (just called `{reskit}`) helping process NHP model results.

## Status

This is just a framework repository at present, while the package scope and
design are being explored.
This README will be fleshed out as the package is developed.

## Usage

_To be added._

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
AZ_STORAGE_EP=
AZ_STORAGE_CONTAINER=
```

These may vary depending on the specific container you’re connecting to.

## Getting help

Please use the [Issues][issues] feature on GitHub to report any bugs, ideas
or problems, including with the package documentation.

Alternatively, to ask any questions about the package you may contact
[Fran Barton](mailto:francis.barton@nhs.net).

[posit_env]: https://docs.posit.co/ide/user/ide/guide/environments/r/managing-r.html#renviron
[github]: https://docs.github.com/en/get-started/getting-started-with-git/ignoring-files
[suds]: https://the-strategy-unit.github.io/data_science/about.html
[issues]: https://github.com/The-Strategy-Unit/nhp_reskit/issues
