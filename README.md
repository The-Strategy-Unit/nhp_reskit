# reskit ![R](https://www.r-project.org/favicon-32x32.png) 📦🍪🦺

<!-- badges: start -->
[![License: MIT][mit_svg]](https://opensource.org/licenses/MIT)
[![Project Status: Active – The project has reached a stable, usable state and
is being actively developed][repostatus_svg]][repostatus_info]
![GitHub R package version][gh_ver]
[![R CMD check status][cmd_svg]][cmd_yaml]

[mit_svg]: https://img.shields.io/badge/License-MIT-yellow.svg?label=licence
[repostatus_svg]: https://www.repostatus.org/badges/latest/active.svg
[repostatus_info]: https://www.repostatus.org/#active
[gh_ver]: https://img.shields.io/github/r-package/v/The-Strategy-Unit/azkit?logo=r&label=version
[cmd_svg]: https://github.com/The-Strategy-Unit/nhp_reskit/actions/workflows/R-CMD-check.yaml/badge.svg?event=release
[cmd_yaml]: https://github.com/The-Strategy-Unit/nhp_reskit/actions/workflows/R-CMD-check.yaml
<!-- badges: end -->

An R package (just called `reskit`) that helps process NHP model results.


## Status

The package is now at version 0.5 and contains a range of useful functions.
While of course it is still in development, and we may make further signficant
changes to the user interface for some functions, the package is now ready to be
used.

Using it will be very helpful as it will no doubt surface some bugs or areas
where the documentation needs to be improved.


## Installation

You should be able to run the following R command to install {reskit}:

```r
# install.packages("pak") # if not already installed
pak::pak("The-Strategy-Unit/nhp_reskit")
```

On Windows, you may need to have [RTools](https://cloud.r-project.org/)
already installed in order to install reskit.


## Usage

You will need certain environment variables to be available (see section below).

Some key functions and workflows you might want to use include:

#TODO

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
