# Read in a file from a Strategy Unit GitHub repo

Read in a file from a Strategy Unit GitHub repo

## Usage

``` r
get_su_gh_file(repo, folder, path)
```

## Arguments

- repo:

  string. The name of the repository in which to find the file

- folder:

  string. The folder where the file is located. Set to `""` to use the
  root folder of the repo.

- path:

  string. The path relative to `folder` to the file to read in

## Value

The URL to the raw file contents, to be passed to a reader function
