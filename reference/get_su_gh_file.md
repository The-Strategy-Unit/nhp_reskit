# Read in a file from a Strategy Unit GitHub repo

Read in a file from a Strategy Unit GitHub repo

## Usage

``` r
get_su_gh_file(repo, folder, file)
```

## Arguments

- repo:

  string. The name of the repository in which to find the file

- folder:

  string. The folder where the file is located. Set to `""` to use the
  root folder of the repo.

- file:

  string. The name of the file to read in

## Value

The URL to the raw file contents, to be passed to a reader function
