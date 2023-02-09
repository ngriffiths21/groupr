
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groupr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/groupr)](https://CRAN.R-project.org/package=groupr)
[![R-CMD-check](https://github.com/ngriffiths21/groupr/workflows/R-CMD-check/badge.svg)](https://github.com/ngriffiths21/groupr/actions?query=workflow%3AR-CMD-check)
[![Codecov test
coverage](https://codecov.io/gh/ngriffiths21/groupr/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ngriffiths21/groupr?branch=master)

<!-- badges: end -->

The groupr package is designed to work with tibbles and dplyr. It
provides replacements for tidyverse grouping and pivoting operations,
and uses richer data structures to make these operations easier to think
about.

## Motivation

There are two main ideas behind this package. First is the idea of
[inapplicable
data](https://towardsdatascience.com/richer-missing-values-dea7377f5541).
While we often use `NA` as a placeholder for unknown but important
information, R doesn’t provide a way to mark data that should definitely
be ignored. `groupr` provides an inapplicable value (printed `<I>`).

The second main idea is that pivoting is just a way to [rearrange groups
of data](https://epinotes.netlify.app/post/pivoting/). Some kinds of
pivots cannot be expressed by a single tidyr pivot statement and require
two or even three consecutive pivot calls. Inapplicable groups can be
used to describe some of these more complex operations in a very
straightforward way.

## Usage

### Install

``` r
devtools::install_github("ngriffiths21/groupr")
```

### Easier pivots using groups

``` r
library(groupr)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
```

Make columns out of row groups:

``` r
p_df2
#> # A tibble: 5 × 3
#>   grp1   grp2   val
#>   <chr> <dbl> <dbl>
#> 1 A         1   1.9
#> 2 A         2  10.1
#> 3 B         2   3.1
#> 4 B         1   4.7
#> 5 C        NA   4.9

# group and make the NA an inapplicable grouping
p_df2 <- group_by2(p_df2, grp1, grp2 = NA)

group_data(p_df2)
#> # A tibble: 5 × 3
#>         grp1       grp2       .rows
#>   <polymiss> <polymiss> <list<int>>
#> 1          A          1         [1]
#> 2          A          2         [1]
#> 3          B          1         [1]
#> 4          B          2         [1]
#> 5          C        <I>         [1]

# groups version of pivot
pivot_grps(p_df2, cols = "grp1")
#> # A tibble:    2 × 2
#> # Row indices: grp2 [2]
#> # Col index:   grp1
#>    grp2 val$A    $B    $C
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1   1.9   4.7   4.9
#> 2     2  10.1   3.1   4.9

# tidyr version
pivot_wider(p_df2, names_from = grp1, values_from = val)
#> # A tibble: 3 × 4
#>    grp2     A     B     C
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1   1.9   4.7  NA  
#> 2     2  10.1   3.1  NA  
#> 3    NA  NA    NA     4.9
```

Note that with this inapplicable grouping, the value from the “C” group
is applied to both subgroups. This behavior is not possible using tidyr.

Make row groups out of columns (pivot longer):

``` r
p_df <- group_by2(iris, Species)

# groups version: make column grouping, then pivot
colgrouped <- sep_colgrp(p_df, ".", index_name = "Measurement")
colgrouped
#> # A tibble:    150 × 3
#> # Row indices: Species [3]
#> # Col index:   Measurement
#>    Species Sepal$Length $Width Petal$Length $Width
#>    <fct>          <dbl>  <dbl>        <dbl>  <dbl>
#>  1 setosa           5.1    3.5          1.4    0.2
#>  2 setosa           4.9    3            1.4    0.2
#>  3 setosa           4.7    3.2          1.3    0.2
#>  4 setosa           4.6    3.1          1.5    0.2
#>  5 setosa           5      3.6          1.4    0.2
#>  6 setosa           5.4    3.9          1.7    0.4
#>  7 setosa           4.6    3.4          1.4    0.3
#>  8 setosa           5      3.4          1.5    0.2
#>  9 setosa           4.4    2.9          1.4    0.2
#> 10 setosa           4.9    3.1          1.5    0.1
#> # … with 140 more rows
pivot_grps(colgrouped, rows = "Measurement")
#> # A tibble:    300 × 4
#> # Row indices: Species, Measurement [6]
#>    Species Measurement Sepal Petal
#>    <fct>   <chr>       <dbl> <dbl>
#>  1 setosa  Length        5.1   1.4
#>  2 setosa  Length        4.9   1.4
#>  3 setosa  Length        4.7   1.3
#>  4 setosa  Length        4.6   1.5
#>  5 setosa  Length        5     1.4
#>  6 setosa  Length        5.4   1.7
#>  7 setosa  Length        4.6   1.4
#>  8 setosa  Length        5     1.5
#>  9 setosa  Length        4.4   1.4
#> 10 setosa  Length        4.9   1.5
#> # … with 290 more rows

# tidyr version
pivot_longer(iris, cols = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
             values_to = "value")
#> # A tibble: 600 × 3
#>    Species name         value
#>    <fct>   <chr>        <dbl>
#>  1 setosa  Sepal.Length   5.1
#>  2 setosa  Sepal.Width    3.5
#>  3 setosa  Petal.Length   1.4
#>  4 setosa  Petal.Width    0.2
#>  5 setosa  Sepal.Length   4.9
#>  6 setosa  Sepal.Width    3  
#>  7 setosa  Petal.Length   1.4
#>  8 setosa  Petal.Width    0.2
#>  9 setosa  Sepal.Length   4.7
#> 10 setosa  Sepal.Width    3.2
#> # … with 590 more rows
```

Using this approach we can preserve separate columns for each flower
part rather than combining them into one.

Pivot both rows and columns:

``` r
p_df3 <- pivot_grps(p_df2, cols = "grp1")

p_df3
#> # A tibble:    2 × 2
#> # Row indices: grp2 [2]
#> # Col index:   grp1
#>    grp2 val$A    $B    $C
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1   1.9   4.7   4.9
#> 2     2  10.1   3.1   4.9

# groups version
pivot_grps(p_df3, rows = "grp1",
           cols = "grp2")
#> # A tibble:    3 × 2
#> # Row indices: grp1 [3]
#> # Col index:   grp2
#>   grp1  val$`1`  $`2`
#>   <chr>   <dbl> <dbl>
#> 1 A         1.9  10.1
#> 2 B         4.7   3.1
#> 3 C         4.9   4.9
```

## Lifecycle

At this point this is experimental, with limited testing. The API is
likely to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)
