
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groups

<!-- badges: start -->

![R-CMD-check](https://github.com/ngriffiths21/groups/workflows/R-CMD-check/badge.svg)

<!-- badges: end -->

The groups package is designed to work with tibbles and dplyr. It
provides replacements for tidyverse grouping and pivoting operations,
and uses richer data structures to make these operations easier to think
about.

## Motivation

There are two main ideas behind this package. First is the idea of
[inapplicable
data](https://towardsdatascience.com/richer-missing-values-dea7377f5541).
While we often use `NA` as a placeholder for unknown but important
information, R doesn’t provide a way to mark data that should definitely
be ignored. `groups` provides an inapplicable value (printed `<I>`).

This makes a few different operations easier:

**Calculation on a subset**. In a dataset containing two groups, A and
B, it is easier to express that we want to calculate the square roots of
all A values, store them in a new column, and fill the remaining rows
with `NA`.

**Hierarchical groups**. In a dataset with two grouping variables,
`main` and `subgroup`, we can set `subgroup` to `<I>` when we want to
indicate that the data applies to the entire main group.

The second main idea is that pivoting is just a way to [rearrange groups
of data](https://epinotes.netlify.app/post/pivoting/). Some kinds of
pivots cannot be expressed by a single tidyr pivot statement and require
two or even three consecutive pivot calls. Inapplicable groups can be
used to describe some of these more complex operations in a very
straightforward way.

## Usage

### Install

``` r
devtools::install_github("ngriffiths21/groups")
```

### Easier pivots using groups

``` r
library(groups)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
```

Make columns out of row groups:

``` r
p_df2
#> # A tibble: 5 x 3
#>   grp1   grp2   val
#>   <chr> <dbl> <dbl>
#> 1 A         1   1.9
#> 2 A         2  10.1
#> 3 B         2   3.1
#> 4 B         1   4.7
#> 5 C        NA   4.9

# group and make the NA an inapplicable grouping
p_df2 <- group_by2(p_df2, grp1 = NULL, grp2 = NA)

group_data(p_df2)
#> # A tibble: 5 x 3
#>         grp1       grp2       .rows
#>   <polymiss> <polymiss> <list<int>>
#> 1          A          1         [1]
#> 2          A          2         [1]
#> 3          B          1         [1]
#> 4          B          2         [1]
#> 5          C        <I>         [1]

# groups version of pivot
pivot_grps(p_df2, cols = "grp1")
#> # A tibble: 2 x 4
#> # Groups:   grp2 [2]
#>    grp2 val_A val_B val_C
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1   1.9   4.7   4.9
#> 2     2  10.1   3.1   4.9

# tidyr version
pivot_wider(p_df2, names_from = grp1, values_from = val)
#> # A tibble: 3 x 4
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
p_df <- group_by2(iris, Species = NULL)

# groups version
pivot_grps(p_df, rows = list(value = c("Sepal.Length", "Sepal.Width")))
#> # A tibble: 300 x 3
#> # Groups:   Species, name [6]
#>    Species name         value
#>    <fct>   <chr>        <dbl>
#>  1 setosa  Sepal.Length   5.1
#>  2 setosa  Sepal.Length   4.9
#>  3 setosa  Sepal.Length   4.7
#>  4 setosa  Sepal.Length   4.6
#>  5 setosa  Sepal.Length   5  
#>  6 setosa  Sepal.Length   5.4
#>  7 setosa  Sepal.Length   4.6
#>  8 setosa  Sepal.Length   5  
#>  9 setosa  Sepal.Length   4.4
#> 10 setosa  Sepal.Length   4.9
#> # … with 290 more rows

# tidyr version
pivot_longer(iris, cols = c(Sepal.Length, Sepal.Width), values_to = "value")
#> # A tibble: 300 x 5
#>    Petal.Length Petal.Width Species name         value
#>           <dbl>       <dbl> <fct>   <chr>        <dbl>
#>  1          1.4         0.2 setosa  Sepal.Length   5.1
#>  2          1.4         0.2 setosa  Sepal.Width    3.5
#>  3          1.4         0.2 setosa  Sepal.Length   4.9
#>  4          1.4         0.2 setosa  Sepal.Width    3  
#>  5          1.3         0.2 setosa  Sepal.Length   4.7
#>  6          1.3         0.2 setosa  Sepal.Width    3.2
#>  7          1.5         0.2 setosa  Sepal.Length   4.6
#>  8          1.5         0.2 setosa  Sepal.Width    3.1
#>  9          1.4         0.2 setosa  Sepal.Length   5  
#> 10          1.4         0.2 setosa  Sepal.Width    3.6
#> # … with 290 more rows
```

In addition, groups supports creating multiple new columns, which is not
possible in tidyr.

Pivot both rows and columns:

``` r
p_df3 <- pivot_grps(p_df2, cols = "grp1")

p_df3
#> # A tibble: 2 x 4
#> # Groups:   grp2 [2]
#>    grp2 val_A val_B val_C
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1   1.9   4.7   4.9
#> 2     2  10.1   3.1   4.9

# groups version
pivot_grps(p_df3, rows = list(val = c("val_A", "val_B", "val_C")),
           cols = "grp2")
#> # A tibble: 3 x 3
#> # Groups:   name [3]
#>   name  val_1 val_2
#>   <chr> <dbl> <dbl>
#> 1 val_A   1.9  10.1
#> 2 val_B   4.7   3.1
#> 3 val_C   4.9   4.9

# tidyr version
pivot_wider(
  pivot_longer(p_df3, cols = c(val_A, val_B, val_C), values_to = "val"),
  names_from = grp2,
  values_from = val
)
#> # A tibble: 3 x 3
#>   name    `1`   `2`
#>   <chr> <dbl> <dbl>
#> 1 val_A   1.9  10.1
#> 2 val_B   4.7   3.1
#> 3 val_C   4.9   4.9
```

## Lifecycle

This package is currently experimental. At this point it has only
undergone limited testing and it is not optimized for performance. The
API is likely to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)
