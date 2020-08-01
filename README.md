
<!-- README.md is generated from README.Rmd. Please edit that file -->

# groups

<!-- badges: start -->

<!-- badges: end -->

The groups package provides a more powerful version of grouped tibbles
from dplyr. It allows groups in a tibble to be marked inapplicable,
which is a simple but widely useful way to express structure in a
dataset. It also provides replacements for common tidyverse functions
that are centered around groupings with potentially inapplicable values.

## Motivation

Groupings with inapplicable values can make many data cleaning tasks
more straightforward to express. Here are some examples:

**Calculation on a subset**. In a dataset containing two groups, A and
B, it is not easy to express that you want to calculate the square roots
of all A values, storing them in a new column, and filling the remaining
rows with NA. This can easily be expressed if B can be marked as an
inapplicable group.

**Hierarchical groups**. Imagine you have groups A and B, and within A
you also have subgroups S1 and S2. If you have some data that is
relevant to the entirety of group A, you must include the rows in both
subgroups. An “inapplicable” subgroup can indicate that the data applies
to the whole primary group.

**Pivoting**. Pivoting operations essentially [convert groups of
data](https://epinotes.netlify.app/post/pivoting/) into columns of data,
and vice versa. Some kinds of pivots cannot be expressed by a single
tidyr pivot statement and require two or even three consecutive pivot
calls. Inapplicable groups can be used to describe some of these more
complex operations in a very straightforward way.

## Usage

Install:

``` r
devtools::install_github("ngriffiths21/groups")
```

Group and mark inapplicables:

``` r
library(groups)
igrped_df <- group_by2(iris, Species = "setosa")
dplyr::group_data(igrped_df)
#> # A tibble: 3 x 2
#>      Species       .rows
#>   <polymiss> <list<int>>
#> 1        <I>        [50]
#> 2 versicolor        [50]
#> 3  virginica        [50]
```

Make row groups out of columns (pivot longer):

``` r
p_df <- group_by2(iris, Species = NULL)
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
```

Make columns out of row groups:

``` r
p_df2
#> # A tibble: 5 x 3
#> # Groups:   grp1, grp2 [5]
#>   grp1   grp2   val
#>   <chr> <dbl> <dbl>
#> 1 A         1   1.9
#> 2 A         2  10.1
#> 3 B         2   3.1
#> 4 B         1   4.7
#> 5 C         2   4.9

pivot_grps(p_df2, cols = "grp2")
#> # A tibble: 3 x 3
#> # Groups:   grp1 [3]
#>   grp1  val_1 val_2
#>   <chr> <dbl> <dbl>
#> 1 A       1.9  10.1
#> 2 B       4.7   3.1
#> 3 C      NA     4.9
```

Both operations in one call:

``` r
p_df3 <- pivot_grps(p_df2, cols = "grp2")

pivot_grps(p_df3, rows = list(val = c("val_1", "val_2")),
           cols = "grp1")
#> # A tibble: 2 x 4
#> # Groups:   name [2]
#>   name  val_A val_B val_C
#>   <chr> <dbl> <dbl> <dbl>
#> 1 val_1   1.9   4.7  NA  
#> 2 val_2  10.1   3.1   4.9
```

## Lifecycle

This package is experimental and the API is expected to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)
