
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

Group a tibble:

``` r
library(groups)
igrped_df <- group_by2(mtcars, gear = 5, vs = NULL)
dplyr::group_data(igrped_df)
#> # A tibble: 6 x 3
#>         gear         vs       .rows
#>   <polymiss> <polymiss> <list<int>>
#> 1          3          0        [12]
#> 2          3          1         [3]
#> 3          4          0         [2]
#> 4          4          1        [10]
#> 5        <I>          0         [4]
#> 6        <I>          1         [1]
```

## Lifecycle

This package is experimental and the API is expected to change.

## License

[MIT](https://choosealicense.com/licenses/mit/)
