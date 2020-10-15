hierarchical <- tibble::tribble(
  ~grp, ~type, ~subgrp, ~val,
  1, "sub", "sub1", 1.9,
  1, "sub", "sub2", 3.1,
  2, "sub", "sub1", 9.7,
  2, "sub", "sub2", 3.8,
  2, "main", "na", 4.0
)

usethis::use_data(hierarchical, overwrite = TRUE)
