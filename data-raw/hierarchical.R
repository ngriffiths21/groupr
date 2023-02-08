hierarchical <- tibble::tribble(
  ~grp, ~type, ~subgrp, ~val,
  1, "sub", "sub1", 1.9,
  1, "sub", "sub2", 3.1,
  2, "sub", "sub1", 9.7,
  2, "sub", "sub2", 3.8,
  2, "main", "na", 4.0
)

scientists <- tibble::tribble( 
  ~ type, ~ person, ~ city, ~ day, ~ val,
  "Age", "Alice", "na", NA_real_, 25,
  "Location", "Alice", "Boston", NA_real_, 1,
  "Location", "Alice", "Worcester", NA_real_, 4,
  "Age", "Bob", "na", NA_real_, 31,
  "Location", "Bob", "Boston", NA_real_, 2,
  "Location", "Bob", "Worcester", NA_real_, 9,
  "Age", "Charlie", "na", NA_real_, 35,
  "Location", "Charlie", "Boston", NA_real_, 3,
  "Location", "Charlie", "Worcester", NA_real_, 10,
  "Value", "Bob", "Boston", 1, 10,
  "Value", "Bob", "Boston", 2, 12,
  "Value", "Alice", "Boston", 1, 18,
  "Value", "Alice", "Boston", 2, 19,
  "Value", "Alice", "Boston", 3, 20,
  "Value", "Bob", "Worcester", 1, 15,
  "Value", "Bob", "Worcester", 2, 17,
  "Value", "Bob", "Worcester", 3, 19,
  "Value", "Alice", "Worcester", 1, 21,
  "Value", "Alice", "Worcester", 3, 23,
  "Value", "Charlie", "Worcester", 1, 19,
  "Value", "Charlie", "Worcester", 2, 20,
  "Value", "Charlie", "Worcester", 3, 21
)
  
usethis::use_data(scientists, overwrite = TRUE)

usethis::use_data(hierarchical, overwrite = TRUE)
