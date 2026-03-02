# Extracted from test-home.R:10

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "CVmodel", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
my_old_rad <- new_cast_iron("Dining Room", height = 600, depth = 140, n_elements = 20)
print(my_old_rad$p_nom)
expect_eq(my_old_rad$p_nom, 2380)
