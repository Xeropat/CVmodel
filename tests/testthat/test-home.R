test_that("it works", {
  # Example usage:
  # A 600mm high, 140mm deep radiator with 20 sections
  my_old_rad <- new_cast_iron("Dining Room", height = 600, depth = 140, n_elements = 20)

  # print(my_old_rad$p_nom)
  # Should give roughly 2380 Watts at 80/60/20

    # Expect the nominal power to be a positive number
  expect_equal(my_old_rad$p_nom, 2380)
})
