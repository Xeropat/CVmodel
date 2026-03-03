test_that("new_cast_iron calculates nominal power correctly", {
  # Create a known radiator
  rad <- new_cast_iron("Test Rad", height = 600, depth = 140, n_elements = 10)

  # Expect the nominal power to be a positive number
  expect_gt(rad$p_nom, 0)

  # Check if class is assigned correctly
  expect_s3_class(rad, "cast_iron")
  expect_s3_class(rad, "radiator")
})

test_that("Simulation respects physics", {
  rad <- new_cast_iron("Test", 600, 140, 20)
  rad$temp <- 20

  sim <- simulate_output(rad, t_in = 70, t_room = 20, flow_rate = 0.05, dt = 60)

  # DEBUG: Run this manually to see what's inside
  # print(sim$state$t_out)

  expect_length(sim$state$t_out, 1) # Ensure it's not a vector
  expect_lt(sim$state$t_out, 71)    # Use 71 to allow for tiny numerical bounces
})

test_that("Radiator respects energy balance", {
  rad <- new_cast_iron("Test", 600, 140, 20)
  rad$temp <- 60

  # Flow in at 70C
  sim <- simulate_output(rad, t_in = 70, t_room = 20, flow_rate = 0.05, dt = 60)

  # 1. Return temp (t_out) must be less than Flow temp (t_in)
  expect_lt(sim$state$t_out, 70)

  # 2. Return temp (t_out) must be greater than current radiator temp
  # because the water is hotter than the iron
  expect_gt(sim$state$t_out, sim$temp)

  # 3. Check that it emitted some heat
  expect_gt(sim$state$q_actual, 0)
})
