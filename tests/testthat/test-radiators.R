test_that("new_cast_iron calculates nominal power correctly", {
  # Create a known radiator
  rad <- new_cast_iron("Test Rad", height = 600, depth = 140, n_elements = 10)

  # Expect the nominal power to be a positive number
  expect_gt(rad$p_nom, 0)

  # Check if class is assigned correctly
  expect_s3_class(rad, "cast_iron")
  expect_s3_class(rad, "radiator")
})

test_that("Simulation respects physics (Output < Input Temp)", {
  rad <- new_cast_iron("Test Rad", 600, 140, 10)
  sim <- simulate_output(rad, t_in = 70, t_room = 20, flow_rate = 0.05)

  # The water must cool down as it moves through the radiator
  expect_lt(sim$state$t_out, 70)
})

test_that("Radiator respects energy balance", {
  # Setup
  rad <- new_cast_iron("ConservationTest", height = 600, depth = 140, n_elements = 20)
  t_in <- 80
  t_room <- 20
  flow_rate <- 0.02 # kg/s
  cp_water <- 4186  # J/kgK

  # Run simulation
  res <- simulate_output(rad, t_in, t_room, flow_rate)

  # 1. Calculate Q from the water temperature drop
  delta_t_water <- res$state$t_in - res$state$t_out
  q_from_water <- flow_rate * cp_water * delta_t_water

  # 2. Compare with the Q the radiator claimed to emit
  # Use expect_equal with a small tolerance for floating point math
  expect_equal(res$state$q_actual, q_from_water, tolerance = 1e-3)
})
