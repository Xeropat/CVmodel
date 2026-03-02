test_that("it works", {
  # Example usage:
  # A 600mm high, 140mm deep radiator with 20 sections
  my_old_rad <- new_cast_iron("Dining Room", height = 600, depth = 140, n_elements = 20)

  # print(my_old_rad$p_nom)
  # Should give roughly 2380 Watts at 80/60/20

    # Expect the nominal power to be a positive number
  expect_equal(my_old_rad$p_nom, 2380)
})

test_that("Room warms up when radiator is on", {
  # Setup
  my_room <- new_room("TestRoom", volume = 40, temp_initial = 15, u_value = 20)

  # Simulate a 2000W heat source for 10 minutes (600s)
  # We use a mock radiator state or just pass the gain value
  new_room_state <- update_room(my_room, list(state = list(q_actual = 2000)),
                                temp_outdoor = 0, dt = 600)

  # Check that it warmed up significantly but realistically
  expect_gt(new_room_state$temp, 15)
  expect_lt(new_room_state$temp, 25) # Should be ~19.23
  expect_equal(new_room_state$temp, 19.23, tolerance = 0.01)
})
