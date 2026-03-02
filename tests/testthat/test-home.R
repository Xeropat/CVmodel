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
  my_room <- new_room("Office", volume = 30, temp_initial = 15, u_value = 10)
  my_rad <- new_cast_iron("Rad", 600, 140, 20)

  # Simulate radiator state (hot water 70C)
  my_rad <- simulate_output(my_rad, t_in = 70, t_room = 15, flow_rate = 0.05)

  # Advance room by 1 hour (3600 seconds)
  new_room_state <- update_room(my_room, my_rad, temp_outdoor = 0, dt = 3600)

  # The room should be warmer than 15°C
  expect_gt(new_room_state$temp, 15)
})
