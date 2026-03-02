#' Create a Radiator Object
#' @param name Character name of the radiator (e.g., "Living Room")
#' @param nominal_power Watts at Delta T = 50 (e.g., 2000)
#' @param exponent Radiator exponent (n), usually 1.3 for steel panels
#' @return An object of class 'radiator'
#' @export
new_radiator <- function(name, nominal_power, exponent = 1.3) {
  structure(
    list(
      name = name,
      p_nom = nominal_power,
      n = exponent,
      state = list(t_in = 0, t_out = 0, q_actual = 0)
    ),
    class = "radiator"
  )
}


#' Print method for Radiator objects
#'
#' @param x A radiator object
#' @param ... Additional arguments passed to print
#' @export
#' @keywords internal
print.radiator <- function(x, ...) {
  cat("Radiator:", x$name, "\n")
  cat("  Nominal Power:", x$p_nom, "W\n")
  cat("  Current Output:", round(x$state$q_actual, 1), "W\n")
  cat("  Temps (In/Out):", x$state$t_in, "degC /", x$state$t_out, "degC\n")
}

#' Simulate Radiator Output
#' @param x A radiator object
#' @param t_in Flow temperature from boiler (°C)
#' @param t_room Current room temperature (°C)
#' @param flow_rate Water flow (kg/s)
#' @param dt Time step in seconds
#' @export
simulate_output <- function(x, t_in, t_room, flow_rate, dt = 60) {
  cp_water <- 4186

  # 1. Calculate heat emission based on CURRENT radiator temp
  # (Not the water temp, because the iron has to warm up first!)
  q_emitted <- x$p_nom * ((x$temp - t_room) / 50)^x$n

  # 2. Calculate heat gained from the water flow
  # We assume the water cools down to the radiator's current temperature
  q_from_water <- flow_rate * cp_water * (t_in - x$temp)

  # 3. Energy Balance: Net energy staying in the radiator (Joules)
  # Net = (Heat in from water) - (Heat out to room)
  net_energy <- (q_from_water - q_emitted) * dt

  # 4. Update Radiator Internal Temperature
  # deltaT = Joules / ThermalMass
  x$temp <- x$temp + (net_energy / x$thermal_mass)

  # 5. Record states for the simulation
  x$state$t_in <- t_in
  # The water leaves at the new radiator temperature
  x$state$t_out <- x$temp
  x$state$q_actual <- q_emitted

  return(x)
}
