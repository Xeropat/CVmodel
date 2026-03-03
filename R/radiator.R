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

#' Simulate Radiator with LMTD and Return Temp
#' @param x Radiator object
#' @param t_in Flow temperature from boiler (°C)
#' @param t_room Room temperature (°C)
#' @param flow_rate Water flow in kg/s (default 0.05 is ~3 L/min)
#' @param dt Time step in seconds
#' @export
simulate_output <- function(x, t_in, t_room, flow_rate, dt = 60) {
  cp_water <- 4186

  # 1. Calculate Heat Emission (Q_out)
  # Based on current radiator surface temperature
  q_emitted <- x$p_nom * ((x$temp - t_room) / 50)^x$n

  # 2. Calculate Heat Gained from Water (Q_in)
  # If the pump is off (flow_rate = 0), no heat enters
  if (flow_rate > 0) {
    # We use an exponential decay model: the water temperature
    # approaches the radiator temperature as it flows through.
    # NTU (Number of Transfer Units) simplified for a radiator:
    ua_effective <- x$p_nom / (50^x$n) # Simplified UA constant
    ntu <- ua_effective / (flow_rate * cp_water)

    # Return temperature (T_out) calculation
    t_out <- x$temp + (t_in - x$temp) * exp(-ntu)
    q_from_water <- flow_rate * cp_water * (t_in - t_out)
  } else {
    t_out <- x$temp
    q_from_water <- 0
  }

  # 3. Update Thermal Mass (Energy Balance)
  net_energy <- (q_from_water - q_emitted) * dt
  x$temp <- x$temp + (net_energy / x$thermal_mass)

  # 4. Store State
  x$state$t_in <- t_in
  x$state$t_out <- t_out   # This is your Return Temperature!
  x$state$q_actual <- q_emitted

  return(x)
}
