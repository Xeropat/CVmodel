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

#' Calculate Heat Output
#' @param x A radiator object
#' @param t_in Inlet water temperature (°C)
#' @param t_room Ambient room temperature (°C)
#' @param flow_rate Water flow rate (kg/s or L/s)
#' @export
simulate_output <- function(x, t_in, t_room, flow_rate) {
  # Specific heat of water (approx 4186 J/kg·K)
  Cp <- 4186

  # Iterative or simplified approach:
  # For now, let's assume a 10°C drop to find an initial Q
  delta_t_initial <- (t_in - 5) - t_room
  q_guess <- x$p_nom * (delta_t_initial / 50)^x$n

  # Physics: Q = m_dot * Cp * (T_in - T_out)
  # Therefore: T_out = T_in - (Q / (m_dot * Cp))
  t_out <- t_in - (q_guess / (flow_rate * Cp))

  # Update the object state
  x$state$t_in <- t_in
  x$state$t_out <- round(t_out, 2)
  x$state$q_actual <- q_guess

  return(x)
}
