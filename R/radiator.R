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
  cp <- 4186

  # Target: Find T_out such that Q_emitted == Q_water_loss
  # For a simple version, we can use an iterative 'shrink'
  # or solve the energy balance equation directly.

  # Simplified iterative step for demonstration:
  t_out <- t_in - 10 # Start with a 10 degree drop guess

  for(i in 1:5) {
    t_avg <- (t_in + t_out) / 2
    q_emitted <- x$p_nom * ((t_avg - t_room) / 50)^x$n
    t_out <- t_in - (q_emitted / (flow_rate * cp))
  }

  x$state$t_in <- t_in
  x$state$t_out <- t_out
  x$state$q_actual <- q_emitted
  return(x)
}
