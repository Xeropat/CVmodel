#' Create a Cast Iron Radiator Object
#' @param name Character name
#' @param height Height in mm
#' @param depth Depth in mm
#' @param n_elements Number of sections/columns
#' @param n Radiator exponent (default 1.3 for cast iron)
#' @export
new_cast_iron <- function(name, height, depth, n_elements, n = 1.3) {

  # 1. Estimate nominal power (80/60/20 -> Delta T = 50)
  # This is a simplified regression for cast iron heat emission:
  # Power is proportional to height and number of elements,
  # with a scaling factor for depth.

  # Base factor: ~60W for a standard 600mm section.
  power_per_element <- (height / 600) * (depth / 100) * 85
  total_nominal_p <- power_per_element * n_elements

  structure(
    list(
      name = name,
      dims = list(h = height, d = depth, n = n_elements),
      p_nom = total_nominal_p, # This is our 80/60/20 baseline
      n = n,
      state = list(t_in = 0, t_out = 0, q_actual = 0)
    ),
    class = c("cast_iron", "radiator")
  )
}
