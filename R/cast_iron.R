#' Create a Cast Iron Radiator Object
#' @param name Character name
#' @param height Height in mm
#' @param depth Depth in mm
#' @param n_elements Number of sections
#' @param n Delta T exponent (usually 1.3)
#' @export
new_cast_iron <- function(name, height, depth, n_elements, n = 1.3) {

  # 1. Estimate Nominal Power (P_nom)
  # Rough estimate: a 600x140mm element produces ~100W at DeltaT 50
  p_nom <- n_elements * (height / 600) * (depth / 140) * 119

  # 2. Calculate Thermal Mass (The "Flywheel")
  # Iron mass: ~6.5kg per element * 460 J/kgK
  # Water mass: ~1.0kg per element * 4186 J/kgK
  mass_iron <- n_elements * 6.5
  mass_water <- n_elements * 1.0
  thermal_mass_value <- (mass_iron * 460) + (mass_water * 4186)

  structure(
    list(
      name = name,
      height = height,
      depth = depth,
      n_elements = n_elements,
      p_nom = p_nom,
      n = n,
      temp = 20,                # Initial internal temperature
      thermal_mass = thermal_mass_value, # <--- Crucial for simulate_output
      state = list(
        t_in = 20,
        t_out = 20,
        q_actual = 0
      )
    ),
    class = c("cast_iron","radiator")
  )
}
