#' Create a Room Object
#' @param name Character name
#' @param volume Volume in m3
#' @param temp_initial Initial air temperature (°C)
#' @param u_value overall heat loss coefficient (W/K)
#' @export
new_room <- function(name, volume, temp_initial = 18, u_value = 50) {
  # Air density approx 1.2 kg/m3, Specific heat approx 1005 J/kgK
  # Thermal mass = Vol * density * Cp
  thermal_mass <- volume * 1.2 * 1005

  structure(
    list(
      name = name,
      volume = volume,
      ua = u_value,
      temp = temp_initial,
      thermal_mass = thermal_mass
    ),
    class = "room"
  )
}
