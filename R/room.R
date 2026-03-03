#' Create a Room Object
#' @param name Character name
#' @param volume Volume in m3
#' @param temp_initial Initial air temperature (°C)
#' @param u_value overall heat loss coefficient (W/K)
#' @export
new_room <- function(name, volume, temp_initial = 18, u_value = 30) {
  # Air Mass
  air_mass <- volume * 1.2 * 1005

  # Add a factor (e.g., 5x) to represent the thermal mass of
  # the actual contents/structure of the room.
  thermal_mass <- air_mass * 5

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

#' Advance Room Simulation
#' @param room A room object
#' @param radiator A radiator object
#' @param temp_outdoor Outdoor temperature (°C)
#' @param dt Time step in seconds (e.g., 60 for 1 minute)
#' @export
update_room <- function(room, radiator, temp_outdoor, dt = 60) {

  # 1. Heat Gain from Radiator (W = J/s)
  q_gain <- radiator$state$q_actual

  # 2. Heat Loss to outside (Q = UA * deltaT)
  q_loss <- room$ua * (room$temp - temp_outdoor)

  # 3. Net Energy Change (Joules)
  net_energy <- (q_gain - q_loss) * dt

  # 4. Temperature Change (deltaT = Q / ThermalMass)
  delta_temp <- net_energy / room$thermal_mass

  room$temp <- room$temp + delta_temp
  return(room)
}

#' Update Room Temperature from Multiple Heat Sources
#' @param room Room object
#' @param q_total Total Watts from all radiators
#' @param temp_outdoor Outdoor temp (°C)
#' @param dt Time step (seconds)
#' @export
update_room_multi <- function(room, q_total, temp_outdoor, dt = 60) {
  # Heat loss to outside (W)
  q_loss <- room$ua * (room$temp - temp_outdoor)

  # Net energy change (Joules)
  net_energy <- (q_total - q_loss) * dt

  # Update room temp
  room$temp <- room$temp + (net_energy / room$thermal_mass)

  return(room)
}
