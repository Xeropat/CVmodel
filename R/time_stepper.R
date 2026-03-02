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
