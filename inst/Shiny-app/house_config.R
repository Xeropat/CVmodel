# house_config.R

# Define the Room
ROOM_CONFIG <- list(
  name = "Living",
  volume = 6*7*3,      # m3
  u_value = 28,     # W/K (Loss rate)
  target_temp = 21,
  hysteresis = 0.5
)

# Define the Radiator Inventory
# This allows for varied shapes and sizes
MY_RADIATORS <- list(
  list(name = "Kitchen Wall", height = 720, depth = 220, sections = 8),
  list(name = "Garden Wall E",   height = 720, depth = 220,  sections = 13),
  list(name = "Garden Wall W",   height = 720, depth = 220, sections = 13)
)

# System Constants
PUMP_FLOW_KG_S <- 0.05  # Total system flow
