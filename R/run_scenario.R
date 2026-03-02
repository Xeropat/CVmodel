# Setup initial objects
my_volume = 6*7*3
my_room <- new_room("Living Room", volume = my_volume, temp_initial = 15, u_value = 30)
my_rad  <- new_cast_iron("Large Rad", height = 600, depth = 220, n_elements = 8)

# Simulation parameters
dt <- 600          # 10-minute time steps (in seconds)
hours <- 3
steps <- (hours * 3600) / dt
t_outdoor <- 0     # Constant freezing temp outside
t_water_in <- 60   # Constant boiler flow temp

# Data frame to store results
results <- data.frame(
  time_hr = (1:steps) * (dt/3600),
  room_temp = 0,
  heat_out = 0
)

# The Simulation Loop
for(i in 1:steps) {
  # 1. Update Radiator based on CURRENT room temp
  my_rad <- simulate_output(my_rad, t_in = t_water_in, t_room = my_room$temp, flow_rate = 0.05)

  # 2. Update Room based on Radiator output
  my_room <- update_room(my_room, my_rad, temp_outdoor = t_outdoor, dt = dt)

  # 3. Store results
  results$room_temp[i] <- my_room$temp
  results$heat_out[i]  <- my_rad$state$q_actual
}

par(mfrow = c(2,1))
plot(results$time_hr, results$room_temp, type="l", col="red",
     main="Room Temperature Over 24h", xlab="Hours", ylab="Temp (°C)")
abline(h = 20, lty = 2) # Target comfort line

plot(results$time_hr, results$heat_out, type="l", col="blue",
     main="Radiator Heat Output", xlab="Hours", ylab="Watts")
