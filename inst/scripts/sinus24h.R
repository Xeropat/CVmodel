# --- 1. Setup ---
library(CVmodel)
library(ggplot2)
library(tidyverse)

dt <- 60                # 1-minute steps
steps <- (24 * 3600) / dt
target_temp <- 21
hysteresis <- 0.5

# --- 2. Objects ---
# Your 20-element Victorian radiator (2380W)
my_rad <- new_cast_iron("LK", 600, 220, 8)
my_vol = 3*6*6
my_room <- new_room("LivingRoom", volume = my_vol, temp_initial = 18, u_value = 25)

# Storage
results <- data.frame(
  time_hr = (1:steps * dt) / 3600,
  room_temp = numeric(steps),
  rad_temp = numeric(steps),
  outdoor_temp = numeric(steps),
  boiler_on = logical(steps)
)

# --- 3. The Loop ---
boiler_active <- FALSE

for(i in 1:steps) {
  # Outdoor temp: Min 2°C at night, Max 10°C in day
  t_out <- 6 - 4 * cos(2 * pi * (i / steps) - (pi / 6))

  # Thermostat Logic
  if (my_room$temp < (target_temp - hysteresis)) boiler_active <- TRUE
  if (my_room$temp > (target_temp + hysteresis)) boiler_active <- FALSE

  # Boiler Flow (65°C if on, or room temp if off)
  t_flow <- if(boiler_active) 45 else my_room$temp

  # Update Physics
  my_rad <- simulate_output(my_rad, t_in = t_flow, t_room = my_room$temp, flow_rate = 0.05, dt = dt)
  my_room <- update_room(my_room, my_rad, temp_outdoor = t_out, dt = dt)

  # Store
  results$room_temp[i] <- my_room$temp
  results$rad_temp[i] <- my_rad$temp
  results$outdoor_temp[i] <- t_out
  results$boiler_on[i] <- boiler_active
}

# Pivot for ggplot
plot_data <- results %>%
  pivot_longer(cols = c(room_temp, rad_temp, outdoor_temp),
               names_to = "measure", values_to = "value")

ggplot(plot_data, aes(x = time_hr, y = value, color = measure)) +
  # Shaded areas for when the boiler is running
  geom_rect(data = results[results$boiler_on, ],
            aes(xmin = time_hr, xmax = time_hr + (dt/3600), ymin = -Inf, ymax = Inf),
            fill = "orange", alpha = 0.05, inherit.aes = FALSE) +
  geom_line(size = 1) +
  geom_hline(yintercept = 21, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("outdoor_temp" = "blue", "rad_temp" = "red", "room_temp" = "darkgreen")) +
  labs(title = "24-Hour CVmodel Simulation: Cast Iron Performance",
       subtitle = "Orange bars indicate Boiler Active periods",
       x = "Time (Hours)", y = "Temperature (°C)") +
  theme_minimal()
