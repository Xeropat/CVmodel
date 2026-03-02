# --- 1. Setup Environment ---
library(CVmodel)

dt <- 60                # 1-minute steps
hours <- 24
steps <- (hours * 3600) / dt
target_temp <- 21
hysteresis <- 0.5

# --- 2. Initialize Objects ---
# Using your specific parameters: 600mm height, 140mm depth, 20 elements
my_rad <- new_cast_iron(
  name = "Victorian_Rad",
  height = 600,
  depth = 140,
  n_elements = 20,
  n = 1.3
)

# A room that matches this radiator's size (roughly 40-50 m3)
my_room <- new_room("LivingRoom", volume = 45, temp_initial = 18, u_value = 25)

# Storage
results <- data.frame(
  step = 1:steps,
  time_hr = (1:steps * dt) / 3600,
  room_temp = numeric(steps),
  rad_temp = numeric(steps),
  boiler_on = logical(steps)
)

# --- 3. Run Simulation ---
boiler_active <- FALSE

for(i in 1:steps) {
  # A. Simple Outdoor Profile (Colder at night)
  t_out <- 5 - 5 * cos(2 * pi * (i / steps) - pi/2)

  # B. Thermostat Logic (Bang-Bang)
  if (my_room$temp < (target_temp - hysteresis)) {
    boiler_active <- TRUE
  } else if (my_room$temp > (target_temp + hysteresis)) {
    boiler_active <- FALSE
  }

  # C. Boiler Water Temp (65C if on, or cooling to room temp if off)
  t_flow <- if(boiler_active) 65 else my_room$temp

  # D. Update Radiator (Physics)
  my_rad <- simulate_output(my_rad, t_in = t_flow, t_room = my_room$temp,
                            flow_rate = 0.05, dt = dt)

  # E. Update Room (Physics)
  my_room <- update_room(my_room, my_rad, temp_outdoor = t_out, dt = dt)

  # F. Save State
  results$room_temp[i] <- my_room$temp
  results$rad_temp[i] <- my_rad$temp
  results$boiler_on[i] <- boiler_active
  results$outdoor_temp[i] <- t_out
}


library(ggplot2)

ggplot(results, aes(x = time_hr)) +
  # Draw a shaded area when the boiler is ON
  geom_rect(aes(xmin = time_hr, xmax = time_hr + (dt/3600),
                ymin = 15, ymax = 25, fill = boiler_on), alpha = 0.1) +
  geom_line(aes(y = room_temp, color = "Room Temperature"), size = 1) +
  geom_hline(yintercept = 21, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("transparent", "orange"), name = "Boiler Status") +
  labs(title = "24-Hour Cast Iron Heating Profile",
       y = "Temperature (°C)", x = "Time (Hours)") +
  theme_minimal()
