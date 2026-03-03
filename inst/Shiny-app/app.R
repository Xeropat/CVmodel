library(shiny)
library(CVmodel)
library(ggplot2)
library(tidyr)
library(dplyr)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel("CVmodel: Heating Digital Twin"),

  sidebarLayout(
    sidebarPanel(
      h4("System Profile"),
      wellPanel(
        p(strong("Room:"), ROOM_CONFIG$name),
        p(strong("Radiators:"), length(MY_RADIATORS)),
        p(strong("Total Sections:"), sum(sapply(MY_RADIATORS, function(x) x$sections)))
      ),

      sliderInput("t_flow_max", "Max Flow Temp (°C):", 35, 75, 65),

      hr(),
      h4("House & Environment"),
      sliderInput("u_value", "Heat Loss (U-Value):", 5, 100, 25),
      sliderInput("t_outdoor_min", "Coldest Outdoor Temp (°C):", -10, 15, 2),

      hr(),
      h4("Economics"),
      numericInput("gas_price", "Gas Price (£/kWh):", 0.10, step = 0.01)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Temperatures", plotOutput("tempPlot", height = "500px")),
        tabPanel("Efficiency & Cost",
                 fluidRow(
                   column(6, plotOutput("returnPlot")),
                   column(6, h3("Summary Metrics"), tableOutput("metricsTable"))
                 )),
          tabPanel("Radiator Load", tableOutput("radTable"))
        )
      )
    )
  )

server <- function(input, output) {

  sim_data <- reactive({
    dt <- 60
    steps <- (24 * 3600) / dt
    target_temp <- 21
    hysteresis <- 0.5

    source("house_config.R")

    rads <- lapply(MY_RADIATORS, function(r) {
      new_cast_iron(r$name, r$height, r$depth, r$sections)
    })

    room <- new_room(ROOM_CONFIG$name, ROOM_CONFIG$volume, u_value = input$u_value)

    # STORAGE: These names MUST match the ggplot calls exactly
    results <- data.frame(
      time_hr = (1:steps * dt) / 3600,
      room_temp = numeric(steps),
      rad_temp = numeric(steps),      # The average temp of the iron
      return_temp = numeric(steps),   # The water coming back to boiler
      outdoor_temp = numeric(steps),
      gas_kwh = numeric(steps)
    )

    boiler_on <- FALSE

    for(i in 1:steps) {
      t_out <- input$t_outdoor_min + 5 + 5 * cos(2 * pi * (i/steps) - (pi/6))

      if (room$temp < (target_temp - hysteresis)) boiler_on <- TRUE
      if (room$temp > (target_temp + hysteresis)) boiler_on <- FALSE

      t_flow <- if(boiler_on) input$t_flow_max else room$temp

      current_total_q <- 0
      current_returns <- c()
      current_rad_temps <- c()

      for(j in seq_along(rads)) {
        rads[[j]] <- simulate_output(
          rads[[j]], t_in = t_flow, t_room = room$temp,
          flow_rate = 0.05 / length(rads), dt = dt
        )
        current_total_q <- current_total_q + rads[[j]]$state$q_actual
        current_returns <- c(current_returns, rads[[j]]$state$t_out)
        current_rad_temps <- c(current_rad_temps, rads[[j]]$temp)
      }

      room <- update_room_multi(room, current_total_q, temp_outdoor = t_out, dt = dt)

      # --- Gas & Efficiency Logic ---
      avg_return <- mean(current_returns)
      # Calculate gas: q_water (W) = flow * cp * deltaT
      q_water_w <- 0.05 * 4186 * (t_flow - avg_return)
      eff <- if(avg_return < 54) 0.96 else 0.87

      results$room_temp[i] <- room$temp
      results$rad_temp[i]  <- mean(current_rad_temps)
      results$return_temp[i] <- avg_return
      results$outdoor_temp[i] <- t_out
      results$gas_kwh[i] <- ((q_water_w * (dt/3600)) / 1000) / eff
    }
    results
  })

  output$tempPlot <- renderPlot({
    # Select exact names from the results DF
    df <- sim_data() %>%
      select(time_hr, room_temp, rad_temp, outdoor_temp, return_temp) %>%
      pivot_longer(-time_hr)

    ggplot(df, aes(x = time_hr, y = value, color = name)) +
      geom_line(linewidth = 1.2) + # 'size' is deprecated, use 'linewidth'
      scale_color_manual(values = c("outdoor_temp" = "blue", "rad_temp" = "red", "room_temp" = "darkgreen")) +
      theme_minimal() +
      labs(y = "Temperature (°C)", x = "Hour of Day", title = "24h Thermal Performance")
  })

  output$returnPlot <- renderPlot({
    ggplot(sim_data(), aes(x = time_hr, y = return_temp)) +
      geom_line(color = "purple", linewidth = 1) +
      geom_hline(yintercept = 54, linetype = "dashed", color = "blue") +
      annotate("text", x = 2, y = 52, label = "Condensing Threshold", color = "blue") +
      theme_minimal() +
      labs(title = "System Return Water Temp", y = "Temp (°C)")
  })

  output$metricsTable <- renderTable({
    df <- sim_data()
    total_gas <- sum(df$gas_kwh, na.rm = TRUE)
    # Filter for times when boiler was actually dumping heat to avoid 0% div by 0
    heating_cycles <- df[df$return_temp > (df$room_temp + 1), ]
    condensing_rate <- if(nrow(heating_cycles) > 0) {
      sum(heating_cycles$return_temp < 54) / nrow(heating_cycles)
    } else { 0 }

    data.frame(
      Metric = c("Total Gas Used (kWh)", "Total Cost", "Average Room Temp", "Condensing Efficiency"),
      Value = c(
        round(total_gas, 2),
        sprintf("£%.2f", total_gas * input$gas_price),
        round(mean(df$room_temp), 2),
        sprintf("%.1f%%", condensing_rate * 100)
      )
    )
  })

  output$radTable <- renderTable({
    # Pull the LAST state of the radiators from the simulation
    # To show their peak performance during the 24h period
    data.frame(
      Location = sapply(MY_RADIATORS, function(x) x$name),
      Sections = sapply(MY_RADIATORS, function(x) x$sections),
      Nominal_Power_W = sapply(MY_RADIATORS, function(r) {
        # Re-calculate or pull from simulation objects
        new_cast_iron(r$name, r$height, r$depth, r$sections)$p_nom
      })
    )
  })
}

shinyApp(ui, server)
