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
      h4("Radiator Configuration"),
      sliderInput("n_elements", "Number of Sections:", 5, 50, 20),
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
                 )
        )
      )
    )
  )
)

server <- function(input, output) {

  sim_data <- reactive({
    # Simulation Settings
    dt <- 60
    steps <- (24 * 3600) / dt
    target_temp <- 21
    hysteresis <- 0.5

    # Initialize Objects
    rad <- new_cast_iron("TwinRad", 600, 140, input$n_elements)
    room <- new_room("TwinRoom", volume = 45, temp_initial = 18, u_value = input$u_value)

    results <- data.frame(
      time_hr = (1:steps * dt) / 3600,
      room_temp = numeric(steps),
      rad_temp = numeric(steps),
      return_temp = numeric(steps),
      outdoor_temp = numeric(steps),
      q_kw = numeric(steps),
      gas_kwh = numeric(steps)
    )

    boiler_on <- FALSE

    for(i in 1:steps) {
      # Sinusoidal outdoor temp
      t_out <- input$t_outdoor_min + 5 + 5 * cos(2 * pi * (i / steps) - (pi / 6))

      # Thermostat
      if (room$temp < (target_temp - hysteresis)) boiler_on <- TRUE
      if (room$temp > (target_temp + hysteresis)) boiler_on <- FALSE

      t_flow <- if(boiler_on) input$t_flow_max else room$temp

      # Update Physics (Using your LMTD/Return Temp function)
      rad <- simulate_output(rad, t_in = t_flow, t_room = room$temp, flow_rate = 0.05, dt = dt)
      room <- update_room(room, rad, temp_outdoor = t_out, dt = dt)

      # Efficiency Calculation (Condensing vs Non-condensing)
      eff <- if(rad$state$t_out < 54) 0.96 else 0.87
      q_water_kw <- (0.05 * 4186 * (t_flow - rad$state$t_out)) / 1000

      # Store
      results$room_temp[i] <- room$temp
      results$rad_temp[i] <- rad$temp
      results$return_temp[i] <- rad$state$t_out
      results$outdoor_temp[i] <- t_out
      results$q_kw[i] <- rad$state$q_actual / 1000
      results$gas_kwh[i] <- (q_water_kw * (dt / 3600)) / eff
    }
    results
  })

  output$tempPlot <- renderPlot({
    df <- sim_data() %>%
      select(time_hr, room_temp, rad_temp, outdoor_temp) %>%
      pivot_longer(-time_hr)

    ggplot(df, aes(x = time_hr, y = value, color = name)) +
      geom_line(size = 1.2) +
      theme_minimal() +
      labs(y = "Temperature (°C)", x = "Hour of Day", title = "24h Thermal Performance") +
      scale_color_manual(values = c("darkblue", "red", "darkgreen"))
  })

  output$returnPlot <- renderPlot({
    df <- sim_data()
    ggplot(df, aes(x = time_hr, y = return_temp)) +
      geom_line(color = "purple") +
      geom_hline(yintercept = 54, linetype = "dashed", color = "blue") +
      annotate("text", x = 2, y = 52, label = "Condensing Threshold", color = "blue") +
      theme_minimal() + labs(title = "Return Water Temp (T_out)")
  })

  output$metricsTable <- renderTable({
    df <- sim_data()
    total_gas <- sum(df$gas_kwh)
    data.frame(
      Metric = c("Total Gas Used (kWh)", "Total Cost", "Average Room Temp", "Condensing Efficiency"),
      Value = c(
        round(total_gas, 2),
        sprintf("£%.2f", total_gas * input$gas_price),
        round(mean(df$room_temp), 2),
        sprintf("%.1f%%", (sum(df$return_temp < 54 & df$return_temp > 25) / sum(df$return_temp > 25)) * 100)
      )
    )
  })
}

shinyApp(ui, server)
