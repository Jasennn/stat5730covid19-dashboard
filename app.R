library(shiny)
library(stat5730covid19)
library(maps)
library(tidyverse)
library(RcppRoll)

# States in the data and the last date available
states <- nyt_county %>% count(state) %>% pull(state)
max_county_date <- max(nyt_county$date)

# This function takes a state as input and returns a data frame containing 
# the boundaries of counties,and the COVID-19 deaths for each county, and 
# a U.S. Census population estimate
get_state_deaths_map_data <- function(selected_state) {
  # Get state county boundaries. Annoyingly, maps uses lower case state
  state_map <- map_data("county", region = tolower(selected_state)) %>%
    mutate(polyname = paste(region, subregion, sep = ",")) %>%
    left_join(maps::county.fips, by = "polyname") %>%
    # Convert FIPS code from integer to 5 digit string
    mutate(fips = sprintf("%05d", fips)) %>%
    select(long, lat, group, fips)

  # Get deaths from NYT
  state_deaths <- nyt_county %>%
    filter(state == selected_state, date == max_date) %>%
    select(fips, state, county, deaths)
  
  # Get population estimates of Census
  state_pop <- census_county_pop %>%
    filter(state == selected_state) %>%
    select(fips = GEOID, POP)

  # Join map, deaths, population using FIPS code as key
  state_map %>%
    left_join(state_deaths, by = "fips") %>%
    left_join(state_pop, by = "fips") %>%
    mutate(deaths = replace_na(deaths, 0))
}

plot_deaths_map <- function(df, per_capita = TRUE) {
  if (per_capita) {
    df <- mutate(df, deaths = 1e6 * deaths / POP)
    ylab <- "Deaths per 1M capita"
  } else {
    ylab <- "Deaths"      
  }
  
  ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = deaths), 
                 color = "black", 
                 data = df) +
    scale_fill_gradient(name = ylab, low = "white", high = scales::muted("red")) + 
    coord_quickmap() + theme_void() + theme(legend.position = "bottom") + 
    labs(title = paste("Cumulative deaths, current as of", max_county_date))
}

# This function takes a state as input and returns a data frame containing 
# dates and total deaths in the state from COVID Tracking Project and population
get_state_death_increase_line_data <- function(selected_state) {
  # Get deaths from COVID Tracking Project
  state_deaths <- covidtracking %>%
    filter(state == selected_state) %>%
    select(date, state, death_increase, fips)  %>%
    arrange(date)
    
  # Get population estimates of Census
  state_pop <- census_state_pop %>%
    filter(state == state) %>%
    select(fips = GEOID, POP)
  
  # Join deaths and population using FIPS code as key
  state_deaths %>%
    left_join(state_pop, by = "fips")
}

plot_death_increase_line <- function(df, per_capita = TRUE, log_scale = TRUE, comparison = TRUE) {
  if (per_capita) {
    df <- mutate(df, death_increase = 1e6 * death_increase / POP)
    ylab <- "Increase in deaths per 1M capita"
  } else {
    ylab <- "Increase in deaths"
  }

  # Add 7 day moving average
  df <- mutate(df, average = roll_mean(death_increase, n = 7, align = "right", fill = NA))

  # Base plot
  p <- ggplot()

  if (comparison) {
    # Add background layer of 7-day averages for other states
    df_comparison <- covidtracking %>% 
      arrange(date) %>%
      group_by(fips) %>%
      mutate(average = roll_mean(death_increase, n = 7, align = "right", fill = NA))
  
    p <- p +
      geom_line(aes(date, average, group = fips), 
                alpha = 1/4, 
                data = df_comparison) +
      geom_line(aes(date, death_increase), size = 2, color = "white", 
              data = df) +
      geom_line(aes(date, death_increase), size = 1, 
                data = df)
  } else {
    # Use columns if no comparison
    p <- p + geom_col(aes(date, death_increase), data = df)
  }

  # Layer 7-day average.
  p <- p +
    geom_line(aes(date, average, color = "7 day average"), 
              size = 3/2, alpha = 1/2, 
              data = df) +
    scale_color_manual(name = NULL, values = c("7 day average" = "red")) +
    labs(y = ylab, title = "Daily new deaths") + 
    theme_minimal() + theme(legend.position = "bottom")

  # Add a log-scale to the plot
  if (log_scale) {
    p <- p + 
      scale_y_log10(labels = scales::label_number_si()) + 
      coord_cartesian(ylim = c(1, NA)) # crop y-axis to prevent log(0)
  }
  
  # Return the plot
  p
}

# This function constructs the state executive orders table
table_state_orders <- function(selected_state) {
  state_orders %>%
    filter(state == selected_state) %>%
    select(date, order = order_type) %>%
    arrange(date) %>%
    # Print the date as a string
    mutate(date = format(date, format = "%m/%d/%y")) %>%
    # Drop rows with missing values
    drop_na()
}

ui <- fluidPage(
  titlePanel(str_glue("STAT5730: COVID-19 deaths")),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_state", "Which state/region?", states, selected = "Ohio"), 
      radioButtons("map_per_capita", "Map:", 
                   c("Deaths per 1M capita" = TRUE, "Deaths" = FALSE)),
      hr(), 
      radioButtons("log_scale", "Line chart:", 
                   c("log scale" = TRUE, "linear scale" = FALSE)),
      hr(), 
      radioButtons("line_per_capita", NULL,  
                   c("Increase per 1M capita" = TRUE, "Increase" = FALSE), 
                   selected = FALSE),
      hr(), 
      radioButtons("line_background", "Comparison:",
                   c("other states 7-day average" = TRUE, "none" = FALSE),
                   selected = TRUE),
      h3("Executive orders"),
      tableOutput("orders_table")
    ),
    mainPanel(
      plotOutput("map_plot"),
      plotOutput("line_plot")
    )
  )
)

server <- function(input, output, session) {
  # Reactive for producing map data for selected state
  state_deaths_map_data <- reactive({
    get_state_deaths_map_data(input$selected_state)
  })

  # Reactive for produce line chart data for selected state
  state_death_increase_line_data <- reactive({
    get_state_death_increase_line_data(input$selected_state)
  })

  # Render the map
  output$map_plot <- renderPlot({
    plot_deaths_map(state_deaths_map_data(), input$map_per_capita)
  })

  # Render the line chart
  output$line_plot <- renderPlot({
    plot_death_increase_line(state_death_increase_line_data(), 
                     input$line_per_capita, input$log_scale, input$line_background)
  })

  # Render the table of executive orders
  output$orders_table <- renderTable({
    table_state_orders(input$selected_state)
  }, hover = TRUE)
}

shinyApp(ui, server)
