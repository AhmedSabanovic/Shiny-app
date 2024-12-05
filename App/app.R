library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)
library(jsonlite)
library(countrycode)
library(shinyalert)
library(maps)

# Load the data
data_cia <- fromJSON("data_cia.json")


# Rename columns 
data_cia <- data_cia %>%
  rename(
    iso3 = ISO3, 
    median_age = median_age, 
    youth_unemployment_rate = youth_unempl_rate,
    net_migration_rate = net_migr_rate, 
    electricity_fossil_fuel = electricity_fossil_fuel, 
    area = area, 
    population_growth_rate = pop_growth_rate, 
    life_expectancy = life_expectancy
  )
world_map <- map_data("world")
world_map$ISO3 <- countrycode::countrycode(sourcevar = world_map$region,
                                           origin = "country.name",
                                           destination = "iso3c", nomatch = NA)
# UI
ui <- fluidPage(
  titlePanel("CIA Factbook Data Analysis"),
  useShinyalert(),
  tabsetPanel(
    tabPanel("Univariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Select Variable:",
                             choices = list(
                               "Median Age" = "median_age",
                               "Youth Unemployment Rate" = "youth_unemployment_rate",
                               "Net Migration Rate" = "net_migration_rate",
                               "Electricity from Fossil Fuels" = "electricity_fossil_fuel",
                               "Population Growth Rate" = "population_growth_rate",
                               "Life Expectancy" = "life_expectancy"
                             )),
                 actionButton("viewData", "View Raw Data"),
                 DTOutput("data_table")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", plotlyOutput("map_plot")),
                   tabPanel("Boxplot (Overall)", plotlyOutput("boxplot_overall")),
                   tabPanel("Boxplot per Continent", plotlyOutput("boxplot_continent"))
                 )
               )
             )
    ),
    tabPanel("Multivariate Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_variable", "Select X Variable:",
                             choices = list(
                               "Median Age" = "median_age",
                               "Youth Unemployment Rate" = "youth_unemployment_rate",
                               "Net Migration Rate" = "net_migration_rate",
                               "Electricity from Fossil Fuels" = "electricity_fossil_fuel",
                               "Population Growth Rate" = "population_growth_rate",
                               "Life Expectancy" = "life_expectancy"
                             )),
                 selectInput("y_variable", "Select Y Variable:",
                             choices = list(
                               "Median Age" = "median_age",
                               "Youth Unemployment Rate" = "youth_unemployment_rate",
                               "Net Migration Rate" = "net_migration_rate",
                               "Electricity from Fossil Fuels" = "electricity_fossil_fuel",
                               "Population Growth Rate" = "population_growth_rate",
                               "Life Expectancy" = "life_expectancy"
                             )),
                 selectInput("size_variable", "Select Size Variable:",
                             choices = list(
                               "Population" = "population",
                               "Area" = "area"
                             ))
               ),
               mainPanel(
                 tabPanel("Multivariate Scatterplot", plotlyOutput("plot_mult"))
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  shinyalert("Welcome", "Welcome to the Group 67 Dashboard! This app allows you to explore the 2020 CIA Factbook data through univariate and multivariate analysis.", type = "info")
  observeEvent(input$viewData, {
    output$data_table <- renderDT({
      req(input$variable)
      data_cia %>%
        select(country, continent, !!input$variable) %>%
        datatable(options = list(pageLength = 15))
    })
  })
  
  output$map_plot <- renderPlotly({
    req(input$variable)
    world_map <- world_map %>%
      left_join(data_cia, by = c("ISO3" = "iso3")) %>%
      filter(!is.na(ISO3))
    
    p <- ggplot(world_map, aes(x = long, y = lat, group = group, fill = !!sym(input$variable))) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c() +
      labs(fill = input$variable)
    
    ggplotly(p, tooltip = c("region", input$variable))
  })
  
  output$boxplot_overall <- renderPlotly({
    req(input$variable)
    p <- ggplot(data_cia, aes(y = !!sym(input$variable))) +
      geom_boxplot() +
      labs(y = input$variable)
    ggplotly(p)
  })
  
  output$boxplot_continent <- renderPlotly({
    req(input$variable)
    p <- ggplot(data_cia, aes(x = continent, y = !!sym(input$variable))) +
      geom_boxplot() +
      labs(x = "Continent", y = input$variable)
    ggplotly(p)
  })
  
  # Mulitvariate
  output$plot_mult <- renderPlotly({
    req(input$x_variable, input$y_variable, input$size_variable)
    p <- ggplot(data_cia, aes(x = !!sym(input$x_variable), y = !!sym(input$y_variable), 
                              size = !!sym(input$size_variable), color = continent)) +
      geom_point(alpha = 0.7) +
      labs(x = input$x_variable, y = input$y_variable, size = input$size_variable, color = "Continent")
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
