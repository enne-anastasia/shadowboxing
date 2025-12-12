#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Libraries ####

library(shiny)
library(readr)
library(dplyr)
library(text2speech)
library(stringr)
library(beepr)

# Loading the dataframe with combis ####

combis <- readr::read_csv("../combis.csv")

# Fixed parameters ####

base_time_per_strike = 1 # in seconds
pause_between_strikes = 0.2 # in seconds

# Functions ####

calculate_time <- function(strikes, base_time, intencity) {
  total_time <- strikes * (base_time * intencity)
  return(total_time)
}

say <- function(text) {
  print(text)
  system(paste0("say '", text, "'"))
}

# UI ####

ui <- shiny::fluidPage(
  # Application title
  shiny::titlePanel("Shadow boxing"),

  # Sidebar with a slider input for number of bins 
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput("intencity",
                         "Intencity:",
                         min = 0.1,
                         max = 4,
                         value = 1),
      shiny::numericInput("n_combos_per_round",
                          "Number of combos per round:",
                          min = 1,
                          max = 18,
                          step = 1,
                          value = 5),
      shiny::numericInput("n_rounds",
                          "Number of rounds:",
                          min = 1,
                          step = 1,
                          value = 2),
      shiny::actionButton("start_button",
                          "Start!",
                          class = "btn-success")
      ),
    
    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::textOutput("value")
      )
    )
  )

# Server ####

server <- function(input, output) {
  shiny::observeEvent(input$start_button, {
    output$value = shiny::renderText("Hey!")
  })
}

# Running the app ####

shiny::shinyApp(ui = ui, server = server)
