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
library(stringr)
library(beepr)

# Loading the combo database for the interface

combis = readr::read_csv("../combis.csv")

# UI ####

ui <- shiny::fluidPage(
  # Application title
  shiny::titlePanel("Shadow boxing"),

  # Sidebar with inputs
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::span("Number of available combis:", style = "font-weight:bold;"),
      shiny::span(shiny::textOutput("n_combis_total")),
      shiny::hr(),
      shiny::checkboxGroupInput("length_select",
                                "Combo length:",
                                choices = sort(unique(combis$length)),
                                selected = sort(unique(combis$length))),
      # numeric input for number of combinations per round: minimum 1 combo,
      # maximum of total amount of combos in the library, default value 5
      shiny::numericInput("n_combos_per_round",
                          "Number of combos per round:",
                          min = 1,
                          step = 1,
                          value = 5),
      # slider input for intensity that then will be used to calculate how much
      # time the app will wait after announcing a combination
      shiny::sliderInput("intensity",
                         "Intensity (seconds per combo):",
                         min = 0,
                         max = 60,
                         step = 5,
                         value = 15),
      shiny::hr(),
      shiny::span("Total time:", style = "font-weight:bold;"),
      shiny::span(shiny::textOutput("total_time")),
      shiny::hr(),
      # button to start rounds with chosen intensity, number of combos per
      # round, and number of rounds
      shiny::actionButton("start_button",
                          "Start!",
                          class = "btn-success"),
      # button to stop training
      shiny::actionButton("stop_button",
                          "Stop",
                          class = "btn-danger")
      ),

    # Show a plot of the generated distribution
    shiny::mainPanel(
      shiny::h3(paste0("Status:")),
      shiny::span(shiny::textOutput("status"), style = "font-size:20px"),
      shiny::h3(paste0("Current combo:")),
      shiny::span(shiny::textOutput("combo"), style="font-size:80px")
      )
    )
  )

# Server ####

server <- function(input, output) {
  # reactive dataframe with combi database
  df = shiny::reactive(
    combis %>% dplyr::filter(length %in% input$length_select)
  )
  # reactive dataframe with combis of the current round
  round_combis = shiny::reactiveVal(
    data.frame()
  )
  # state variables
  state = shiny::reactiveValues(
    running = FALSE,
    combo = 0,
    last_combo = 0,
    start_time = NULL
  )
  # timer that ticks every 0.5 seconds
  timer = shiny::reactiveTimer(500)
  
  # output how many combis there are in the database
  output$n_combis_total = shiny::renderText(nrow(df()))
  
  output$total_time = shiny::renderText({
    if (!is.na(input$n_combos_per_round)) {
      paste0(
        round(input$intensity*input$n_combos_per_round/60, 2)," minutes")
    } else {
      "0 minutes"
    }
  })
  
  # start button handler
  shiny::observeEvent(input$start_button, {
    # change running status to true
    state$running = TRUE
    # sample combos for the current round
    if (nrow(df()) < input$n_combos_per_round) {
      round_combis(dplyr::sample_n(df(), input$n_combos_per_round, replace = TRUE))
    } else {
      round_combis(dplyr::sample_n(df(), input$n_combos_per_round, replace = FALSE))
    }
    # show the first combo
    state$combo = 1
    state$last_combo = 0
    # note the start time
    state$start_time = Sys.time()
  })
  
  # stop button handler
  shiny::observeEvent(input$stop_button, {
    state$running = FALSE
  })
  
  # output app status
  output$status = shiny::renderText({
    # if the app is running and we have a combo to show
    if (state$running) {
      if (state$combo>0 && 
          state$combo<=input$n_combos_per_round) {
        paste0("Combo ",state$combo,
               "/",input$n_combos_per_round)
      }
    } else {
      "Press Start! to start"
    }
  })
  
  # output combo
  output$combo = shiny::renderText({
    # if the app is running and we have a combo to show
    if (state$running) {
      if (state$combo>0 && 
          state$combo<=input$n_combos_per_round) {
        round_combis()$combi[state$combo]
      }
    } else {
      ""
    }
  })
  
  # main observer
  shiny::observe({
    # depend on timer
    timer()
    # if the app is running
    if (state$running) {
      shiny::isolate({
        # calculate how much time has passed since the training has started
        elapsed = as.numeric(difftime(Sys.time(), state$start_time, 
                                              units = "secs"))
        # calculate new combo index based on how much time has passed divided
        # by intensity
        new_combo_index = floor(elapsed / input$intensity) + 1
        
        # if we are still within the given number of combos
        if (new_combo_index <= input$n_combos_per_round) {
          # if index has changed
          if (new_combo_index != state$last_combo) {
            beepr::beep(sound = 2)
            state$last_combo = new_combo_index
          }
          # update the index
          state$combo = new_combo_index
        } else {
          # stop the app
          state$running = FALSE
        }
      })
    }
  })
}

# Running the app ####

shiny::shinyApp(ui = ui, server = server)