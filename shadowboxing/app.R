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

# Loading the combo database for the interface

combis = readr::read_csv("../combis.csv")

# UI ####

ui <- shiny::fluidPage(
  # Application title
  shiny::titlePanel("Shadow boxing"),

  # Sidebar with inputs
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # slider input for intensity that then will be used to calculate how much
      # time the app will wait after announcing a combination
      shiny::sliderInput("intensity",
                         "Intensity (seconds per combo):",
                         min = 0,
                         max = 30,
                         step = 1,
                         value = 15),
      # numeric input for number of combinations per round: minimum 1 combo,
      # maximum of total amount of combos in the library, default value 5
      shiny::numericInput("n_combos_per_round",
                          "Number of combos per round:",
                          min = 1,
                          step = 1,
                          value = 5),
      shiny::checkboxGroupInput("length_select",
                                "Combo length:",
                                choices = sort(unique(combis$length)),
                                selected = sort(unique(combis$length))),
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
      shiny::h3(paste0("Number of available combis: ")),
      shiny::span(shiny::textOutput("n_combis_total"), style="font-size:20px"),
      shiny::h3(paste0("Status:")),
      shiny::span(shiny::textOutput("status"), style="font-size:20px"),
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
  
  # start button handler
  shiny::observeEvent(input$start_button, {
    # change running status to true
    state$running = TRUE
    # sample combos for the current round
    round_combis(dplyr::sample_n(df(), input$n_combos_per_round))
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

# Archive ####

# # start button handler
# shiny::observeEvent(input$start_button, {
#   state$running = TRUE
#   state$paused = FALSE
#   state$round = 1
#   state$combo = 1
#   state$start_time = Sys.time()
#   intensity = input$intensity
#   n_combos_per_round = input$n_combos_per_round
#   n_rounds = input$n_rounds
#   round_combis(dplyr::sample_n(df(), n_combos_per_round))
# })
# # shiny::observeEvent(input$pause_button, {
# #   if (state$running) {
# #     state$paused = !state$paused
# #   }
# # })
# shiny::observeEvent(input$stop_button, {
#   state$running = FALSE
#   state$paused = FALSE
# })
# output$status = shiny::renderText({
#   if (state$paused) {
#     "Paused"
#   } else if (state$combo > 0 && state$running) {
#     paste0("Round ",state$round,"/",input$n_rounds," - Combo ",state$combo,
#            "/",input$n_combos_per_round)
#   } else {
#     "Press Start! to begin"
#   }
# })
# output$combo = shiny::renderText({
#   if (state$combo > 0 && state$running && state$combo <= input$n_combos_per_round) {
#     round_combis()$combi[state$combo]
#   } else {
#     ""
#   }
# })
# shiny::observe({
#   timer()
#   
#   if (state$running) {
#     shiny::isolate({
#       elapsed = as.numeric(difftime(Sys.time(), state$start_time, units = "secs"))
#       new_index = floor(elapsed / 3) + 1
#       if (new_index <= input$n_combos_per_round) {
#         state$combo = new_index
#       } else {
#         state$running = FALSE
#       }
#     })
#   }
#   # if (state$running && !state$paused) {
#   #   shiny::invalidateLater(shiny::isolate(
#   #     round_combis()$length[state$combo]*input$intensity*1000))
#   #   shiny::isolate({
#   #     if (state$combo < input$n_combos_per_round) {
#   #       state$combo = state$combo + 1
#   #     } else {
#   #       state$running = FALSE
#   #     }
#   #   })
#   # }
# })

# AI example ####

# library(shiny)
# 
# ui <- fluidPage(
#   titlePanel("Vector Element Display"),
#   
#   actionButton("start", "Start"),
#   actionButton("pause", "Pause"),
#   
#   h3("Current Element:"),
#   textOutput("current_element")
# )
# 
# server <- function(input, output, session) {
#   
#   # Your vector
#   my_vector <- c("Apple", "Banana", "Cherry", "Date", "Elderberry")
#   
#   # Reactive values to track state
#   values <- reactiveValues(
#     index = 0,
#     running = FALSE,
#     start_time = NULL,
#     paused_elapsed = 0  # Track elapsed time when paused
#   )
#   
#   # Timer that ticks every second
#   timer <- reactiveTimer(1000)
#   
#   # Start button handler
#   observeEvent(input$start, {
#     if (!values$running) {
#       if (values$index == 0) {
#         # Starting fresh
#         values$index <- 1
#         values$paused_elapsed <- 0
#       }
#       # Resume from where we paused
#       values$running <- TRUE
#       values$start_time <- Sys.time()
#     }
#   })
#   
#   # Pause button handler
#   observeEvent(input$pause, {
#     if (values$running) {
#       values$running <- FALSE
#       # Save the elapsed time when pausing
#       values$paused_elapsed <- values$paused_elapsed + 
#         as.numeric(difftime(Sys.time(), values$start_time, units = "secs"))
#     }
#   })
#   
#   # Update index based on elapsed time
#   observe({
#     timer()  # Depend on timer
#     
#     if (values$running) {
#       isolate({
#         current_elapsed <- as.numeric(difftime(Sys.time(), values$start_time, units = "secs"))
#         total_elapsed <- values$paused_elapsed + current_elapsed
#         new_index <- floor(total_elapsed / 3) + 1
#         
#         if (new_index <= length(my_vector)) {
#           values$index <- new_index
#         } else {
#           values$running <- FALSE
#         }
#       })
#     }
#   })
#   
#   # Display current element
#   output$current_element <- renderText({
#     if (values$index > 0 && values$index <= length(my_vector)) {
#       my_vector[values$index]
#     } else {
#       "Press Start to begin"
#     }
#   })
# }
# 
# shinyApp(ui = ui, server = server)