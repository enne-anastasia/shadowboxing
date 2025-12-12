# Libraries ####

# library(readr)
# library(dplyr)
# library(text2speech)
# library(stringr)
# library(beepr)

# Loading the dataframe with combis ####

# combis <- readr::read_csv("combis.csv")

# Fixed parameters ####

# base_time_per_strike = 1 # in seconds
# pause_between_strikes = 0.2 # in seconds

# Variable parameters ####

# intencity = 1 # coefficient
# n_combos_per_round = 5
# n_rounds = 2

# Functions ####

# calculate_time <- function(strikes, base_time, intencity) {
#   total_time <- strikes * (base_time * intencity)
#   return(total_time)
# }
# 
# say <- function(text) {
#   print(text)
#   system(paste0("say '", text, "'"))
# }

# Main loop ####

for (i in seq(1, n_rounds, 1)) {
  round_combis = dplyr::sample_n(combis, n_combos_per_round)
  Sys.sleep(pause_between_strikes)
  for (j in seq(1, length(round_combis), 1)) {
    beepr::beep(sound = 1)
    combo = stringr::str_split(round_combis$combi[j], pattern = "-")[[1]]
    for (k in seq(1, length(combo), 1)) {
      say(combo[k])
      Sys.sleep(pause_between_strikes)
    }
    beepr::beep(sound = 1)
    pause = calculate_time(strikes = round_combis$length[j], 
                           base_time = base_time_per_strike, 
                           intencity = intencity)
    Sys.sleep(pause)
  }
}
