# Libraries ####

library(readr)
library(dplyr)
library(text2speech)
library(stringr)
library(beepr)

# Loading the dataframe with combis ####

combis <- readr::read_csv("combis.csv")
combis = combis %>% dplyr::filter(length == 5)

# Fixed parameters ####

pause_between_strikes = 0.2 # in seconds

# Variable parameters ####

intencity = 4 # seconds per strike
n_combos_per_round = nrow(combis)
n_rounds = 1

# Functions ####

calculate_time <- function(strikes, intencity) {
  total_time <- strikes * intencity
  return(total_time)
}

say <- function(text) {
  print(text)
  system(paste0("say '", text, "'"))
}

# Main loop ####

for (i in seq(1, n_rounds, 1)) {
  round_combis = dplyr::sample_n(combis, n_combos_per_round)
  Sys.sleep(pause_between_strikes)
  for (j in seq(1, n_combos_per_round, 1)) {
    beepr::beep(sound = 1)
    combo = stringr::str_split(round_combis$combi[j], pattern = "-")[[1]]
    for (k in seq(1, length(combo), 1)) {
      say(combo[k])
      Sys.sleep(pause_between_strikes)
    }
    beepr::beep(sound = 1)
    pause = calculate_time(strikes = round_combis$length[j], 
                           intencity = intencity)
    Sys.sleep(pause)
  }
}
