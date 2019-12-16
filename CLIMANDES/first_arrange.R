library(tidyverse)
library(purrr)
dir.create("to_submit")
source("functions.R")
data_dir <- "DECADE_database_sh/DECADE_detabase/DECADE_data_final_ACMANT/"
variables <- c("PRCP", "TN", "TX")

# 1. Precipitation --------------------------------------------------------
pp_list <- sprintf("%s/%s/", data_dir, variables[1]) %>%
  list.files(
    pattern = "\\.dat", full.names = TRUE,
    all.files = TRUE
  )

# Create metadata
PP_metadata <- pp_list %>%
  map(~ create_metadata(.x)) %>%
  do.call(bind_rows, .) %>%
  mutate_at(
    .vars = vars(Latitude, Longitude),
    .funs = as.numeric
  )

write_csv(PP_metadata, "to_submit/PP_metadata.csv")
dir.create("to_submit/PP")

for (gauge in pp_list) {
  station <- generate_rawdata(gauge)
  name_station <- sprintf(
    "%s%s.csv",
    "to_submit/PP/",
    station$station_code[1]
  )
  message(name_station)
  write_csv(station, name_station)
}



# 2. TN -------------------------------------------------------------------
tn_list <- sprintf("%s/%s/", data_dir, variables[2]) %>%
  list.files(
    pattern = "\\.dat", full.names = TRUE,
    all.files = TRUE
  )

# Create metadata
TN_metadata <- tn_list %>%
  map(~ create_metadata(.x)) %>%
  do.call(bind_rows, .) %>%
  mutate_at(
    .vars = vars(Latitude, Longitude),
    .funs = as.numeric
  )

write_csv(TN_metadata, "to_submit/TN_metadata.csv")
dir.create("to_submit/TN")
for (gauge in tn_list) {
  station <- generate_rawdata(gauge)
  name_station <- sprintf(
    "%s%s.csv",
    "to_submit/TN/",
    station$station_code[1]
  )
  message(name_station)
  write_csv(station, name_station)
}


# 2. TX ------------------------------------------------------------------
tx_list <- sprintf("%s/%s/", data_dir, variables[3]) %>%
  list.files(
    pattern = "\\.dat", full.names = TRUE,
    all.files = TRUE
  )

# Create metadata
TX_metadata <- tx_list %>%
  map(~ create_metadata(.x)) %>%
  do.call(bind_rows, .) %>%
  mutate_at(
    .vars = vars(Latitude, Longitude),
    .funs = as.numeric
  )

write_csv(TX_metadata, "to_submit/TX_metadata.csv")
dir.create("to_submit/TX")
for (gauge in tx_list) {
  station <- generate_rawdata(gauge)
  name_station <- sprintf(
    "%s%s.csv",
    "to_submit/TX/",
    station$station_code[1]
  )
  message(name_station)
  write_csv(station, name_station)
}