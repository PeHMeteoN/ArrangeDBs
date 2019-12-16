# pip3 install geohash
# dir.create("to_submit/FINAL")

library(reticulate)
library(tidyverse)
library(purrr)
source("functions.R")

gh <- import("geohash")
data_dir <- "to_submit/"
metadata <- c("PP_metadata", "TN_metadata", "TX_metadata")
metadata_dir <- sprintf("%s/%s.csv", data_dir, metadata)

# Merging PP,TEMP MAX and MIN
metadata_df <- lapply(metadata_dir, function(x) read_csv(x))
total_code <- do.call(rbind, metadata_df)
all_decade_codes <- unique(total_code$DECADECode)

final_metadata_list <- list()
meta_data_list <- list()

for (z in 1:length(all_decade_codes)) {
  cat("processing: ", all_decade_codes[z])
  de_code <- all_decade_codes[z]
  pp_data <- read_csv(sprintf("to_submit/PP/%s.csv", de_code))

  tn_file <- sprintf("to_submit/TN/%s.csv", de_code)
  tn_data <- tryCatch(
    expr = read_csv(tn_file),
    error = function(e) data_frame(),
    finally = print("TN nodata")
  )

  tx_file <- sprintf("to_submit/TX/%s.csv", de_code)
  tx_data <- tryCatch(
    expr = read_csv(tn_file),
    error = function(e) data_frame(),
    finally = print("TX nodata")
  )

  min_date <- min(c(pp_data$date, tn_data$date, tx_data$date))
  max_date <- max(c(pp_data$date, tn_data$date, tx_data$date))
  max_sequence <- seq(min_date, max_date, "day")

  # PP
  if (length(pp_data) == 0) {
    pp_complete <- data_frame(
      date = max_sequence,
      station_code = NA,
      value = NA
    )
  } else {
    pp_complete <- pp_data %>% complete(date = max_sequence)
    pp_complete$station_code <- unique(pp_data$station_code)
  }

  # TN
  if (length(tn_data) == 0) {
    tn_complete <- data_frame(
      date = max_sequence,
      station_code = NA,
      value = NA
    )
  } else {
    tn_complete <- tn_data %>% complete(date = max_sequence)
    tn_complete$station_code <- unique(tn_data$station_code)
  }


  # TX
  if (length(tx_data) == 0) {
    tx_complete <- data_frame(
      date = max_sequence,
      station_code = NA,
      value = NA
    )
  } else {
    tx_complete <- tx_data %>% complete(date = max_sequence)
    tx_complete$station_code <- unique(tx_data$station_code)
  }

  # Creting phd code
  met_data <- total_code[which(total_code$DECADECode %in% de_code)[1], ]

  code <- encode_phd(
    country = "bo",
    ico = "M",
    source = 'CLIMANDES',
    state = "nodata",
    category = "nodata",
    lat = met_data$Latitude,
    long = met_data$Longitude,
    name = met_data$StationName
  )
  
  final_db <- data_frame(
    GEOCODE = code,
    DATE = pp_complete$date,
    PP = pp_complete$value,
    TN = tn_complete$value,
    TX = tx_complete$value
  ) %>%
    pivot_longer(c(-GEOCODE, -DATE), names_to = "VARIABLE", values_to = "VALUE")

  decode_data <- decode_phd(phd_code = code)

  final_metadata <- data_frame(
    GEOCODE = code,
    LAT = decode_data$lat,
    LON = decode_data$lon,
    SOURCE = "CLIMANDES",
    TYPE_STATION = decode_data$ico,
    STATE = decode_data$state,
    CATEGORY = decode_data$category,
    FIRST_YEAR = min_date,
    LAST_YEAR = max_date
  )

  met_data$GEOCODE <- code

  write_csv(final_db, sprintf("to_submit/FINAL/%s.csv", de_code))
  final_metadata_list[[z]] <- final_metadata
  meta_data_list[[z]] <- met_data
}

final_mtd <- do.call(rbind, final_metadata_list)
final_aux_mtd <- do.call(rbind, meta_data_list)

write_csv(final_mtd, "to_submit/db_bolivia_metadata.csv")
write_csv(final_aux_mtd, "to_submit/db_bolivia_auxiliary.csv")
