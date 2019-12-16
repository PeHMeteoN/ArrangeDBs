# From CR2 to PhD POSTGIS -------------------------------------------------
# pip3 install geohash
# dir.create("to_submit/FINAL")

library(lubridate)
library(reticulate)
library(tidyverse)
library(purrr)
source("functions.R")

gh <- import("geohash")
data_dir <- "to_submit/"



# 1. Reading data ---------------------------------------------------------
# - metadata
db_description <- list.files('db/','_stations_',recursive = TRUE,full.names = TRUE)
db_data <- list.files('db/','\\_2018_ghcn.csv$',recursive = TRUE,full.names = TRUE)
metadata_df <- lapply(db_description, read_csv) %>% 
  do.call(rbind,.)

# - precipitation
pp_data <- read_csv(db_data[1])
pp_colnames <- colnames(pp_data)[2:ncol(pp_data)]
fix_col_pp <- sapply(pp_colnames, fix_col_fn)

# - Tx
tx_data <- read_csv(db_data[2])
tx_colnames <- colnames(tx_data)[2:ncol(tx_data)]
fix_col_tx <- sapply(tx_colnames, fix_col_fn)

# - Tn
tn_data <- read_csv(db_data[3])
tn_colnames <- colnames(tn_data)[2:ncol(tn_data)]
fix_col_tn <- sapply(tn_colnames, fix_col_fn)

# - Merging metadata PP,TX and TN
all_decade_codes <- unique(metadata_df$codigo_estacion)





# Saving metadata ---------------------------------------------------------
final_metadata_list <- list()
meta_data_list <- list()

for (z in 1:length(all_decade_codes)) {
  cat("processing: ", all_decade_codes[z])
  de_code <- all_decade_codes[z]
  
  # Reading just Date and values for a specific
  # gauge station.
  
  # - pp
  pp_data_se <- pp_data[15:nrow(pp_data),
                        c(1, (which(fix_col_pp %in% de_code)+1))]
  
  if (ncol(pp_data_se) == 2) {
    colnames(pp_data_se) <- c('date','value')
    pp_data_se$value[as.numeric(pp_data_se$value) == -9999]=NA
    pp_data_se <- na.omit(pp_data_se)
    date_pp <- pp_data_se %>%
      na.omit() %>%
      arrange(date) %>% 
      '[['('date')
    init_pp <- sprintf('%s-01-01',year(min(date_pp)))
    last_pp <- sprintf('%s-12-31',year(max(date_pp)))
  } else {
    init_pp <- NA
    last_pp <- NA
  }
  
  # - tx
  tx_data_se <- tx_data[15:nrow(tx_data),
                        c(1, (which(fix_col_tx %in% de_code)+1))]

  if (ncol(tx_data_se) == 2) {
    colnames(tx_data_se) <- c('date','value')
    tx_data_se$value[as.numeric(tx_data_se$value) == -9999]=NA
    tx_data_se <- na.omit(tx_data_se)
    date_tx <- tx_data_se %>%
      na.omit() %>%
      arrange(date) %>% 
      '[['('date')
    init_tx <- sprintf('%s-01-01',year(min(date_tx)))
    last_tx <- sprintf('%s-12-31',year(max(date_tx)))
  } else {
    init_tx <- NA
    last_tx <- NA
  }
  
  # - tn
  tn_data_se <- tn_data[15:nrow(tn_data),
                        c(1, (which(fix_col_tn %in% de_code)+1))]

  if (ncol(tn_data_se) == 2) {
    colnames(tn_data_se) <- c('date','value')
    tn_data_se$value[as.numeric(tn_data_se$value) == -9999]=NA
    tn_data_se <- na.omit(tn_data_se)
    date_tn <- tn_data_se %>%
      na.omit() %>%
      arrange(date) %>% 
      '[['('date')
    init_tn <- sprintf('%s-01-01',year(min(date_tn)))
    last_tn <- sprintf('%s-12-31',year(max(date_tn)))
  } else {
    init_tn <- NA
    last_tn <- NA
  }
      
  # Calculating max date sequence for the gauge station
  min_date <- min(na.omit(c(init_pp, init_tx, init_tn)))
  max_date <- max(na.omit(c(last_pp, last_tx, last_tn)))
  max_sequence <- seq(as.Date(min_date), as.Date(max_date), "day")
  
  # Creating stable data_frame with a date range equals to
  # 'max_sequence'.
  
  # - pp
  if (ncol(pp_data_se) != 2) {
    pp_data_se <- data_frame (
      date = max_sequence,
      value = NA
    )
  } else {
    colnames(pp_data_se) <- c('date','value')
    pp_data_se <- pp_data_se %>%
      mutate(date = as.Date(date)) %>% 
      complete(date = max_sequence)
    pp_data_se$value[pp_data_se$value == "-9999"] = NA
  }
  
  # - tx
  if (ncol(tx_data_se) != 2) {
    tx_data_se <- data_frame(
      date = max_sequence,
      value = NA
    )  
  } else {
    colnames(tx_data_se) <- c('date','value')
    tx_data_se <- tx_data_se %>%
      mutate(date = as.Date(date)) %>% 
      complete(date = max_sequence)
    tx_data_se$value[tx_data_se$value == "-9999"] = NA
  }
  
  # - tn
  if (ncol(tn_data_se) != 2) {
    tn_data_se <- data_frame(
      date = max_sequence,
      value = NA
    )  
  } else {
    colnames(tn_data_se) <- c('date','value')
    tn_data_se <- tn_data_se %>%
      mutate(date = as.Date(date)) %>% 
      complete(date = max_sequence)
    tn_data_se$value[tn_data_se$value == "-9999"] = NA
  }
  
  # Selecting the metadata for the specific gauge station.
  met_data <- metadata_df[which(metadata_df$codigo_estacion %in% de_code)[1], ]
  
  code <- encode_phd(
    country = "cl",
    ico = "M",
    source = 'CR2',
    state = "nodata",
    category = "nodata",
    lat = met_data$latitud,
    long = met_data$longitud,
    name = met_data$nombre
  )
  
  # Creating PhD metadata 
  final_db <- data_frame(
    GEOCODE = code,
    DATE = max_sequence,
    PP = as.numeric(pp_data_se$value),
    TN = as.numeric(tn_data_se$value),
    TX = as.numeric(tx_data_se$value)
  ) %>%
    pivot_longer(c(-GEOCODE, -DATE), names_to = "VARIABLE", values_to = "VALUE")
  
  # Creating PhD data 
  decode_data <- decode_phd(phd_code = code)
  
  final_metadata <- data_frame(
    GEOCODE = code,
    LAT = decode_data$lat,
    LON = decode_data$lon,
    SOURCE = "CR2",
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

write_csv(final_mtd, "to_submit/db_chile_metadata.csv")
write_csv(final_aux_mtd, "to_submit/db_chile_auxiliary.csv")
