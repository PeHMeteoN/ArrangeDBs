# Functions ---------------------------------------------------------------
create_metadata <- function(dat_file) {
  con <- file(dat_file, "r", blocking = FALSE)
  con_read <- readLines(con, n = -1)
  close(con)
  con_read[1:11] %>%
    strsplit(":") %>%
    map(~ str_replace_all(.x, " ", "")) %>%
    do.call(rbind, .) %>%
    `colnames<-`(c("VARIABLE", "VALUE")) %>%
    as_tibble() %>%
    pivot_wider(names_from = VARIABLE, values_from = VALUE)
}


generate_rawdata <- function(station) {
  con <- file(station, "r", blocking = FALSE)
  con_read <- readLines(con, n = -1)
  close(con)
  station_code <- con_read[2] %>%
    str_extract("\\:.*") %>%
    str_remove(": ") # lazzy

  raw_data <- con_read[15:length(con_read)] %>%
    strsplit(" ") %>%
    map(~ arrange_gauge(.x)) %>%
    do.call(bind_rows, .)
  raw_data$station_code <- rep(station_code, nrow(raw_data))
  raw_data[c(3, 1, 2)]
}


arrange_gauge <- function(x) {
  data_raw <- subset(x, !(x %in% ""))[1:5]
  # julyday <- data_raw[1]
  num_data <- as.numeric(data_raw)
  date_data <- sprintf(
    "%s-%02d-%02d", num_data[2],
    num_data[3], num_data[4]
  )
  value_data <- num_data[5]
  data_frame(
    date = date_data,
    value = value_data
  )
}

reftable <- data_frame(
  bit_32 = c(0:9,letters[c(-9,-1,-12,-14)]),
  ico  = c("H","M",'nodata', rep(NA,29)),
  ico_name = c('HIDROLOGICA','METEOROLOGICA','nodata', rep(NA,29)),
  source = c('SENAMHI_HISTORIC','PISCOp','SENAMHI-REALTIME',
             'CLIMANDES','CR2','IDEAM','INAMHI','ANA', rep(NA,24)),
  state  = c('AUTOMATICA', 'DIFERIDO', 'REAL','nodata', rep(NA,28)),
  cate  = c('CO', 'CP', 'EAA', 'EAMA', 'EHA', 'EHMA',
            'EMA', 'HLG', 'HLM', 'MAP', 'PE','nodata',rep(NA,20)),
  cate_name = c('Climatica_Ordinaria',
                'Climatica_Principal',
                'Estacion_Agrometeorologica_Automatica',
                'Estacion_Agrometeorologica_Automatica',
                'Estacion_Hidrologica_Automatica',
                'Estacion_Hidrometeorologica_Automatica',
                'Estacion_Meteorologica_Automatica',
                'Estacion_Hidrologica_Limnigrafica',
                'Estacion_Hidrologica_Limnimetrica',
                'Estacion_Meteorologica_Agricola_Principal',
                'Estacion_Meteorologica_Proposito_Especifico',
                'nodata',
                rep(NA,20)
  )
)
write_csv(reftable,'phdcode.csv')

encode_phd <- function(country,
                       ico,
                       source,
                       state,
                       category,
                       lat,
                       long,
                       name,
                       phddf = "phdcode.csv") {
  phd_df <- read_csv(phddf)
  # ico
  ico_code <- phd_df %>%
    "["(phd_df$ico == ico, ) %>%
    "[["("bit_32") %>%
    na.omit() %>%
    as.character()
  
  # source
  source_code <- phd_df %>%
    "["(phd_df$source == source, ) %>%
    "[["("bit_32") %>%
    na.omit() %>%
    as.character()
  
  # state
  state_code <- phd_df %>%
    "["(phd_df$state == state, ) %>%
    "[["("bit_32") %>%
    na.omit() %>%
    as.character()
  
  # category
  category_code <- phd_df %>%
    "["(phd_df$cate == category, ) %>%
    "[["("bit_32") %>%
    na.omit() %>%
    as.character()
  
  # GEOHASH: phd define a general precision of 13
  geohash <- gh$encode(
    as.numeric(lat),
    as.numeric(long), 13L
  )
  # length name
  length_name <- phd_df$bit_32[nchar(name)]
  if (nchar(name) > 32) length_name <- "z"
  
  # last letter
  last_letter <- sapply(strsplit(name, ""), tail, 1)
  
  # phd code
  sprintf(
    "%s%s%s%s%s%s%s%s", 
    country,
    ico_code, 
    source_code,
    state_code,
    category_code, geohash,
    length_name, last_letter
  )
}

decode_phd <- function(phd_code, phddf = "phdcode.csv") {
  
  phd_df <- read_csv(phddf)
  country <- substr(phd_code, 1, 2)
  
  ico <- phd_df[phd_df$bit_32 == substr(phd_code, 3, 3), "ico_name"][[1]]
  source <- phd_df[phd_df$bit_32 == substr(phd_code, 4, 4), "source"][[1]]
  state <- phd_df[phd_df$bit_32 == substr(phd_code, 5, 5), "state"][[1]]
  category <- phd_df[phd_df$bit_32 == substr(phd_code, 6, 6), "cate"][[1]]
  geohash <- substr(phd_code, 7, 19)
  lat <- gh$decode(geohash)[[1]]
  lon <- gh$decode(geohash)[[2]]
  length_name <- phd_df[phd_df$bit_32 == substr(phd_code, 20, 20), "bit_32"][[1]]
  last_letter <- substr(phd_code, 21, 21)
  data_frame(
    country = country,
    ico = ico,
    state = state,
    category = category,
    lat = lat,
    lon = lon,
    length_name = length_name,
    last_letter = last_letter
  )
}
