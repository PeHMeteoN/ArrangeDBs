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