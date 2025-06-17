# EMPTY ZIP
download_nlcd(
  collection = "Coterminous United States",
  year = 2021,
  directory_to_save = "data/",
  acknowledgement = TRUE,
  download = TRUE,
  remove_command = FALSE,
  unzip = TRUE,
  remove_zip = FALSE,
  hash = FALSE
)

unzip("data/zip_files/2017neiApr_onroad_byregions.zip", "data/data_files/")

# LONG LOAD
download_population(
  data_resolution = "30 second",
  data_format = "GeoTIFF",
  year = "2020",
  directory_to_save = "data/",
  acknowledgement = TRUE,
  download = TRUE, # NOTE: download skipped for examples,
  remove_command = FALSE,
  unzip = TRUE
)

# WORKS
download_nei(
  year = c(2017L, 2018),
  directory_to_save = "data/",
  acknowledgement = TRUE,
  download = FALSE, # NOTE: download skipped for examples,
  remove_command = FALSE,
  unzip = TRUE
)

unzip("data/zip_files/2017neiApr_onroad_byregions.zip", exdir = "data/data_files/")

# WORKS
download_tri(
  year = 2021L,
  directory_to_save = "data/",
  acknowledgement = TRUE,
  download = TRUE, # NOTE: download skipped for examples,
  remove_command = FALSE
)

