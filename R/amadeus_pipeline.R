library(amadeus)

directory <- "data/"
download_data(
  dataset_name = "narr",
  year = 2022,
  variable = "weasd",
  directory_to_save = directory,
  acknowledgement = TRUE,
  download = TRUE,
  hash = TRUE
)

list.files(file.path(directory, "weasd"))

weasd_process <- process_covariates(
  covariate = "narr",
  date = c("2022-01-01", "2022-01-05"),
  variable = "weasd",
  path = file.path(directory, "weasd"),
  extent = NULL
)

weasd_process

load("data/gis_simulated.RData")
names(epr.gis)

locs <- data.frame(id = epr.gis$epr_number, lon = epr.gis$gis_longitude, lat = epr.gis$gis_latitude)
weasd_covar <- calculate_covariates(
  covariate = "narr",
  from = weasd_process,
  locs = locs,
  locs_id = "id",
  radius = 0,
  geom = "sf"
)

weasd_covar 
