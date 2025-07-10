# # EMPTY ZIP
# download_nlcd(
#   collection = "Coterminous United States",
#   year = 2021,
#   directory_to_save = "data/",
#   acknowledgement = TRUE,
#   download = TRUE,
#   remove_command = FALSE,
#   unzip = TRUE,
#   remove_zip = FALSE,
#   hash = FALSE
# )
# 
# ## Test get from MRLC website NLCD data
# library(httr)
# GET("https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_LndCov_2024_CU_C1V1.zip",
#           write_disk("data/testing.zip"))


# LONG LOAD
download_groads(
  data_region = "Americas",
  data_format = "Shapefile",
  directory_to_save = 'data',
  acknowledgement = TRUE,
  download = TRUE, # NOTE: download skipped for examples,
  remove_command = TRUE,
  unzip = TRUE
)

groads <- process_groads(
  path = "data/data_files/groads-v1-americas-shp/gROADS-v1-americas.shp"
)

loc <- epr.gis %>%
  rename("lat" = "gis_latitude",
         "lon" = "gis_longitude")

loc_vec <- vect(loc, geom = c("lon", "lat"), crs = "EPSG:4326")
loc_proj <- project(loc_vec, "EPSG:3857")

groads_proj = project(groads, "EPSG:3857")

test_join = nearest(loc_proj, groads_proj)

groads_nearest <- groads_proj[test_join]

df_test = as.data.frame(test_join)

linked = cbind(as.data.frame(loc), as.data.frame(groads_nearest))
  
x = calculate_groads(
  from = groads, # derived from process_groads() example
  locs = loc,
  locs_id = "epr_number",
  radius = 1000,
  fun = "sum",
  geom = FALSE
)


# # WORKS
download_nei(
  year = c(2017L, 2018),
  directory_to_save = "data/",
  acknowledgement = TRUE,
  download = TRUE, # NOTE: download skipped for examples,
  remove_command = FALSE,
  unzip = TRUE
)
# 
# unzip("data/zip_files/2017neiApr_onroad_byregions.zip", exdir = "data/data_files/")
# 
# # WORKS
# download_tri(
#   year = 2021L,
#   directory_to_save = "data/",
#   acknowledgement = TRUE,
#   download = TRUE, # NOTE: download skipped for examples,
#   remove_command = FALSE
# )

download_merra2(
  collection = "inst1_2d_int_Nx",
  date = "2024-01-01",
  directory_to_save = "data/",
  acknowledgement = TRUE,
  download = TRUE, # NOTE: download skipped for examples,
  remove_command = FALSE,
)

