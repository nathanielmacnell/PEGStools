
earth_data_token = "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6Im5hbWF0byIsImV4cCI6MTc1MjU5MzM0NywiaWF0IjoxNzQ3NDA5MzQ3LCJpc3MiOiJodHRwczovL3Vycy5lYXJ0aGRhdGEubmFzYS5nb3YiLCJpZGVudGl0eV9wcm92aWRlciI6ImVkbF9vcHMiLCJhY3IiOiJlZGwiLCJhc3N1cmFuY2VfbGV2ZWwiOjN9.jIqZbGtxQsYGt5l30EW5MBybtHw-AjKSvVvfMLCwlLrcn69gsRYcTWDgxhybZ64TbbsD_2XHHcLHddFMvc8j7aN6W2nQHlWWOdp5Z0c7rCLTc5lzhbKt156FymVGR62U4Lee-Z5a0T6IlAVxsZSi2yx9KtOPT36eL9Rcr9SEPENNauiEUwhiLLuXKT3PbgomHQRg_jxMytQ7xpFFpObIGKIoPqKLzdl-goFqQwxmeyMiRbMPiWcVygd7NK1TxwLfUsGDHD99XO-DvbxFgw256JvEQzNhRl0FTzizbUfIUgUwz29_57l2NjZHS04WPgi-hjQ2-724qFRppojUuKyDgw"

dynamicUI <- function() {
  tagList(
    virtualSelectInput(inputId = 'product', label = "Select Product",
                       choices = c("MOD09GA", "MYD09GA", "MOD09GQ", "MYD09GQ", "MOD09A1", "MYD09A1",
                                   "MOD09Q1", "MYD09Q1", "MOD11A1", "MYD11A1", "MOD11A2", "MYD11A2", "MOD11B1",
                                   "MYD11B1", "MOD13A1", "MYD13A1", "MOD13A2", "MYD13A2", "MOD13A3", "MYD13A3",
                                   "MOD06_L2", "MCD19A2", "VNP46A2")),

    dateRangeInput(inputId = 'dateRange', label = "Select Date Range", min = "1990-01-01", max = Sys.Date(),
                   start = "2024-01-01", end = "2024-01-05")
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  directory <- "data/"
  download_modis(
    # product = input$product,
    product = "MOD09GA",
    version = "61",
    horizontal_tiles = c(8,8),
    vertical_tiles = c(4,4),
    # date = input$dateRange,
    date = c("2024-01-01", "2024-01-05"),
    directory_to_save = "data/",
    # directory_to_save = directory,
    nasa_earth_data_token = earth_data_token,
    acknowledgement = TRUE,
    download = TRUE,
    hash = FALSE,
    remove_command = TRUE
  )
  
  # download_modis(
  #   product = "MOD13A1",
  #   version = "61",
  #   horizontal_tiles = c(8,11),
  #   vertical_tiles = c(5,5),
  #   date = "2024-01-01",
  #   directory_to_save = "data/",
  #   nasa_earth_data_token = earth_data_token,
  #   acknowledgement = TRUE,
  #   download = TRUE,
  #   hash = FALSE,
  #   remove_command = TRUE
  # )
  
  # Read the downloaded data into R (and apply some filters)
  
  gdal_subdatasets(file = paste0(getwd(),
                                 "/data/2024/001/MOD13A1.A2024001.h08v05.061.2024022142113.hdf"))
  
  process <- process_modis_merge(
    date = "2024-01-01",
    subdataset = "500m 16 days NDVI",
    path = list.files("data/", pattern = 'MOD13A1',
                      full.names = TRUE,
                      recursive = TRUE),
    fun_agg = "mean"
  )
  



  # Join
  locs <- data.frame(id = rv$df$epr_number, lon = rv$df$gis_longitude, lat = rv$df$gis_latitude)
  locs_sf <- st_as_sf(locs, coords = c("gis_longitude","gis_latitude"), crs = 4326)
  linked_modis = calculate_modis(
    from = list.files("data/2024/", pattern = "MOD13A1", full.names = TRUE, recursive = TRUE),
    locs = locs_sf,
    locs_id = "id",
    radius = c(0L, 1000L),
    preprocess = process_modis_merge,
    name_covariates = "MODIS_Grid_16DAY_500m_VI",
    subdataset = "500m 16 days NDVI",
    fun_summary = "mean"
  )
  
  output$linkDisplay = renderDataTable(datatable(linked_modis, rownames = FALSE,
                                                 class = 'table table-striped table-hover table-dark'))
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  t2 = Sys.time()
  rv$time_taken = round(t2 - t1, 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  
}