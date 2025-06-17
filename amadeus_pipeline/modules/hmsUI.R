dynamicUI <- function() {
  tagList(
    dateRangeInput(inputId = 'dateRange', label = "Select Date Range", min = "1990-01-01", max = Sys.Date(),
                   start = "2022-01-01", end = "2022-01-05")
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  # Download data
  directory <- "data/"
  download_hms(
    data_format = "Shapefile",
    date = input$dateRange,
    directory_to_save = directory,
    acknowledgement = TRUE,
    download = TRUE, 
    remove_command = TRUE,
    unzip = TRUE
  )
  
  # Read the downloaded data into R spatrast
  
  hms <- process_hms(
    date = input$dateRange,
    path = "data/data_files"
  )
  
  # Join data to participants
  locs <- data.frame(id = rv$df$epr_number, lon = rv$df$gis_longitude, lat = rv$df$gis_latitude)
  
  joined = calculate_hms(
    from = hms,
    locs = locs,
    locs_id = "id",
    radius = 0,
    geom = 'sf'
  )
  
  rv$joined = joined %>%
    st_drop_geometry()
  
  output$linkDisplay = renderDataTable(datatable(rv$joined, rownames = FALSE),
                                       class = 'table table-striped table-hover table-dark')
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  t2 = Sys.time()
  rv$time_taken = round(t2 - t1, 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  
  
  
}