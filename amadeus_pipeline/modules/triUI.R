source("functions/rework_process_tri.R")

dynamicUI <- function() {
  tagList(
    selectInput(inputId = "selectYear", label = "Select Year(s)", choices = c(2023:1988),
                multiple = TRUE)
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  # Download data
  directory <- "data/"
  download_tri(
    year = input$selectYear,
    directory_to_save = "data/",
    acknowledgement = TRUE,
    download = TRUE,
    remove_command = TRUE
  )
  
  # Read the downloaded data into R spatrast
  
  tri <- rework_process_tri(
    path = "./data",
    year = input$selectYear
  )
  
  # Join data to participants
  # load("output/gis_simulated.RData")
  # locs <- epr.gis
  # locs <- data.frame(id = locs$epr_number, lon = locs$gis_longitude, lat = locs$gis_latitude)
  
  
  locs <- data.frame(id = rv$df$epr_number, lon = rv$df$gis_longitude, lat = rv$df$gis_latitude)
  locs_sf = st_as_sf(locs, coords = c("lon","lat"), crs = "EPSG:4269")
  
  joined = calculate_tri(
    from = tri,
    locs = locs_sf,
    locs_id = "id",
    radius = 1000L
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