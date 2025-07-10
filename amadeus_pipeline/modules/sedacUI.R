dynamicUI <- function() {
  tagList(
    h3("Date Range 1980-2010")
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  # saveRDS(groads, "amadeus_pipeline/data/sedac/gROADS-v1-americas.rds")
  groads <- readRDS("data/sedac/gROADS-v1-americas.rds")
  
  
  # Join data to participants
  locs <- data.frame(id = rv$df$epr_number, lon = rv$df$gis_longitude, lat = rv$df$gis_latitude)
  
  joined = calculate_groads(
    from = groads,
    locs = locs,
    locs_id = "id",
    radius = 1000,
    fun = "sum",
    geom = FALSE
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