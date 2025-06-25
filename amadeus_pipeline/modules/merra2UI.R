library(earthdatalogin)
library(terra)
library(sf)
library(raster)




dynamicUI <- function() {
  tagList(

    actionButton(inputId = "checkAvailableData", label = "Check Available Data"),
    textInput(inputId = "dataName", "Input Dataset Name", value = "MERRA2_100.tavgM_2d_slv_Nx.198101.nc4")

  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  earthdatalogin::edl_download(href = paste0("https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2_MONTHLY/M2TMNXSLV.5.12.4/1981/", input$dataName),
                               dest = paste0("data/", input$dataName))

  r = rast(paste0("data/",input$dataName))
  names(r)


  # Convert to sf points
  participants_sf <- st_as_sf(rv$df, coords = c("gis_longitude", "gis_latitude"), crs = 4326)

  # 3. Ensure CRS match between raster and points (reproject points if needed)
  if (!compareCRS(r, participants_sf)) {
    participants_sf <- st_transform(participants_sf, crs(r))
  }

  # 4. Extract raster values at participant points
  vals <- terra::extract(r, vect(participants_sf))

  # 5. Combine extracted values with participant data
  result <- cbind(rv$df, vals)
  
  rv$joined = result %>%
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