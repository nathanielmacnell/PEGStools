library(earthdatalogin)
library(terra)
library(sf)
library(raster)




dynamicUI <- function() {
  tagList(
    
    virtualSelectInput(inputId = 'selectYear', label = "Select Year",
                       choices = c(1985:2024), selected = 2022, multiple = FALSE)
    
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  landcover <- readRDS(paste0("../for_host/nlcd/Land_Cover_",input$selectYear,".rds"))
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  # Convert to sf points
  participants_sf <- st_as_sf(rv$df, coords = c("gis_longitude", "gis_latitude"), crs = 4326)
  
  # 3. Ensure CRS match between raster and points (reproject points if needed)
  if (!compareCRS(landcover, participants_sf)) {
    participants_sf <- st_transform(participants_sf, st_crs(landcover))
  }
  
  # 4. Extract raster values at participant points
  numeric_landcover <- as.numeric(landcover)
  # Extract with modal land cover within 90m buffer
  vals <- terra::extract(numeric_landcover, vect(participants_sf), fun = modal)
  
  code_map <- data.frame(
    `Pixel Value` = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95, 250),
    `NLCD Land Cover Class` = c(
      "Open Water",
      "Perennial Ice/Snow",
      "Developed, Open Space",
      "Developed, Low Intensity",
      "Developed, Medium Intensity",
      "Developed, High Intensity",
      "Barren Land (Rock/Sand/Clay)",
      "Deciduous Forest",
      "Evergreen Forest",
      "Mixed Forest",
      "Shrub/Scrub",
      "Grassland/Herbaceous",
      "Pasture/Hay",
      "Cultivated Crops",
      "Woody Wetlands",
      "Emergent Herbaceous Wetlands",
      "NoData"
    )
  )
  
  names(code_map)[1] = names(vals)[2]
  
  coded_vals = vals %>%
    left_join(code_map, by = names(vals)[2])
  
  # Combine with original data
  result <- cbind(rv$df, coded_vals[, -1])  # Remove 'ID' duplicate
  
  rv$joined = result 
  
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