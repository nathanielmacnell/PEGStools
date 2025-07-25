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
  
  numeric_landcover <- rast(paste0("../for_host/nlcd/Num_Land_Cover_",input$selectYear,".tif"))
  # numeric_landcover <- rast(paste0("for_host/nlcd/Num_Land_Cover_",1985,".tif"))
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  # Convert to sf points
  participants_sf <- st_as_sf(rv$df, coords = c("gis_longitude", "gis_latitude"), crs = 4326)
  # participants_sf <- st_as_sf(epr.gis, coords = c("gis_longitude", "gis_latitude"), crs = 4326)
  
  # convert to NLCD crs
  aea_proj <- "PROJCRS[\"AEA        WGS84\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"Albers Equal Area\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",23,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",29.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",45.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
  participants_sf <- st_transform(participants_sf, crs = aea_proj)
  
  
  # 4. Extract raster values at participant points
  # numeric_landcover <- as.numeric(landcover)
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
  
  output$linkDisplay = renderDataTable(datatable(rv$joined, rownames = FALSE,
                                                 extensions = "Buttons",
                                                 options = list(
                                                   dom = 'Bfrtip',
                                                   buttons = 'csv',
                                                   pageLength = 10
                                                 )),
                                       class = 'table table-striped table-hover table-dark')
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  t2 = Sys.time()
  rv$time_taken = round(t2 - t1, 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  
  
  
}