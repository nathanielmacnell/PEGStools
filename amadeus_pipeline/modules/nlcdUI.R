library(earthdatalogin)
library(terra)
library(sf)
library(raster)




dynamicUI <- function() {
  tagList(
    
    virtualSelectInput(inputId = 'selectYear', label = "Select Year",
                       choices = c(1985:2024), selected = 2022, multiple = FALSE),
    sliderInput(inputId = 'searchRadius', label = "Buffer Radius (meters)", min = 0, max = 1000, value = 0, step = 10)
    
  )
}

dynamicUI2 <- function(input, output, server, rv, session) {
  tagList(
    virtualSelectInput(inputId = "landCoverSelect", label = "Select Land Cover(s) of Interest",
                       choices = unique(rv$joined$NLCD.Land.Cover.Class), multiple = TRUE),
    br(),
    br(),
    actionButton(inputId = "dynamicManipulateButton", label = "Filter Land Cover")
  )
}

dynamicManipulateButton <- function(input, output, server, rv, session) {
  rv$manipulated = rv$manipulated %>%
    filter(NLCD.Land.Cover.Class %in% input$landCoverSelect)
  
  if(!is.null(input$landCoverSelect)){
    tmp = data.frame(inputId = "landCoverSelect",
                     type = "virtualSelectInput",
                     value = paste(input$landCoverSelect, collapse = ", "),
                     group = 2)
    
    rv$inputs_df = bind_rows(rv$inputs_df, tmp)
  }
  
}

dynamicButton <- function(input, output, server, rv, session){
  
  numeric_landcover <- rast(paste0("../for_host/nlcd/Num_Land_Cover_",input$selectYear,".tif"))
  # numeric_landcover <- rast(paste0("for_host/nlcd/Num_Land_Cover_",1985,".tif"))
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  # Convert to sf points
  participants_sf <- st_as_sf(rv$df, coords = c("gis_longitude", "gis_latitude"), crs = 4326)
  # participants_sf <- st_as_sf(df, coords = c("gis_longitude", "gis_latitude"), crs = 4326)
  
  # convert to NLCD crs
  participants_sf <- st_transform(participants_sf, crs = st_crs(numeric_landcover))
  
  st_crs(participants_sf)
  
  # 4. Extract raster values at participant points
  # numeric_landcover <- as.numeric(landcover)
  # Extract with modal land cover within 90m buffer
  vals <- terra::extract(numeric_landcover, vect(participants_sf), fun = modal, search_radius = input$searchRadius)
  
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
  result <- cbind(rv$df, coded_vals[, -1]) %>%
    dplyr::select(-`Pixel Count`)
  
  rv$joined = result 
  rv$manipulated = result
  
  output$linkDisplay = renderDataTable(datatable(rv$joined, rownames = FALSE,
                                                 extensions = "Buttons",
                                                 options = list(
                                                   dom = 'Bfrtip',
                                                   buttons = list(
                                                     list(
                                                       extend = "csv",
                                                       text = "Download CSV",
                                                       exportOptions = list(
                                                         modifier = list(page = "all")  # export all rows, not just visible
                                                       )
                                                     )
                                                   ),
                                                   pageLength = 10
                                                 )),
                                       class = 'table table-striped table-hover table-dark',
                                       server = FALSE)
  
  output$manipulatedDisplay = renderDataTable(datatable(rv$manipulated, rownames = FALSE,
                                                        extensions = "Buttons",
                                                        options = list(
                                                          dom = 'Bfrtip',
                                                          buttons = list(
                                                            list(
                                                              extend = "csv",
                                                              text = "Download CSV",
                                                              exportOptions = list(
                                                                modifier = list(page = "all")  # export all rows, not just visible
                                                              )
                                                            )
                                                          ),
                                                          pageLength = 10
                                                        )),
                                              class = 'table table-striped table-hover table-dark',
                                              server = FALSE)
  
  print(input$selectDatasetName)
  print(input$selectYear)
  print(input$searchRadius)
  
  
  rv$inputs_df = data.frame(inputId = c("selectDatasetName", "selectYear", "searchRadius"),
                            type = c("selectInput", "virtualSelectInput", "sliderInput"),
                            value = c(input$selectDatasetName, input$selectYear, input$searchRadius),
                            group = c(1, 1, 1))
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  t2 = Sys.time()
  rv$time_taken = round(as.numeric(t2 - t1, units = "mins"), 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  
  
  
}