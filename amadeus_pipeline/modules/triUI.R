source("functions/rework_process_tri.R")

dynamicUI <- function() {
  tagList(
    div(align = 'center',
      selectInput(inputId = "selectYear", label = "Select Year", choices = c(2023:1988),
                  selected = 2022,
                  multiple = FALSE),
      sliderInput(inputId = 'searchRadius', label = "Buffer Radius (meters)", min = 0, max = 50000, value = 0, step = 100)
    )

    
    
  )
}

dynamicUI2 <- function(input, output, server, rv, session) {
  tagList(

  )
}

dynamicManipulateButton <- function(input, output, server, rv, session) {
  # print(input$averageGroupSelect)
  # rv$manipulated = rv$manipulated %>%
  #   group_by(across(all_of(input$averageGroupSelect))) %>%
  #   summarize(
  #     across(all_of(input$averageSelect), mean, na.rm = TRUE)
  #   )
}

dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  tri_files = list.files(path = "data/", pattern = "tri_", full.names = TRUE)
  
  file.remove(tri_files)
  
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
    radius = input$searchRadius
  )
  
  id = str_match(string = names(joined), pattern = "AIR_(.*?)_")[,2]
  col_ids = data.frame(column_ind = 1:length(id),
                       id = id)
  
  col_ids = col_ids %>%
    left_join(chem_names_ids, by = join_by("id" == "TRI_CHEMICAL_COMPOUND_ID")) %>%
    filter(complete.cases(.))
  
  for(i in 1:nrow(col_ids)){
    col = col_ids$column_ind[i]
    
    colnames(joined)[col] = gsub(pattern = col_ids$id[i],
                                 replacement = col_ids$CHEMICAL[i],
                                 x = colnames(joined)[col])
  }
  
  rv$joined = joined %>%
    st_drop_geometry()
  rv$manipulated = joined %>%
    st_drop_geometry()
  
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
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  t2 = Sys.time()
  rv$time_taken = round(as.numeric(t2 - t1, units = "mins"), 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  
  
  
}