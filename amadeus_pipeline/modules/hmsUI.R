dynamicUI <- function() {
  tagList(
    dateRangeInput(inputId = 'dateRange', label = "Select Date Range", min = "1990-01-01", max = Sys.Date(),
                   start = "2023-06-01", end = "2023-06-08"),
    sliderInput(inputId = 'selectBufferRadius', label = "Buffer Radius (meters)", min = 0, max = 1000, value = 0, step = 10)
  )
}

dynamicUI2 <- function(input, output, server, rv, session) {
  tagList(
    strong(h3("Calculate average smoke cover by ID")),
    virtualSelectInput(inputId = "averageGroupSelect", label = "Select Column to Group by",
                       choices = names(rv$joined), multiple = FALSE),
    virtualSelectInput(inputId = "averageSelect", label = "Select Columns to Average",
                       choices = names(rv$joined), multiple = TRUE),
    br(),
    actionButton(inputId = "dynamicManipulateButton", label = "Apply Averages")
  )
}

dynamicManipulateButton <- function(input, output, server, rv, session) {
  print(input$averageGroupSelect)
  rv$manipulated = rv$manipulated %>%
    group_by(across(all_of(input$averageGroupSelect))) %>%
    summarize(
      across(all_of(input$averageSelect), mean, na.rm = TRUE)
    )
}


dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  unlink("data/data_files", recursive = TRUE)
  unlink("data/zip_files", recursive = TRUE)
  
  
  successful_download = FALSE
  
  # Download data
  directory <- "data/"
  tryCatch({
    download_hms(
      data_format = "Shapefile",
      date = input$dateRange,
      directory_to_save = directory,
      acknowledgement = TRUE,
      download = TRUE, 
      remove_command = TRUE,
      unzip = TRUE
    )
    
    successful_download = TRUE
  }, error = function(e) {
    successful_download = FALSE
    print(paste0("Download failed MESSAGE HERE: ", e$message))
    
    shinybusy::remove_modal_spinner()
    
    shinyalert::shinyalert(title = "Failed!",
                           text = "Invalid date range!",
                           type = "error")
  })
  
  # Read the downloaded data into R spatrast
  if(successful_download == TRUE){
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
      radius = input$selectBufferRadius,
      geom = 'sf'
    )
    
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
    print(rv$time_taken)
    updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  }

  
  
  
  
}