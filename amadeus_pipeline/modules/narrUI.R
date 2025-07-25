dynamicUI <- function() {
  tagList(
    selectInput(inputId = "selectVariable", label = "Select Variable(s)",
                choices = c('weasd', 'snowc','snod')),
    dateRangeInput(inputId = 'dateRange', label = "Select Date Range", min = "1990-01-01", max = Sys.Date(),
                   start = "2022-01-01", end = "2022-01-05")
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  t1 = Sys.time()
  
  
  directory <- "data/"
  download_data(
    dataset_name = input$selectDatasetName,
    year = year(input$dateRange),
    variable = input$selectVariable,
    directory_to_save = directory,
    acknowledgement = TRUE,
    download = TRUE,
    hash = FALSE,
    remove_command = TRUE
  )
  
  # Read the downloaded data into R (and apply some filters)
  
  weasd_process <- process_covariates(
    covariate = input$selectDatasetName,
    date = input$dateRange,
    variable = input$selectVariable,
    path = file.path(directory, input$selectVariable),
    extent = NULL
  )
  
  # Join weasd (snow cover) to simulated participants
  locs <- data.frame(id = rv$df$epr_number, lon = rv$df$gis_longitude, lat = rv$df$gis_latitude)
  weasd_covar <- calculate_covariates(
    covariate = input$selectDatasetName,
    from = weasd_process,
    locs = locs,
    locs_id = "id",
    radius = 0,
    geom = "sf"
  )
  
  rv$joined = weasd_covar %>%
    st_drop_geometry()
  
  output$linkDisplay = renderDataTable(datatable(rv$joined, rownames = FALSE,
                                                 extensions = "Buttons",
                                                 options = list(
                                                   dom = 'Bfrtip',
                                                   buttons = 'csv',
                                                   pageLength = 10
                                                 )))
  
  shinybusy::remove_modal_spinner()
  
  t2 = Sys.time()
  rv$time_taken = round(t2 - t1, 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  
}