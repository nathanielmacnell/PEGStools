dynamicUI <- function() {
  tagList(
    selectInput(inputId = "selectVariable", label = "Select Variable(s)",
                choices = c('weasd', 'snowc','snod')),
    dateRangeInput(inputId = 'dateRange', label = "Select Date Range", min = "1990-01-01", max = Sys.Date(),
                   start = "2022-01-01", end = "2022-01-05")
  )
}

dynamicUI2 <- function(input, output, server, rv, session) {
  tagList(
    strong(h3("Calculate average snow cover by ID")),
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
  
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  t1 = Sys.time()
  
  narr_files = list.files(path = "data/", pattern = "narr_", full.names = TRUE)
  weasd_files = list.files(path = "data/", pattern = "weasd", full.names = TRUE, recursive = TRUE)
  
  
  file.remove(narr_files)
  file.remove(weasd_files)
  
  
  directory <- "data/"
  download_data(
    dataset_name = input$selectDatasetName,
    # dataset_name = 'narr',
    year = year(input$dateRange),
    # year = c("2022", "2022"),
    # variable = "weasd",
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
    # covariate = "narr",
    date = input$dateRange,
    # date = c("2022-01-01","2022-01-05"),
    # variable = "weasd",
    variable = input$selectVariable,
    path = file.path(directory, input$selectVariable),
    # path = file.path(directory, "weasd"),
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
  rv$manipulated = weasd_covar %>%
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
  
  t2 = Sys.time()
  rv$time_taken = round(as.numeric(t2 - t1, units = "mins"), 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  
}