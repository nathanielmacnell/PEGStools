library(shiny)
library(DT)
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(shinythemes)
library(amadeus)
library(shinyWidgets)
library(lubridate)
library(shinybusy)
library(shinyalert)
library(stringr)
library(leaflet)
library(shinyjs)
library(tigris)
library(stringr)
library(httr)
library(rvest)
library(shinydashboard)
library(tibble)
library(purrr)
library(openxlsx)

# Get map data
states <- map_data("state")

# Get available data options
available_data = list.files('modules', full.names = FALSE)
available_data = str_match(available_data, "(.*?)UI")[,2]


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'black',
                    dashboardHeader(title = "Amadeus Pipeline"),
                    dashboardSidebar(width = '300px',
                                     sidebarMenu(
                                       id = "sidebarTabs",
                                       menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
                                       menuItem("Analysis", icon = icon("th"), tabName = "analysis")
                                     ),
                                     conditionalPanel(condition = "input.sidebarTabs == 'overview'",
                                                      
                                     ),
                                     conditionalPanel( condition = "input.sidebarTabs == 'analysis'",
                                                       div(align = 'center',

                                                           div(class="file-input",
                                                               fileInput(inputId = "fileInput", label = "Upload Participant Data"),
                                                           ),
                                                           paste0("If you want to replicate settings from a prior linkage, ",
                                                                  "you can upload a config file."),
                                                           div(class="file-input",
                                                               fileInput(inputId = "configInput", label = "Upload Config File"),
                                                           ),
                                                           selectInput(inputId = "selectDatasetName", label = "Dataset Name",
                                                                       choices = c("")),
                                                           uiOutput("dynamicUI"),
                                                           br(),
                                                           br(),
                                                           hr(),
                                                           actionButton(inputId = 'downloadSelected', label = 'Download and Link', class = 'btn-success btn-colored'),
                                                       )
                                                       
                                     )
                                     
                    ),
                    dashboardBody(
                      shinybusy::add_busy_spinner(position = 'top-right', color = 'black'),
                      tabItems(
                        tabItem(
                          tabName = "overview",
                          fluidRow(
                            column(
                              width = 12,
                              box(
                                width = 12,
                                status = "primary",
                                solidHeader = TRUE,
                                title = "Welcome to the Amadeus Data Integration App",
                                HTML("
          <p style='font-size: 16px;'>
            This application lets you seamlessly <strong>download, link, and analyze</strong> Amadeus datasets
            with your own participant data. 
          </p>
          <p style='font-size: 16px;'>
            This app has been designed in a modular fashion, meaning additional data sources/linking functionality can be easily added by other developers.
             The underlying package that supports this apps functionality is the 'amadeus' package. Some of the functions in the CRAN version of amadeus 
             are outdated (some now require auth). Custom versions of various amadeus functions have been created as a work around.
          </p>
        ")
                              )
                            )
                          ),
                          
                          fluidRow(
                            box(
                              height = "150px",
                              width = 6,
                              status = "primary",
                              solidHeader = TRUE,
                              title = "How to Get Started",
                              HTML("
        <ol style='font-size: 15px;'>
          <li>Navigate to the <strong>Analysis</strong> tab.</li>
          <li>Upload your participant data file.</li>
          <li>Choose the Amadeus dataset you want to download.</li>
          <li>Click <em>Download and Link</em> to merge your data.</li>
        </ol>
      ")
                            ),
                            
                            
                            
                            box(
                              height = "150px",
                              width = 6,
                              status = "primary",
                              solidHeader = TRUE,
                              title = "About the Amadeus Package",
                              HTML("
        <p style='font-size: 15px;'>
          The <strong>amadeus</strong> R package provides direct access to a variety of rich datasets.
          This app uses it under the hood to help you source and merge data faster, without the need
          to write complex R scripts. To learn more about the Amadeus package, click <a href=https://niehs.github.io/amadeus/ target=_blank>
                                     HERE </a>
        </p>
      ")
                            )
                          ),
                          hr(id = "break_line"),
                          
                          fluidRow(
                            valueBox(
                              value = "Step 1",
                              subtitle = "Upload your participant data",
                              icon = icon("upload"),
                              color = "aqua",
                              width = 4
                            ),
                            valueBox(
                              value = "Step 2",
                              subtitle = "Download data using the Amadeus package and custom functions",
                              icon = icon("download"),
                              color = "yellow",
                              width = 4
                            ),
                            valueBox(
                              value = "Step 3",
                              subtitle = "Link & analyze results",
                              icon = icon("link"),
                              color = "green",
                              width = 4
                            )
                          ),
                          
                          
                          # Workflow demo images with arrows
                          fluidRow(
                            box(
                              title = "Example Workflow",
                              solidHeader = TRUE,
                              status = "primary",
                              width = 12,
                              align = "center",
                              HTML("
  <div style='display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 20px;'>
    
    <div style='text-align: center;'>
      <img src='step1.png' style='width: 100%; max-width: 100%%; height: auto;'><br>
      <span>Upload Data</span>
    </div>
    
    <i class='fa fa-arrow-down fa-2x' style='color: #555;'></i>
    
    <div style='text-align: center;'>
      <img src='step2.png' style='width: 100%; max-width: 100%; height: auto;'><br>
      <span>Download from Amadeus</span>
    </div>
    
    <i class='fa fa-arrow-down fa-2x' style='color: #555;'></i>
    
    <div style='text-align: center;'>
      <img src='step3.png' style='width: 100%; max-width: 100%; height: auto;'><br>
      <span>Link & Analyze</span>
    </div>
  
  </div>
")
                              
                            )
                          )
                        ),
                        
                        tabItem(tabName = "analysis",
                                tabsetPanel(id = "Tabs",
                                            tabPanel("Data Input",
                                                     fluidRow(
                                                       column(width = 4,
                                                              valueBoxOutput(outputId = 'recordsLoaded', width = NULL)
                                                       ),
                                                       column(width = 4,
                                                              valueBoxOutput(outputId = 'variablesLoaded', width = NULL)
                                                       ),
                                                       column(width = 4,
                                                              valueBoxOutput(outputId = 'dateRange', width = NULL)
                                                       )
                                                     ),
                                                     dataTableOutput("inputDisplay"),
                                                     leafletOutput("participantMap", height = "400px")
                                            ),
                                            tabPanel("Linked Data",
                                                     fluidRow(
                                                       column(width = 6,
                                                              valueBoxOutput(outputId = 'timeTaken', width = NULL)
                                                       ),
                                                       column(width = 6,
                                                              valueBoxOutput(outputId = 'variablesLinked', width = NULL)
                                                       ),
                                                     ),
                                                     
                                                     fluidRow(
                                                       column(width = 2,
                                                              div(class = "link-tab-box", align = 'center',
                                                                  box(width = NULL,
                                                                      solidHeader = TRUE,
                                                                      title = "Additional Manipulation",
                                                                      virtualSelectInput(inputId = "filterSelect", label = "Select Variables to Keep", choices = NULL,
                                                                                         multiple = TRUE, selected = NA),
                                                                      actionButton(inputId = "filterSelectButton", label = "Filter"),
                                                                      br(),
                                                                      hr(),
                                                                      uiOutput("dynamicUI2"),
                                                                      hr(),
                                                                      actionButton(inputId = "resetManipulatedData", label = "Reset Filters",
                                                                                   class = 'btn-primary btn-block btn-colored'),
                                                                      hr(),
                                                                      downloadButton(outputId = "downloadData", label = "Save Output",
                                                                                     class = 'btn-primary btn-block btn-colored')
                                                                      
                                                                  ),
                                                              )
                                                       ),
                                                       
                                                       column(width = 10,
                                                              div(class = 'link-tab-box',
                                                                  
                                                                  box(
                                                                    width = NULL,
                                                                    solidHeader = TRUE,
                                                                    title = "Linked Data",
                                                                    
                                                                    tabsetPanel(id = "output-tabs",
                                                                                tabPanel("Original Linked Data",
                                                                                         
                                                                                         dataTableOutput("linkDisplay")
                                                                                ),
                                                                                tabPanel("Manipulated Data",
                                                                                         dataTableOutput("manipulatedDisplay")
                                                                                )
                                                                                
                                                                    ),
                                                                    
                                                                    
                                                                  )
                                                              )
                                                       )
                                                     ),
                                                     
                                                     
                                            )
                                            
                                )
                        )
                      ),
                      # dashboardthemes::shinyDashboardThemes(theme = "poor_mans_flatly"),
                      useShinyjs(),
                      tags$head(
                        includeCSS("www/styles.css")
                      ),
                      
                    )
                    
                    
)

# Define server logic
server <- function(input, output, session) {
  
  shinyjs::hide("filterSelect")
  shinyjs::hide("filterSelectButton")
  
  updateSelectInput(inputId = "selectDatasetName", label = "Dataset Name",
                    choices = c("Smoke Plume (NOAA)" = "hms",
                                "Consortium National Land Cover (NLCD)" = "nlcd",
                                "North American Regional Reanalysis (NARR)" = "narr",
                                "Toxic Release Inventory (TRI)" = "tri",
                                "National Emissions Inventory (NEI)" = "nei",
                                "Modern-Era Retrospective Analysis (MERRA-2)" = "merra2",
                                "Global Roads (SEDAC)" = "sedac"),
                    selected = NA)
  
  rv = reactiveValues(df = NULL,
                      orig = NULL,
                      joined = NULL,
                      manipulated = NULL,
                      time_taken = NULL,
                      df_chem = NULL,
                      locs_sf = NULL,
                      current_observer = NULL,
                      inputs_df = NULL)
  
  
  
  
  # Display participant data
  observeEvent(input$fileInput, {
    print(input$fileInput$datapath)
    
    rv$df = read.csv(input$fileInput$datapath)
    
    potential_lon_ind = grep(pattern = "lon", x = names(rv$df), ignore.case = TRUE)[1]
    potential_lat_ind = grep(pattern = "lat", x = names(rv$df), ignore.case = TRUE)[1]
    potential_id_ind = grep(pattern = "id", x = names(rv$df), ignore.case = TRUE)[1]
    
    if(!is.na(potential_lon_ind)){
      potential_lon = names(rv$df)[potential_lon_ind]
    }else{
      potential_lon = names(rv$df)[1]
    }
    
    if(!is.na(potential_lat_ind)){
      potential_lat = names(rv$df)[potential_lat_ind]
    }else{
      potnetial_lat = names(rv$df)[1]
    }
    
    if(!is.na(potential_id_ind)){
      potential_id = names(rv$df)[potential_id_ind]
    }else{
      potential_id = names(rv$df)[1]
    }
    
    showModal(modalDialog(
      
      title = "Choose your lat/lon and ID variables",
      div(align = 'center',
          
          selectInput("lon_var", "Longitude Variable:", choices = names(rv$df), selected = potential_lon),
          selectInput("lat_var", "Latitude Variable:", choices = names(rv$df), selected = potential_lat),
          selectInput("id_var", "ID Variable:", choices = names(rv$df), selected = potential_id),
      ),
      
      
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_selection", "Confirm")
      )
      
    ))
    
    
  })
  
  
  observeEvent(input$confirm_selection, {
    removeModal()
    
    rv$df %>%
      rename("epr_number" = input$id_var,
             'gis_latitude' = input$lat_var,
             'gis_longitude' = input$lon_var)
    
    rv$df = rv$df %>%
      filter(!is.na(gis_latitude),
             !is.na(gis_longitude),
             gis_latitude != 0,
             gis_longitude != 0)
    
    output$inputDisplay = renderDataTable(datatable(rv$df, rownames = FALSE))
    
    output$participantMap <- renderLeaflet({
      
      leaflet(rv$df) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(
          lng = ~gis_longitude,
          lat = ~gis_latitude,
          radius = 5,
          color = "#007bff",
          fillColor = "#007bff",
          fillOpacity = 0.7,
          popup = ~paste("ID:", epr_number, "<br>",
                         "Lat:", round(gis_latitude, 4), "<br>",
                         "Lon:", round(gis_longitude, 4))
        ) %>%
        fitBounds(
          lng1 = ~min(gis_longitude, na.rm = TRUE),
          lat1 = ~min(gis_latitude, na.rm = TRUE),
          lng2 = ~max(gis_longitude, na.rm = TRUE),
          lat2 = ~max(gis_latitude, na.rm = TRUE)
        )
    })
    
  })
  
  output$recordsLoaded = renderValueBox({
    if(!is.null(rv$df)){
      valueBox(
        value = h1(nrow(rv$df)),
        subtitle = h2("Records Present"),
        icon = icon("database"),
        color = "purple"
      )
    }else{
      valueBox(
        value = h1("0"),
        subtitle = h2("Records Present"),
        icon = icon("database"),
        color = "purple"
      )
    }
    
  })
  
  output$variablesLoaded = renderValueBox({
    if(!is.null(rv$df)){
      valueBox(
        value = h1(ncol(rv$df)),
        subtitle = h2("Variables Present"),
        icon = icon("columns"),
        color = "purple"
      )
    }else{
      valueBox(
        value = h1("0"),
        subtitle = h2("Variables Present"),
        icon = icon("columns"),
        color = "purple"
      )
    }
  })
  
  output$dateRange = renderValueBox({
    valueBox(
      value = h1(paste0(year(input$dateRange[1]), "-", year(input$dateRange[2]))),
      subtitle = h2("Date Range"),
      icon = icon("calendar-alt"),
      color = "purple"
    )
  })
  
  output$timeTaken = renderValueBox({
    if(!is.null(rv$joined)){
      valueBox(
        value = h1(paste0(rv$time_taken, " mins")),
        subtitle = h2("Time Taken"),
        icon = icon("stopwatch"), 
        color = "purple" 
      )
    }else{
      valueBox(
        value = h1("0"),
        subtitle = h2("Time Taken"),
        icon = icon("stopwatch"), 
        color = "purple" 
      )
    }
    
  })
  
  output$variablesLinked = renderValueBox({
    if(!is.null(rv$joined)){
      valueBox(
        value = h1(ncol(rv$joined)),
        subtitle = h2("Variables Present"),
        icon = icon("link"), 
        color = "purple" 
      )
    }else{
      valueBox(
        value = h1("0"),
        subtitle = h2("Variables Present"),
        icon = icon("link"), 
        color = "purple" 
      )
    }
    
  })
  
  
  # Grab data and link to participants
  observeEvent(input$downloadSelected, {
    
    
    dynamicButton(input, output, server, rv, session)
    
    if(!is.null(rv$joined)){
      
      shinyjs::show("filterSelect")
      shinyjs::show("filterSelectButton")
      updateVirtualSelect(inputId = "filterSelect", label = "Select Variables to Keep",
                          choices = names(rv$joined), selected = NA)
      output$dynamicUI2 = renderUI(dynamicUI2(input, output, server, rv, session))
    }
    
    
    
  })
  
  
  observeEvent(input$checkAvailableData, {
    observeEvent(input$checkAvailableData, {
      runjs("window.open('https://goldsmr4.gesdisc.eosdis.nasa.gov/data/', '_blank')")
    })
  })
  
  observeEvent(input$selectDatasetName, {
    if(input$sidebarTabs == "analysis"){
      print('trying to open modules dir')
      source(paste0("modules/",input$selectDatasetName,"UI.R"))
      output$dynamicUI = renderUI(dynamicUI())
      
    }
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$dateRange, {
    print(input$dateRange)
  })
  
  # Interactive map
  
  
  observeEvent(input$selectYearNEI, {
    file_names = list.files(path = paste0("../for_host/",input$selectYearNEI), pattern = "\\.rds")
    file_names = str_match(string = file_names, pattern = "(.*?)\\.rds")[,2]
    
    updateVirtualSelect(inputId = "fileNameNEI", choices = c(file_names))
    
  })
  
  
  observeEvent(input$confirm_chems, {
    CloseModalFunction(input, output, server, rv, session)
  })
  
  observeEvent(input$filterSelectButton, {
    rv$manipulated = rv$joined %>%
      dplyr::select(input$filterSelect)
    
    
  })
  
  observeEvent(input$dynamicManipulateButton, {
    
    dynamicManipulateButton(input, output, server, rv, session)
    
  })
  
  observeEvent(input$resetManipulatedData, {
    rv$manipulated = rv$joined
  })
  
  
  observeEvent(input$configInput, {
    config_file = read.xlsx(input$configInput$datapath, sheet = 3)
    
    for(i in 1:nrow(config_file)){
      if(config_file$group[i] == 1){
        if(config_file$type[i] == "selectInput"){
          updateSelectInput(inputId = config_file$inputId[i], selected = config_file$value[i])
        }else if(config_file$type[i] == "virtualSelectInput"){
          updateVirtualSelect(inputId = config_file$inputId[i], selected = config_file$value[i])
        }
      }
    }
    
    
  })
  
  # Custom download ZIP handler
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste0("mydata_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tmp_dir <- tempdir()
      
      if(!is.null(input$filterSelect)){
        tmp = data.frame(inputId = "filterSelect",
                         type = "virtualSelectInput",
                         value = paste(input$filterSelect, collapse = ", "),
                         group = 2)
        
        rv$inputs_df = bind_rows(rv$inputs_df, tmp)
      }
      
      
      # 1. Write manipulated_data as CSV
      csv_path <- file.path(tmp_dir, "manipulated_data.csv")
      write.csv(rv$manipulated, csv_path, row.names = FALSE)
      
      
      # 2. Create an Excel file with 3 sheets
      xlsx_path <- file.path(tmp_dir, "other_data.xlsx")
      wb <- createWorkbook()
      addWorksheet(wb, "linked_data");   writeData(wb, "linked_data", rv$joined)
      addWorksheet(wb, "original_data"); writeData(wb, "original_data", rv$df)
      addWorksheet(wb, "inputs");        writeData(wb, "inputs", rv$inputs_df)
      saveWorkbook(wb, xlsx_path, overwrite = TRUE)
      
      # zip it
      zip::zipr(
        zipfile = file,
        files = c(csv_path, xlsx_path),
        recurse = FALSE
      )
      
    },
    contentType = "application/zip"
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
