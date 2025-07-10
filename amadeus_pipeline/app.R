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
library(bs4Dash)
library(leaflet)
library(shinyjs)
library(tigris)
library(stringr)
library(httr)
library(rvest)

# Get map data
states <- map_data("state")

# Get available data options
available_data = list.files('modules', full.names = FALSE)
available_data = str_match(available_data, "(.*?)UI")[,2]

# Only load smoke plume data/narr functionality for now

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Amadeus Pipeline", status = "primary"),
  dashboardSidebar( width = "400px",
                    div(class="file-input",
                        fileInput(inputId = "fileInput", label = "Upload Participant Data"),
                    ),
                    
                    selectInput(inputId = "selectDatasetName", label = "Dataset Name",
                                choices = c("")),
                    uiOutput("dynamicUI"),
                    actionButton(inputId = 'downloadSelected', label = 'Download and Link', class = 'btn-primary')
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      includeCSS("www/styles.css")
    ),
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
                         dataTableOutput("linkDisplay")
                )
    )
  )
  
  
)

# Define server logic
server <- function(input, output, session) {
  
  updateSelectInput(inputId = "selectDatasetName", label = "Dataset Name",
                    choices = c("Smoke Plume (NOAA)" = "hms",
                                "Consortium National Land Cover (NLCD)" = "nlcd",
                                "North American Regional Reanalysis (NARR)" = "narr",
                                "Toxic Release Inventory (TRI)" = "tri",
                                "National Emissions Inventory (NEI)" = "nei",
                                "Modern-Era Retrospective Analysis (MERRA-2)" = "merra2",
                                "Global Roads (SEDAC)" = "sedac"))
  
  rv = reactiveValues(df = NULL,
                      orig = NULL,
                      joined = NULL,
                      time_taken = NULL)
  
  # Display participant data
  observeEvent(input$fileInput, {
    print(input$fileInput$datapath)
    
    rv$df = read.csv(input$fileInput$datapath)
    
    output$inputDisplay = renderDataTable(datatable(rv$df, rownames = FALSE))
    
  })
  
  output$recordsLoaded = renderbs4ValueBox({
    if(!is.null(rv$df)){
      bs4ValueBox(
        value = h1(nrow(rv$df)),
        subtitle = h2("Records Present"),
        color = "primary"
      )
    }else{
      bs4ValueBox(
        value = h1("0"),
        subtitle = h2("Records Present"),
        color = "primary"
      )
    }
    
  })
  
  output$variablesLoaded = renderbs4ValueBox({
    if(!is.null(rv$df)){
      bs4ValueBox(
        value = h1(ncol(rv$df)),
        subtitle = h2("Variables Present"),
        color = "primary"
      )
    }else{
      bs4ValueBox(
        value = h1("0"),
        subtitle = h2("Variables Present"),
        color = "primary"
      )
    }
  })
  
  output$dateRange = renderbs4ValueBox({
    bs4ValueBox(
      value = h1(paste0(year(input$dateRange[1]), "-", year(input$dateRange[2]))),
      subtitle = h2("Date Range"),
      color = "primary"
    )
  })
  
  output$timeTaken = renderbs4ValueBox({
    if(!is.null(rv$joined)){
      bs4ValueBox(
        value = h1(paste0(rv$time_taken, " Seconds")),
        subtitle = h2("Time Taken"),
        color = "primary" 
      )
    }else{
      bs4ValueBox(
        value = h1("0"),
        subtitle = h2("Time Taken"),
        color = "primary" 
      )
    }
    
  })
  
  output$variablesLinked = renderbs4ValueBox({
    if(!is.null(rv$joined)){
      bs4ValueBox(
        value = h1(ncol(rv$joined)),
        subtitle = h2("Variables Present"),
        color = "primary" 
      )
    }else{
      bs4ValueBox(
        value = h1("0"),
        subtitle = h2("Variables Present"),
        color = "primary" 
      )
    }
    
  })
  
  
  # Grab data and link to participants
  observeEvent(input$downloadSelected, {
    
    dynamicButton(input, output, server, rv, session)
    
  })
  
  observeEvent(input$checkAvailableData, {
    observeEvent(input$checkAvailableData, {
      runjs("window.open('https://goldsmr4.gesdisc.eosdis.nasa.gov/data/', '_blank')")
    })
  })
  
  observeEvent(input$selectDatasetName, {
    source(paste0("modules/",input$selectDatasetName,"UI.R"))
    
    output$dynamicUI = renderUI(dynamicUI())
  }, ignoreInit = TRUE)
  
  observeEvent(input$dateRange, {
    print(input$dateRange)
  })
  
  # Interactive map
  output$participantMap <- renderLeaflet({
    req(rv$df)
    
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
  
  observeEvent(input$selectYearNEI, {
    file_names = list.files(path = paste0("../for_host/",input$selectYearNEI), pattern = "\\.rds")
    file_names = str_match(string = file_names, pattern = "(.*?)\\.rds")[,2]
    
    updateVirtualSelect(inputId = "fileNameNEI", choices = c(file_names))
    
  })
  
  observeEvent(input$selectChemicalNEI, {
    rv$joined = rv$orig %>%
      filter(description %in% input$selectChemicalNEI)
  })

  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
