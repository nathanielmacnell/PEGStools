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

# Get map data
states <- map_data("state")

# Get available data options
available_data = list.files('modules', full.names = FALSE)
available_data = str_match(available_data, "(.*?)UI")[,2]

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme(theme = 'flatly'),
  sidebarLayout(
    sidebarPanel(
      h1("Amadeus Pipeline"),
      fileInput(inputId = "fileInput", label = "Upload Participant Data"),
      selectInput(inputId = "selectDatasetName", label = "Dataset Name",
                  choices = c("")),
      uiOutput("dynamicUI"),
      actionButton(inputId = 'downloadSelected', label = 'Download and Link', class = 'btn-primary')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Input",
                 dataTableOutput("inputDisplay"),
                 plotOutput("participantDisplay")
        ),
        tabPanel("Linked Data",
                 dataTableOutput("linkDisplay")
                 )
      )
    )
  )
  
)

# Define server logic
server <- function(input, output) {
  
  updateSelectInput(inputId = "selectDatasetName", label = "Dataset Name",
                    choices = available_data)
  
  rv = reactiveValues(df = NULL)
  
  # Display participant data
  observeEvent(input$fileInput, {
    print(input$fileInput$datapath)
    
    load(input$fileInput$datapath)
    
    rv$df = epr.gis
    
    output$inputDisplay = renderDataTable(datatable(epr.gis, rownames = FALSE, style = 'bootstrap'))
    
    output$participantDisplay = renderPlot(
      ggplot(states, aes(x = long, y = lat, group = group)) +
        geom_polygon(fill = "white", color = "black") +
        coord_fixed(1.3) +
        theme_void() +
        labs(title = "Continental US State Lines") +
        geom_point(data = epr.gis, aes(x = gis_longitude, y = gis_latitude),
                   inherit.aes = FALSE, shape = '.', color = 'blue')
    )
    
  })
  
  
  # Grab data and link to participants
  observeEvent(input$downloadSelected, {
    
    shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
    
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
    
    output$linkDisplay = renderDataTable(datatable(weasd_covar, style = 'bootstrap', rownames = FALSE))
    
    shinybusy::remove_modal_spinner()
    
    shinyalert::shinyalert(title = "Success!",
                           text = "Check your linked data on the 'Linked Data' tab!",
                           type = "success")
    
  })
  
  observeEvent(input$selectDatasetName, {
    source(paste0("modules/",input$selectDatasetName,"UI.R"))
    
    output$dynamicUI = renderUI(dynamicUI())
  }, ignoreInit = TRUE)
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
