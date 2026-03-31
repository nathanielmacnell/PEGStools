dynamicUI <- function() {
  tagList(
    selectInput(inputId = "selectYearNEI", label = "Select Year", choices = c(2008, 2011, 2014, 2017, 2020, 2023),
                multiple = FALSE),
    virtualSelectInput(inputId = "fileNameNEI", label = "File Name",choices = NULL),
    
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Loading Data...")
  
  df = readRDS(paste0("../for_host/",input$selectYearNEI,"/",input$fileNameNEI,".rds"))
  # df = readRDS(paste0("for_host/2020/for_host_2020_2020nei_onroad_byregion.rds"))
  
  
  if(input$selectYearNEI == 2008 | input$selectYearNEI == 2011){
    df = df %>%
      rename("GEOID" = "state_and_county_fips_code") %>%
      mutate("GEOID" = as.character(GEOID)) %>%
      mutate(GEOID = str_pad(string = GEOID, width = 5, side = "left", pad = "0")) %>%
      group_by(GEOID, pollutant_cd, description) %>%
      summarise(sum_emissions = sum(total_emissions))
  }else if(input$selectYearNEI == 2014){
    df = df %>%
      rename("GEOID" = "state_and_county_fips_code") %>%
      mutate("GEOID" = as.character(GEOID)) %>%
      mutate(GEOID = str_pad(string = GEOID, width = 5, side = "left", pad = "0")) %>%
      group_by(GEOID, pollutant_cd, pollutant_desc) %>%
      summarise(sum_emissions = sum(total_emissions))
  }else if(input$selectYearNEI == 2017 | input$selectYearNEI == 2020){
    rename("GEOID" = "fips.code") %>%
      mutate("GEOID" = as.character(GEOID)) %>%
      mutate(GEOID = str_pad(string = GEOID, width = 5, side = "left", pad = "0")) %>%
      group_by(GEOID, pollutant.code, pollutant.desc) %>%
      summarise(sum_emissions = sum(total.emissions))
  }
  
  rv$df_chem = df
  
  if(input$selectYearNEI < 2014){
    county_year = 2013
  }else{
    couty_year = input$selectYearNEI
  }
  # counties = tigris::counties(cb = TRUE, year = input$selectYear, class = 'sf')
  counties = tigris::counties(cb = TRUE, year = county_year, class = 'sf')
  
  
  # locs = epr.gis
  # names(locs) = c("id", "lon", "lat")
  locs <- data.frame(id = rv$df$epr_number, lon = rv$df$gis_longitude, lat = rv$df$gis_latitude)
  # locs <- data.frame(id = epr.gis$epr_number, lon = epr.gis$gis_longitude, lat = epr.gis$gis_latitude)
  
  
  
  
  locs_sf = st_as_sf(locs, coords = c("lon", "lat"), crs = st_crs(counties))
  
  rv$locs_sf = locs_sf
  
  shinybusy::remove_modal_spinner()
  
  if(input$selectYearNEI %in% c(2008, 2011)){
    showModal(modalDialog(
      title = "Choose Chemicals of Interest",
      virtualSelectInput(inputId = "selectChemicalNEI", label = "Select Chemicals of Interest",
                         choices = unique(df$description), selected = unique(df$description),
                         multiple = TRUE, search = TRUE, disableSelectAll = FALSE),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_chems", "Confirm")
      )
    ))
  }else if(input$selectYearNEI == 2014){
    showModal(modalDialog(
      title = "Choose Chemicals of Interest",
      virtualSelectInput(inputId = "selectChemicalNEI", label = "Select Chemicals of Interest",
                         choices = unique(df$pollutant_desc), selected = unique(df$pollutant_desc),
                         multiple = TRUE, search = TRUE, disableSelectAll = FALSE),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_chems", "Confirm")
      )
    ))
  }else if(input$selectYearNEI %in% c(2017, 2020)){
    showModal(modalDialog(
      title = "Choose Chemicals of Interest",
      virtualSelectInput(inputId = "selectChemicalNEI", label = "Select Chemicals of Interest",
                         choices = unique(df$pollutant.desc), selected = unique(df$pollutant.desc),
                         multiple = TRUE, search = TRUE, disableSelectAll = FALSE),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_chems", "Confirm")
      )
    ))
  }
  
}

CloseModalFunction <- function(input, output, server, rv, session){
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")

  if(input$selectYearNEI %in% c(2008, 2011)){
    rv$df_chem = rv$df_chem %>%
      filter(description %in% input$selectChemicalNEI)
  }else if(input$selectYearNEI == 2014){
    rv$df_chem = rv$df_chem %>%
      filter(pollutant_desc %in% input$selectChemicalNEI)
  }else if(input$selectYearNEI %in% c(2017, 2020)){
    rv$df_chem = rv$df_chem %>%
      filter(pollutant.desc %in% input$selectChemicalNEI)
  }
  
  joined_participants = st_join(rv$locs_sf, counties[,c('GEOID')])
  
  joined_participants_2 = joined_participants %>%
    left_join(rv$df_chem)
  
  # Join data to participants
  
  
  rv$joined = joined_participants_2 %>%
    st_drop_geometry()
  rv$orig = rv$joined
  
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