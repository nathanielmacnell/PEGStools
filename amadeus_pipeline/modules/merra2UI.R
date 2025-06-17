dynamicUI <- function() {
  tagList(
    selectInput(inputId = "selectCollection", label = "Collection",
                choices = c("inst1_2d_asm_Nx", "inst1_2d_int_Nx", "inst1_2d_lfo_Nx",
                            "inst3_3d_asm_Np", "inst3_3d_aer_Nv", "inst3_3d_asm_Nv", "inst3_3d_chm_Nv",
                            "inst3_3d_gas_Nv", "inst3_2d_gas_Nx", "inst6_3d_ana_Np", "inst6_3d_ana_Nv",
                            "statD_2d_slv_Nx", "tavg1_2d_adg_Nx", "tavg1_2d_aer_Nx", "tavg1_2d_chm_Nx",
                            "tavg1_2d_csp_Nx", "tavg1_2d_flx_Nx", "tavg1_2d_int_Nx", "tavg1_2d_lfo_Nx",
                            "tavg1_2d_lnd_Nx", "tavg1_2d_ocn_Nx", "tavg1_2d_rad_Nx", "tavg1_2d_slv_Nx",
                            "tavg3_3d_mst_Ne", "tavg3_3d_trb_Ne", "tavg3_3d_nav_Ne", "tavg3_3d_cld_Np", 
                            "tavg3_3d_mst_Np", "tavg3_3d_rad_Np", "tavg3_3d_tdt_Np", "tavg3_3d_trb_Np",
                            "tavg3_3d_udt_Np", "tavg3_3d_odt_Np", "tavg3_3d_qdt_Np", "tavg3_3d_asm_Nv",
                            "tavg3_3d_cld_Nv", "tavg3_3d_mst_Nv", "tavg3_3d_rad_Nv", "tavg3_2d_glc_Nx")),
    dateRangeInput(inputId = 'dateRange', label = "Select Date Range", min = "1990-01-01", max = Sys.Date(),
                   start = "2022-01-01", end = "2022-01-05")

  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  download_merra2(
    collection = "inst1_2d_int_Nx",
    date = "2024-01-01",
    directory_to_save = "data/",
    acknowledgement = TRUE,
    download = FALSE, # NOTE: download skipped for examples,
    remove_command = FALSE,
  )
  
  # Download data
  directory <- "data/"
  download_merra2(
    collection = "inst1_2d_asm_Nx",
    date = c("2022-01-01", "2022-01-05"),
    directory_to_save = tempdir(),
    acknowledgement = TRUE,
    download = FALSE, # NOTE: download skipped for examples,
    remove_command = TRUE,
  )
  
  # Read the downloaded data into R spatrast
  
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
    radius = 0,
    geom = 'sf'
  )
  
  rv$joined = joined %>%
    st_drop_geometry()
  
  output$linkDisplay = renderDataTable(datatable(rv$joined, rownames = FALSE),
                                       class = 'table table-striped table-hover table-dark')
  
  shinybusy::remove_modal_spinner()
  
  shinyalert::shinyalert(title = "Success!",
                         text = "Check your linked data on the 'Linked Data' tab!",
                         type = "success")
  
  t2 = Sys.time()
  rv$time_taken = round(t2 - t1, 2)
  updateTabItems(session, inputId = "Tabs", selected = "Linked Data")
  
  
  
  
}