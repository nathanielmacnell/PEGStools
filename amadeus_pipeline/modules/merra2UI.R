library(earthdatalogin)
library(terra)
library(sf)
library(raster)

source('functions/merra2_funcs.R')


dynamicUI <- function() {
  tagList(
    
    virtualSelectInput(inputId = 'selectCollection', label = "Select Collection",
                       choices = c("inst1_2d_asm_Nx", "inst1_2d_int_Nx", "inst1_2d_lfo_Nx",
                                   "inst3_3d_asm_Np", "inst3_3d_aer_Nv", "inst3_3d_asm_Nv", "inst3_3d_chm_Nv",
                                   "inst3_3d_gas_Nv", "inst3_2d_gas_Nx", "inst6_3d_ana_Np", "inst6_3d_ana_Nv",
                                   "statD_2d_slv_Nx", "tavg1_2d_adg_Nx", "tavg1_2d_aer_Nx", "tavg1_2d_chm_Nx",
                                   "tavg1_2d_csp_Nx", "tavg1_2d_flx_Nx", "tavg1_2d_int_Nx", "tavg1_2d_lfo_Nx",
                                   "tavg1_2d_lnd_Nx", "tavg1_2d_ocn_Nx", "tavg1_2d_rad_Nx", "tavg1_2d_slv_Nx",
                                   "tavg3_3d_mst_Ne", "tavg3_3d_trb_Ne", "tavg3_3d_nav_Ne", "tavg3_3d_cld_Np", 
                                   "tavg3_3d_mst_Np", "tavg3_3d_rad_Np", "tavg3_3d_tdt_Np", "tavg3_3d_trb_Np",
                                   "tavg3_3d_udt_Np", "tavg3_3d_odt_Np", "tavg3_3d_qdt_Np", "tavg3_3d_asm_Nv",
                                   "tavg3_3d_cld_Nv", "tavg3_3d_mst_Nv", "tavg3_3d_rad_Nv", "tavg3_2d_glc_Nx"),
                       multiple = FALSE),
    dateRangeInput(inputId = "selectDateRange", label = "Select Date Range",
                   min = "1980-01-01", max = "2025-01-01",
                   start = "2024-01-01", end = "2024-01-01")
    # actionButton(inputId = "checkAvailableData", label = "Check Available Data")
    # textInput(inputId = "dataName", "Input Dataset Name", value = "MERRA2_100.tavgM_2d_slv_Nx.198101.nc4")
    
  )
}

dynamicButton <- function(input, output, server, rv, session){
  
  t1 = Sys.time()
  shinybusy::show_modal_spinner(spin = "semipolar", text = "Downloading and linking...")
  
  download_merra2_check(
    # collection = "inst1_2d_int_Nx",
    collection = input$selectCollection,
    date = input$selectDateRange,
    # date = c("2024-01-01"),
    directory_to_save = "data/",
    acknowledgement = TRUE,
    download = TRUE
    )
  
  # locs = epr.gis
  # names(locs) = c("id", "lon", "lat")
  
  if(length(input$selectDateRange) == 1){
    dates = c(input$selectDateRange, input$selectDateRange)
  }
  
  dates = as.Date(input$selectDateRange)
  # dates = as.Date(c("2024-01-01", "2024-01-01"))
  
  date_range = seq.Date(from = dates[1], to = dates[2], by = "day")
  date_range = gsub(pattern = "-", x = date_range, replacement = "")
  
  base_name = paste0('MERRA2_400.',input$selectCollection,'.')
  # base_name = paste0('MERRA2_400.',"inst1_2d_int_Nx",'.')
  
  tmp_names = paste0(base_name, date_range, ".nc4")
  
  files_of_interest = list.files(paste0("data/",input$selectCollection))
  # files_of_interest = list.files(paste0("data/","inst1_2d_int_Nx"))
  files_of_interest = files_of_interest[files_of_interest %in% tmp_names]
  files_of_interest = paste0("data/",input$selectCollection,"/", files_of_interest)
  # files_of_interest = paste0("data/","inst1_2d_int_Nx","/", files_of_interest)
  
  r = terra::rast(files_of_interest)
  # nc = nc_open(files_of_interest)
  # cpt_var <- nc$var$CPT
  # cpt_var$units
  # cpt_var$longname
  
  
  # Get the full list of names
  nms <- names(r)
  
  # Find unique layer names (that are repeated)
  unique_nms <- unique(nms)
  
  # For each unique name, average all layers with that name
  avg_list <- lapply(unique_nms, function(n) {
    mean(r[[which(nms == n)]], na.rm = TRUE)
  })
  
  # Combine into a single SpatRaster
  r_avg <- rast(avg_list)
  names(r_avg) <- unique_nms
  
  # Convert to sf points
  participants_sf <- st_as_sf(rv$df, coords = c("gis_longitude", "gis_latitude"), crs = 4326)
  # participants_sf <- st_as_sf(epr.gis, coords = c("gis_longitude", "gis_latitude"), crs = 4326)
  
  # participants_sf <- st_as_sf(locs, coords = c('lon','lat'), crs = 4326)
  
  # 3. Ensure CRS match between raster and points (reproject points if needed)
  if (!compareCRS(r, participants_sf)) {
    participants_sf <- st_transform(participants_sf, crs(r_avg))
  }
  
  # 4. Extract raster values at participant points
  vals <- terra::extract(r_avg, vect(participants_sf))
  
  # 5. Combine extracted values with participant data
  result <- cbind(rv$df, vals)
  
  rv$joined = result %>%
    st_drop_geometry()
  
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