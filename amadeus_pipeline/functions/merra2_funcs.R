download_merra2_check <- function(
    collection = c(
      "inst1_2d_asm_Nx",
      "inst1_2d_int_Nx",
      "inst1_2d_lfo_Nx",
      "inst3_3d_asm_Np",
      "inst3_3d_aer_Nv",
      "inst3_3d_asm_Nv",
      "inst3_3d_chm_Nv",
      "inst3_3d_gas_Nv",
      "inst3_2d_gas_Nx",
      "inst6_3d_ana_Np",
      "inst6_3d_ana_Nv",
      "statD_2d_slv_Nx",
      "tavg1_2d_adg_Nx",
      "tavg1_2d_aer_Nx",
      "tavg1_2d_chm_Nx",
      "tavg1_2d_csp_Nx",
      "tavg1_2d_flx_Nx",
      "tavg1_2d_int_Nx",
      "tavg1_2d_lfo_Nx",
      "tavg1_2d_lnd_Nx",
      "tavg1_2d_ocn_Nx",
      "tavg1_2d_rad_Nx",
      "tavg1_2d_slv_Nx",
      "tavg3_3d_mst_Ne",
      "tavg3_3d_trb_Ne",
      "tavg3_3d_nav_Ne",
      "tavg3_3d_cld_Np",
      "tavg3_3d_mst_Np",
      "tavg3_3d_rad_Np",
      "tavg3_3d_tdt_Np",
      "tavg3_3d_trb_Np",
      "tavg3_3d_udt_Np",
      "tavg3_3d_odt_Np",
      "tavg3_3d_qdt_Np",
      "tavg3_3d_asm_Nv",
      "tavg3_3d_cld_Nv",
      "tavg3_3d_mst_Nv",
      "tavg3_3d_rad_Nv",
      "tavg3_2d_glc_Nx"
    ),
    date = c("2018-01-01", "2018-01-01"),
    directory_to_save = NULL,
    acknowledgement = FALSE,
    download = FALSE,
    remove_command = FALSE,
    hash = FALSE
) {
  collection = "inst1_2d_int_Nx"
  date = c("2024-01-01", '2024-01-03')
  directory_to_save = "data/"
  acknowledgement = TRUE
  download = TRUE # NOTE: download skipped for examples,
  remove_command = FALSE
  
  print('hi')
  #### check for data download acknowledgement
  amadeus::download_permit(acknowledgement = acknowledgement)
  #### directory setup
  amadeus::download_setup_dir(directory_to_save)
  directory_to_save <- amadeus::download_sanitize_path(directory_to_save)
  #### check dates
  if (length(date) == 1) date <- c(date, date)
  stopifnot(length(date) == 2)
  date <- date[order(as.Date(date))]
  #### check for null parameters
  amadeus::check_for_null_parameters(mget(ls()))
  #### check if collection is recognized
  identifiers <- c(
    "inst1_2d_asm_Nx M2I1NXASM 10.5067/3Z173KIE2TPD",
    "inst1_2d_int_Nx M2I1NXINT 10.5067/G0U6NGQ3BLE0",
    "inst1_2d_lfo_Nx M2I1NXLFO 10.5067/RCMZA6TL70BG",
    "inst3_3d_asm_Np M2I3NPASM 10.5067/QBZ6MG944HW0",
    "inst3_3d_aer_Nv M2I3NVAER 10.5067/LTVB4GPCOTK2",
    "inst3_3d_asm_Nv M2I3NVASM 10.5067/WWQSXQ8IVFW8",
    "inst3_3d_chm_Nv M2I3NVCHM 10.5067/HO9OVZWF3KW2",
    "inst3_3d_gas_Nv M2I3NVGAS 10.5067/96BUID8HGGX5",
    "inst3_2d_gas_Nx M2I3NXGAS 10.5067/HNGA0EWW0R09",
    "inst6_3d_ana_Np M2I6NPANA 10.5067/A7S6XP56VZWS",
    "inst6_3d_ana_Nv M2I6NVANA 10.5067/IUUF4WB9FT4W",
    "statD_2d_slv_Nx M2SDNXSLV 10.5067/9SC1VNTWGWV3",
    "tavg1_2d_adg_Nx M2T1NXADG 10.5067/HM00OHQBHKTP",
    "tavg1_2d_aer_Nx M2T1NXAER 10.5067/KLICLTZ8EM9D",
    "tavg1_2d_chm_Nx M2T1NXCHM 10.5067/3RQ5YS674DGQ",
    "tavg1_2d_csp_Nx M2T1NXCSP 10.5067/H0VVAD8F6MX5",
    "tavg1_2d_flx_Nx M2T1NXFLX 10.5067/7MCPBJ41Y0K6",
    "tavg1_2d_int_Nx M2T1NXINT 10.5067/Q5GVUVUIVGO7",
    "tavg1_2d_lfo_Nx M2T1NXLFO 10.5067/L0T5GEG1NYFA",
    "tavg1_2d_lnd_Nx M2T1NXLND 10.5067/RKPHT8KC1Y1T",
    "tavg1_2d_ocn_Nx M2T1NXOCN 10.5067/Y67YQ1L3ZZ4R",
    "tavg1_2d_rad_Nx M2T1NXRAD 10.5067/Q9QMY5PBNV1T",
    "tavg1_2d_slv_Nx M2T1NXSLV 10.5067/VJAFPLI1CSIV",
    "tavg3_3d_mst_Ne M2T3NEMST 10.5067/JRUZ3SJ3ZJ72",
    "tavg3_3d_trb_Ne M2T3NETRB 10.5067/4I7ZI35QRH8K",
    "tavg3_3d_nav_Ne M2T3NENAV 10.5067/N5WAKNS1UYQN",
    "tavg3_3d_cld_Np M2T3NPCLD 10.5067/TX10URJSKT53",
    "tavg3_3d_mst_Np M2T3NPMST 10.5067/0TUFO90Q2PMS",
    "tavg3_3d_rad_Np M2T3NPRAD 10.5067/3UGE8WQXZAOK",
    "tavg3_3d_tdt_Np M2T3NPTDT 10.5067/9NCR9DDDOPFI",
    "tavg3_3d_trb_Np M2T3NPTRB 10.5067/ZRRJPGWL8AVL",
    "tavg3_3d_udt_Np M2T3NPUDT 10.5067/CWV0G3PPPWFW",
    "tavg3_3d_odt_Np M2T3NPODT 10.5067/S0LYTK57786Z",
    "tavg3_3d_qdt_Np M2T3NPQDT 10.5067/A9KWADY78YHQ",
    "tavg3_3d_asm_Nv M2T3NVASM 10.5067/SUOQESM06LPK",
    "tavg3_3d_cld_Nv M2T3NVCLD 10.5067/F9353J0FAHIH",
    "tavg3_3d_mst_Nv M2T3NVMST 10.5067/ZXTJ28TQR1TR",
    "tavg3_3d_rad_Nv M2T3NVRAD 10.5067/7GFQKO1T43RW",
    "tavg3_2d_glc_Nx M2T3NXGLC 10.5067/9ETB4TT5J6US"
  )
  identifiers <- lapply(identifiers, strsplit, split = " ")
  identifiers <- lapply(identifiers, function(x) matrix(x[[1]], nrow = 1))
  identifiers <- do.call(rbind, identifiers)
  identifiers_df <- as.data.frame(identifiers)
  colnames(identifiers_df) <- c("collection_id", "estd_name", "DOI")
  if (!all(collection %in% identifiers_df$collection_id)) {
    message(identifiers_df)
    stop(paste0(
      "Requested collection is not recognized.\n
    Please refer to the table above to find a proper collection.\n"
    ))
  }
  #### define date sequence
  date_sequence <- amadeus::generate_date_sequence(
    date[1],
    date[2],
    sub_hyphen = TRUE
  )
  #### define year + month sequence
  yearmonth_sequence <- unique(substr(date_sequence, 1, 6))
  #### initiate "..._wget_commands.txt" file
  commands_txt <- paste0(
    directory_to_save,
    "merra2_",
    date[1],
    "_",
    date[2],
    "_wget_commands.txt"
  )
  # amadeus::download_sink(commands_txt)
  for (c in seq_along(collection)) {
    c = 1
    collection_loop <- collection[c]
    #### define ESDT name and DOI
    identifiers_df_requested <- subset(
      identifiers_df,
      subset = identifiers_df$collection_id == collection_loop
    )
    esdt_name <- identifiers_df_requested[, 2]
    #### define URL base
    #### NOTE: sorted and defined manually according to
    ####       https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/ \&
    ####       https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/
    esdt_name_4 <- c(
      "M2I1NXASM",
      "M2I1NXINT",
      "M2I1NXLFO",
      "M2I3NXGAS",
      "M2SDNXSLV",
      "M2T1NXADG",
      "M2T1NXAER",
      "M2T1NXCHM",
      "M2T1NXCSP",
      "M2T1NXFLX",
      "M2T1NXINT",
      "M2T1NXLFO",
      "M2T1NXLND",
      "M2T1NXOCN",
      "M2T1NXRAD",
      "M2T1NXSLV",
      "M2T3NXGLC"
    )
    esdt_name_5 <- c(
      "M2I3NPASM",
      "M2I3NVAER",
      "M2I3NVASM",
      "M2I3NVCHM",
      "M2I3NVGAS",
      "M2I6NPANA",
      "M2I6NVANA",
      "M2T3NEMST",
      "M2T3NENAV",
      "M2T3NETRB",
      "M2T3NPCLD",
      "M2T3NPMST",
      "M2T3NPODT",
      "M2T3NPQDT",
      "M2T3NPRAD",
      "M2T3NPTDT",
      "M2T3NPTRB",
      "M2T3NPUDT",
      "M2T3NVASM",
      "M2T3NVCLD",
      "M2T3NVMST",
      "M2T3NVRAD"
    )
    if (esdt_name %in% esdt_name_4) {
      base <- "https://goldsmr4.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    } else if (esdt_name %in% esdt_name_5) {
      base <- "https://goldsmr5.gesdisc.eosdis.nasa.gov/data/MERRA2/"
    }
    #### identify download URLs
    list_urls <- NULL
    for (y in seq_along(yearmonth_sequence)) {
      year <- substr(yearmonth_sequence[y], 1, 4)
      month <- substr(yearmonth_sequence[y], 5, 6)
      if (y == 1) {
        base_url <- paste0(
          base,
          esdt_name,
          ".5.12.4/",
          year,
          "/",
          month,
          "/"
        )
        if (!(amadeus::check_url_status(base_url))) {
          stop(paste0(
            "Invalid date returns HTTP code 404. ",
            "Check `date` parameter.\n"
          ))
        }
      }
      # list_urls_month <- system(
      #   paste0(
      #     "wget -q -nH -nd ",
      #     "\"",
      #     base,
      #     esdt_name,
      #     ".5.12.4/",
      #     year,
      #     "/",
      #     month,
      #     "/\"",
      #     " -O - | grep .nc4 | awk -F'\"' ",
      #     "'{print $4}'"
      #   ),
      #   intern = TRUE
      # )
      
      url = paste0(
        base,
        esdt_name,
        ".5.12.4/",
        year,
        "/",
        month,
        "/"
      )
      cookie_path <- ".urs_cookies"  # Or give full path if needed
      
      # Perform authenticated GET request using stored cookies
      resp <- httr::GET(
        url,
        config(cookiefile = cookie_path, followlocation = TRUE)
      )
      
      # Check that the request succeeded
      httr::stop_for_status(resp)
      
      # Parse HTML content
      page <- httr::content(resp, as = "parsed")
      
      # Extract hrefs from <a> tags
      list_urls_month <- page %>%
        html_elements("a") %>%
        html_attr("href") %>%
        grep("\\.nc4$", ., value = TRUE)  # only .nc4 files

      list_urls <- c(list_urls, list_urls_month)
      list_urls <- list_urls[!duplicated(list_urls)]
    }
    #### match list_urls to date sequence
    list_urls_date_sequence <- list_urls[
      substr(list_urls, 28, 35) %in%
        date_sequence
    ]
    #### separate data and metadata
    list_urls_data <- list_urls_date_sequence[grep(
      "*.xml",
      list_urls_date_sequence,
      invert = TRUE
    )]
    list_urls_metadata <- list_urls_date_sequence[grep(
      "*.xml",
      list_urls_date_sequence,
      invert = FALSE
    )]
    #### concatenate and print download commands to "..._wget_commands.txt"
    for (l in seq_along(date_sequence)) {
      year <- as.character(substr(date_sequence[l], 1, 4))
      month <- as.character(substr(date_sequence[l], 5, 6))
      download_url <- paste0(
        base,
        esdt_name,
        ".5.12.4/",
        year,
        "/",
        month,
        "/",
        list_urls_data[l]
      )
      download_folder <- paste0(
        directory_to_save,
        collection_loop
      )
      if (!dir.exists(download_folder)) {
        dir.create(download_folder, recursive = TRUE)
      }
      download_name <- paste0(
        download_folder,
        "/",
        list_urls_data[l]
      )
      # download_command <- paste0(
      #   "wget ",
      #   download_url,
      #   " -O ",
      #   download_name,
      #   "\n"
      # )
      earthdatalogin::edl_download(href = download_url, dest = download_name)
      if (amadeus::check_destfile(download_name)) {
        #### cat command only if file does not already exist
        cat(download_command)
      }
      # download_url_metadata <- paste0(
      #   base,
      #   esdt_name,
      #   ".5.12.4/",
      #   year,
      #   "/",
      #   month,
      #   "/",
      #   list_urls_metadata[l]
      # )
      # download_folder_metadata <- paste0(
      #   directory_to_save,
      #   collection_loop,
      #   "/metadata/"
      # )
      # if (!dir.exists(download_folder_metadata)) {
      #   dir.create(download_folder_metadata, recursive = TRUE)
      # }
      # download_name_metadata <- paste0(
      #   download_folder_metadata,
      #   list_urls_metadata[l]
      # )
      # download_command_metadata <- paste0(
      #   "wget ",
      #   download_url_metadata,
      #   " -O ",
      #   download_name_metadata,
      #   "\n"
      # )
      # if (amadeus::check_destfile(download_name_metadata)) {
      #   #### cat command only if file does not already exist
      #   cat(download_command_metadata)
      # }
    }
  }
  #### finish "..._wget_commands.txt"
  # sink()
  #### download data
  # amadeus::download_run(
  #   download = download,
  #   commands_txt = commands_txt,
  #   remove = remove_command
  # )
  # return(amadeus::download_hash(hash, directory_to_save))
}

# download_merra2_check(
#   collection = "inst1_2d_int_Nx",
#   date = "2024-01-01",
#   directory_to_save = "data/",
#   acknowledgement = TRUE,
#   download = TRUE, # NOTE: download skipped for examples,
#   remove_command = FALSE,
# )
