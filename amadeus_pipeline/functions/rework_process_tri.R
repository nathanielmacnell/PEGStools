rework_process_tri <- function(
    path = NULL,
    year = 2018,
    extent = NULL,
    ...
) {
  
  csvs_tri_from <-
    list.files(path = path, pattern = "*.csv$", full.names = TRUE)
  csvs_tri <- lapply(csvs_tri_from, read.csv)
  csvs_tri <- data.table::rbindlist(csvs_tri)
  # col_sel <- c(1:ncol(csvs_tri))
  
  dt_tri <- csvs_tri
  
  # column name readjustment
  tri_cns <- colnames(dt_tri)
  tri_cns <- sub(".*?\\.\\.", "", tri_cns)
  tri_cns <- sub("^[^A-Za-z]*", "", tri_cns)
  tri_cns <- gsub("\\.", "_", tri_cns)
  dt_tri <- stats::setNames(dt_tri, tri_cns)
  # dt_tri <- dt_tri[dt_tri$YEAR == year, ]
  dt_tri <- dt_tri %>%
    filter(YEAR %in% year)
  
  chem_names_ids = dt_tri %>%
    dplyr::select(CHEMICAL, TRI_CHEMICAL_COMPOUND_ID) %>%
    distinct(TRI_CHEMICAL_COMPOUND_ID, .keep_all = TRUE)
  
  assign("chem_names_ids", chem_names_ids, .GlobalEnv)
  
  
  
  # depending on the way the chemicals are summarized
  # Unit is kilogram
  # nolint start
  YEAR <- NULL
  LONGITUDE <- NULL
  LATITUDE <- NULL
  TRI_CHEMICAL_COMPOUND_ID <- NULL
  
  dt_tri_x <-
    dt_tri |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_AIR"),
        ~ ifelse(UNIT_OF_MEASURE == "Pounds", . * (453.592 / 1e3), . / 1e3)
      )
    ) |>
    dplyr::group_by(YEAR, LONGITUDE, LATITUDE, TRI_CHEMICAL_COMPOUND_ID) |>
    dplyr::summarize(
      dplyr::across(
        dplyr::ends_with("_AIR"),
        ~ sum(., na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      values_from = c("FUGITIVE_AIR", "STACK_AIR"),
      names_from = "TRI_CHEMICAL_COMPOUND_ID",
      names_sep = "_"
    ) |>
    dplyr::filter(!is.na(LONGITUDE) | !is.na(LATITUDE))
  names(dt_tri_x) <- sub(" ", "_", names(dt_tri_x))
  
  spvect_tri <-
    terra::vect(
      dt_tri_x,
      geom = c("LONGITUDE", "LATITUDE"),
      crs = "EPSG:4269", # all are NAD83
      keepgeom = TRUE
    )
  attr(spvect_tri, "tri_year") <- year
  if (!is.null(extent)) {
    tri_final <- apply_extent(spvect_tri, extent)
    return(tri_final)
  } else {
    return(spvect_tri)
  }
}

########################################################

rework_calculate_tri <- function(
    from = NULL,
    locs,
    locs_id = "site_id",
    radius = c(1e3L, 1e4L, 5e4L),
    geom = FALSE,
    ...
) {
  # from = tri
  # locs_id = "id"
  # radius = c(1e3L, 1e4L, 5e4L)
  # geom = FALSE
  
  amadeus::check_geom(geom)
  if (!methods::is(locs, "SpatVector")) {
    if (methods::is(locs, "sf")) {
      locs <- terra::vect(locs)
    }else{
      locs_sf <- st_as_sf(locs, coords = c("lon","lat"))
      locs <- terra::vect(locs)
    }
  }
  if (!is.numeric(radius)) {
    stop("radius should be numeric.\n")
  }
  terra::crs(locs) <- "EPSG:4269"
  locs_re <- terra::project(locs, terra::crs(from))
  
  # split by year: locs and tri locations
  tri_cols <- grep("_AIR", names(from), value = TRUE)
  # error fix: no whitespace
  tri_cols <- sub(" ", "_", tri_cols)
  
  # inner lapply
  list_radius <- split(radius, radius)
  list_locs_tri <-
    Map(
      function(x) {
        locs_tri_s <-
          sum_edc(
            locs = locs_re,
            from = from,
            locs_id = locs_id,
            sedc_bandwidth = x,
            target_fields = tri_cols,
            geom = FALSE
          )
        return(locs_tri_s)
      },
      list_radius
    )
  
  # bind element data.frames into one
  df_tri <- Reduce(function(x, y) dplyr::full_join(x, y), list_locs_tri)
  if (nrow(df_tri) != nrow(locs)) {
    df_tri <- dplyr::left_join(as.data.frame(locs), df_tri)
  }
  
  df_tri_return <- amadeus::calc_return_locs(
    covar = df_tri,
    POSIXt = FALSE,
    geom = geom,
    crs = terra::crs(from)
  )
  
  # read attr
  df_tri_return$time <- as.integer(attr(from, "tri_year"))
  
  return(df_tri_return)
}
