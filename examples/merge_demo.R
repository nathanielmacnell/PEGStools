######## merge_demo.R #######
# example of merging PEGS dataset components
###

##### 0. Setup #####

### load individual data segments
load('/path/to/earthdata_file.RData')
load('/path/to/hazards_file.RData')
load('/path/to/svi_file.RData')
load('/path/to/eji_file.RData')
load('/path/to/healthexposure_file.RData')
load('/path/to/exposomea_file.RData')
load('/path/to/exposomeb_file.RData')
load('/path/to/map_file.RData')

### merge datasets by epr_number
# with full_join, it doesn't matter which order you do the merge
# if this fails, make sure that all of the epr_numbers are the same type 
# (e.g. convert to string with as.character() first, if needed).
merged <- epr.bcbb.map |>
  dplyr::full_join(epr.he,      by=join_by(epr_number)) |>
  dplyr::full_join(epr.ea,      by=join_by(epr_number)) |>
  dplyr::full_join(epr.eb,      by=join_by(epr_number)) |>
  dplyr::full_join(epr.hazards, by=join_by(epr_number)) |>
  dplyr::full_join(epr.svi,     by=join_by(epr_number)) |>
  dplyr::full_join(epr.sji,     by=join_by(epr_number))

### apply restriction criteria
# the resulting dataset will contain all of the data but also many missing
# values where participants did not complete specific study components.
# in this example, we restrict to participants who completed the asthma
# question on the health and exposure survey, as well as several relevant 
# questions in the exposome survey and the GIS hazards dataset

restricted <- merged |>
  
  dplyr::filter(
    
    # asthma on H&E
    !is.na(he_d030_asthma_PARQ),
    
    # completed information to derive dust exposure on exposome A
    !is.na(he_b134_dust_d),
    
    # has GIS hazards data available
    !is.na(hazards_pm_10_ugm3)
    
  )
  
### save analysis datasets
# it can be useful to save the unrestricted dataset to help
# build a table showing how the inclusion criteria influenced your
# covariate distributions, etc.
save(merged, restricted, 'analysis.RData')













