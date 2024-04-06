# ------------------------------------------------------------- #
# PEGS EJI data build script
# ------------------------------------------------------------- #

# -------- #
# PACKAGES #
# -------- #

library(sf)
library(dplyr)
# library(tigris)
library(purrr)
library(ggplot2)
library(stringr)
library(readxl)
library(labelled)
library(Hmisc)
library(readr)

# ------------- #
# CONFIGURATION #
# ------------- #

# load paths (input data locations)
source('redacted')

DATE_STAMP = '21jun23'         # date stamp for SAS files
VERSION    = '3.1'             # version stamp
DIR_FREEZE = 'Freeze 3.1 v2'   # Directory for Freeze 3.1 data

# --------- #
# LOAD DATA #
# --------- #

epr_tracts <- read_csv(PATH_TRACTS) %>%
  mutate(geoid = as.character(GEOID10)) %>%
  rename(gis_study_event = event) %>%
  mutate(gis_study_event = recode(gis_study_event,
                                  enroll='enrollment',
                                  exposome='current_address_exposome_a',
                                  health='health_and_exposure')) %>%
  select(-GEOID10)
  
str(epr_tracts) # 27651 obs. of 3 variables

eji    <- read.csv(PATH_EJI, as.is = TRUE) %>% # note: import drops leading zero of geoid (added back below)
  select(-OBJECTID, -statefp, -countyfp, -tractce, -affgeoid, -name, -COUNTY, -StateAbbr, -StateDesc, 
         -Location, -SHAPE__Length, -SHAPE__Area) %>%
  mutate(geoid = str_pad(as.character(geoid), width = 11, side = 'left', pad = '0')) %>%
  # prepend "eji_" to variables
  rename_at(
    .vars = c(-1), 
    ~paste0('eji_',. )
  )
str(eji) # 73868 obs. of 121 variables

# ------- #
# LINKAGE #
# ------- #

##### Step 1: Join by GEOID #####

# inspect raw GEOIDs
head(epr_tracts$geoid)
head(eji$geoid)

# QC: Are all GEOIDs from cohort present in the eji data?
prop.table(table(epr_tracts$geoid %in% eji$geoid)) # result = TRUE

# append eji data by geoid (and flatten datasets)
joined <- data.frame(epr_tracts) %>%
  left_join(eji, by = join_by('geoid' == 'geoid')) %>%
  select(-geoid)
str(joined) # 10000 obs. of 123 variables



##### Step 2: Metadata cleanup

# load metadata
eji_meta_raw = read_excel(PATH_EJI_META_IN) %>%
  # set variable prefix to lower case
  mutate(variable_name = str_replace(variable_name, pattern='^EJI',replacement='eji')) %>%
  mutate(long_variable_name = str_replace(long_variable_name, pattern='^EJI',replacement='eji'))

# QC: are names the same in metadata and original dataset
table(names(joined) == eji_meta_raw$variable_name) # TRUE 110

# apply descriptions
label(joined) = as.list(eji_meta_raw$description)

# prepare final dataset
epr.eji = joined %>%
  # set -999 values to missing
  mutate_if(is.numeric, function(x) x = ifelse(x<0, NA, x)) # %>%
  # mutate(epr_number = as.character(epr_number))

epr.eji.meta = eji_meta_raw %>%
  relocate(levels, .after=class) %>%
  relocate(long_variable_name, .after=sas_format)

# apply metadata attributes to dataset
for(attr_i in 1:ncol(epr.eji)) {
  attr_list = as.list(epr.eji.meta[attr_i, ])
  attributes(epr.eji[[attr_i]]) <- attr_list
}


# ---------------- #
# DATA EXPORTATION #
# ---------------- #

# save joined file and templates for subsequent cleanup operations
# save(joined, file = 'data/joined.RData')
# write.csv(joined, file = 'data/joined.csv', row.names = FALSE)




### Output
path_out_rdata = file.path(PATH_ROOT,
                           paste0(
                             DIR_FREEZE,'/EJI/eji_', DATE_STAMP, '_v', VERSION, '.RData')
)

path_out_csv = file.path(PATH_ROOT,
                         paste0(
                           DIR_FREEZE,'/EJI/eji_', DATE_STAMP, '_fmtd_v',VERSION, '.csv')
)

path_out_sasnames = file.path(PATH_ROOT,
                              paste0(
                                'Cache/svi_sasnames_', DATE_STAMP, '_v', 
                                VERSION, '.csv')
)



save(epr.eji, epr.eji.meta, file = path_out_rdata)
                                          
write.csv(epr.eji, file=path_out_csv, row.names=FALSE)



# ------------------------ END OF CODE ------------------------ #
