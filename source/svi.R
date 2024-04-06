# ---------------------------------------------------------------------------- #
# PEGS SVI data build script
# ---------------------------------------------------------------------------- #


##### Config #####
config_generate_metadata_template = FALSE # redo metadata config? default FALSE 
DATE_STAMP = '21jun23'
VERSION = '3.1'
META_CONFIG = 'redacted'
DIR_FREEZE = 'Freeze 3.1 v2'   # Directory for Freeze 3.1 data


###### LIB #####
library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(readxl)
library(data.table)
library(labelled)
library(Hmisc)
library(stringr)

###### PATH #####
# Locations of input data files

### SVI Data
# This folder contains one file per year downloaded from the CDC website

path_root = file.path('redacted')

path_svi = file.path(path_root,
                     'SourceData/SVI')

### SVI .shp files (years 2014-2018)
path_svi_nc_2018 = file.path(path_svi,
                             'NC/2018/SVI2018_NORTHCAROLINA_tract.shp')
path_svi_nc_2016 = file.path(path_svi,
                             'NC/2016/NORTHCAROLINA.shp')
path_svi_nc_2014 = file.path(path_svi,
                             'NC/2014/NORTHCAROLINA.shp')

### SVI .csv files ( years 2000, 2010)
path_svi_nc_2000 = file.path(path_svi,
                             'NC/NorthCarolina2000.csv')
path_svi_nc_2010 = file.path(path_svi,
                             'NC/NorthCarolina2010.csv')

### PEGS Census Tracts
path_epr_tracts = file.path(path_root,
                             'Reference/censusTracts.csv')
  
### Metadata build config (template and completed data)
meta_out = file.path(path_root,
                     paste0(
                        'Cache/svi_metadata_template_', DATE_STAMP, '.csv')
                    )
path_meta_in  = file.path(path_root,
                          paste0('Config/',META_CONFIG)
                    )

# ---------------------------------------------------------------------------- #

##### Linkage #####

### Load SVI Data
# GEOID must be standardized to character class across data sets
svi_nc_2018 = st_read(path_svi_nc_2018) %>%
  rename_all(function(i) paste0('svi_2018_',i)) %>%
  mutate(geoid = as.character(svi_2018_FIPS))

svi_nc_2016 = st_read(path_svi_nc_2016) %>%
  rename_all(function(i) paste0('svi_2016_',i)) %>%
  mutate(geoid = as.character(svi_2016_FIPS))

svi_nc_2014 = st_read(path_svi_nc_2014) %>%
  rename_all(function(i) paste0('svi_2014_',i)) %>%
  mutate(geoid = as.character(svi_2014_FIPS))

# These data are a different format (.csv instead of .shp)
svi_nc_2010 = read_csv(path_svi_nc_2010) %>%
  rename_all(function(i) paste0('svi_2010_',i)) %>%
  mutate(geoid = as.character(svi_2010_FIPS))

# load census tracts of PEGS records
epr_tracts = read_csv(path_epr_tracts) %>%
  mutate(geoid = as.character(GEOID10))

# Join svi into one dataset
svi_nc_raw =
  svi_nc_2018 %>%
  data.frame() %>%
  left_join(svi_nc_2016 %>% data.frame) %>%
  left_join(svi_nc_2014 %>% data.frame) %>%
  left_join(svi_nc_2010) %>%
  # subset to svi variables
  select(geoid,
         starts_with('svi_2010'), starts_with('svi_2014'), 
         starts_with('svi_2016'), starts_with('svi_2018')) %>%
  select(-contains('_ST')) %>%
  select(-contains('_FIPS')) %>%
  select(-contains('_COUNTY')) %>%
  select(-contains('_CENSUSAREA')) %>%
  select(-contains('_AREA')) %>%
  select(-contains('_TRACT')) %>%
  select(-contains('_AFFGEOID')) %>%
  select(-contains('geometry')) %>%
  select(-contains('LOCATION'))

### Append SVI data to participants by census tract
epr_svi_raw = epr_tracts %>%
  left_join(svi_nc_raw, by='geoid')

# ---------------------------------------------------------------------------- #

##### Metadata #####

### Write blank metadata template if needed 
# Typically not needed unless re-building from scratch
# The tr-build step requires manual data entry 
if(config_generate_metadata_template) {
  meta_default = function(default) {
    rep(default, ncol(pegs_svi_raw))
  }

  metadata_template = data.frame(
    old_name      = names(pegs_svi_raw),
    variable_name = names(pegs_svi_raw),
    description   = meta_default(""),
    sas_format    = meta_default(""),
    long_variable_name = names(pegs_svi_raw),
    variable_source = meta_default('SVI'),
    true_class = meta_default('numeric'),
    label = meta_default(""),
    child_vars = meta_default(""),
    is_parent = meta_default('N'),
    is_child = meta_default('N'),
    parent_vars = meta_default(''),
    levels = meta_default(''),
    class = meta_default('')
  
  )

  write_csv(metadata_template, file=meta_out)

}

### Read configured metadata and update naming convention
meta_raw = read_excel(path_meta_in) %>%

  # Clean up descriptions
  mutate(
    # remove year tags (just put in documentation)
    description = str_remove_all(description, ',\\s+\\d{4}-\\s?\\d{4}\\s+ACS'),
    description = str_remove_all(description, ',\\s+2010\\s+SF1')
  )

### Apply other attributes to svi data
label(epr_svi_raw) = as.list(meta_raw$description)



##### Create final dataset
epr.svi = epr_svi_raw[, meta_raw$keep==1] %>%
  rename(gis_study_event = event) %>%
  mutate(gis_study_event = recode(gis_study_event,
                                  enroll='enrollment',
                                  exposome='current_address_exposome_a',
                                  health='health_and_exposure')) %>%
  # set -999 values to missing
  mutate_if(is.numeric, function(x) x = ifelse(x<0, NA, x)) # %>%
  # mutate(epr_number = as.character(epr_number))



##### Create final metadata
epr.svi.meta = meta_raw %>%
  # restrict to variable in output datasets
  filter(keep==1) %>%
  select(-keep) %>%
  # set variable order to match existing metadata
  relocate(levels, .after=class) %>%
  relocate(long_variable_name, .after=sas_format)

# apply metadata attributes to dataset
for(attr_i in 1:ncol(epr.svi)) {
  attr_list = as.list(epr.svi.meta[attr_i, ])
  attributes(epr.svi[[attr_i]]) <- attr_list
}

### QC1: check dataset and metadata variable names match
all(names(epr.svi) == epr.svi.meta$variable_name)
# Pass: [1] TRUE
which(names(epr.svi) != epr.svi.meta$variable_name)
# Pass: integer(0)

### QC2: check gis_study_event values 
unique(epr.svi$gis_study_event)
# Pass: [1] "enrollment"   "current_address_exposome_a"   "health_and_exposure"

### Output
path_out_rdata = file.path(path_root,
                           paste0(
                             DIR_FREEZE,'/SVI/svi_', DATE_STAMP, '_v', VERSION, '.RData')
)

path_out_csv = file.path(path_root,
                         paste0(
                           DIR_FREEZE,'/SVI/svi_', DATE_STAMP, '_fmtd_v',VERSION, '.csv')
)

path_out_sasnames = file.path(path_root,
                              paste0(
                                'Cache/svi_sasnames_', DATE_STAMP, '_v', 
                                VERSION, '.csv')
)




# inspect metadata
names(epr.svi.meta)
#names(epr.he.meta)


### Save results
# R Dataset
save(epr.svi, epr.svi.meta, file=path_out_rdata)

# CSV File
write.csv(epr.svi, path_out_csv, row.names = FALSE)

# Write shorter variable names for SAS
svi_sas = read_excel(path_meta_in) %>% 
  filter(keep==1) %>% 
  select(sas_name = variable_name)

write.csv(
  svi_sas, 
  path_out_sasnames, 
  row.names=FALSE)

# ------------------------ END OF CODE ------------------------ #




