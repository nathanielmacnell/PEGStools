# ------------------------------------------------------------- #
# PEGS Earthdata build script
# ------------------------------------------------------------- #

# CONFIG
DATE_STAMP = ''
config_generate_metadata_template = TRUE

# -------- #
# PACKAGES #
# -------- #

library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(tidyr)
library(sp)
library(maptools)
library(readr)
library(readxl)
library(stringr)
library(labelled)
library(Hmisc)

### Paths
dir_gis = 'redacted'

# earthdata .rds file with merged GIS data
earthdata_file = 'redacted'
earthdata_config = 'redacted'
earthdata_template = 'redacted'
earthdata_meta_in = 'redacted'

# addresses with associated census tracts
path_pegs_tracts = 'redacted'

# NC shapefile (from SVI dataset) for reference
NC = 'redacted'

load(paste0(dir_gis,'redacted'))


### QC: Project participant addresses on NC map
load('redacted')


### Codebook functions
source('functions.r')
source('createRDataCodebook.r')
CODEBOOK_OUT = 'redacted'


# load census tracts of PEGS records
pegs_tracts = read_csv(path_pegs_tracts) %>%
  mutate(geoid = as.character(GEOID10))

# load tracts
tracts = read_sf(NC)
counties = tracts %>%
  group_by(COUNTY) %>%
  summarise(geometry = st_union(geometry))

# QC: plot nc participants
epr_sf  = st_as_sf(epr.gis %>%
                filter(gis_state=='NC') %>%
                  mutate(
                    latitude = gis_latitude + runif(n=n(), min=0.03, max=0.05),
                    longitude = gis_longitude + runif(n=n(), min=0.03, max=0.05)) %>%
  filter(gis_longitude < 0), coords = c('longitude','latitude'), crs=4269) %>%
  filter(rowSums(st_within(., counties, sparse=FALSE))==1) %>%
  mutate(`Study Event` = recode(gis_study_event,
                                current_address_exposome_a = 'Current address (Exposome A)',
                                enrollment = 'Enrollment address',
                                health_and_exposure = 'Current address (Health and Exposure)',
                                longest_lived_adult_exposome_a = 'Longest-lived adult address (Exposome A)',
                                longest_lived_child_exposome_a = 'Longest-lived child address (Exposome A)')
         )
  
g = ggplot(counties) +
  geom_sf() + 
  geom_sf(aes(color=`Study Event`), data=epr_sf, size=0.1) +
  labs(title='PEGS Participant Addresses in North Carolina',
       subtitle='(n=32001)',
       caption='Points moved by a small random distance in a random direction to preserve confidentiality.') +
  guides(color = guide_legend(override.aes = list(size=1))) +
  theme(
    panel.grid = element_blank(),
    rect = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
    
  )



pdf('pegs_map.pdf', height=4, width=10)
  g
dev.off()

pdf('pegs_map_panel.pdf', height=8, width=16)
  g + facet_wrap(~`Study Event`, ncol = 2)
dev.off()



# read spatial data
spatial_data = read_rds(earthdata_file) %>%
  # ensure we are sorted by geometry and then date
  arrange(as.character(geometry), datetime) %>%
  # Assign geometry_id for future merges
  # each datetime (minorid) is now repeated within each geometry (majorid)
  mutate(
    geometry_id = rep(1:length(unique(geometry)), each=length(unique(datetime)))
  ) %>%
  select(geometry_id, everything())

eligible_to_merge = epr.gis %>%
  filter(gis_study_event %in% c('enrollment','health_and_exposure','current_address_exposome_a'))


dates = mdy(eligible_to_merge$gis_event_date)
min(dates, na.rm=TRUE)  # 2002
max(dates, na.rm=TRUE)  # 2020

# filter to NC geocode records
to_join = epr.gis %>%
  mutate(event = recode(
                gis_study_event,
                enrollment = 'enroll',
                health_and_and_exposure='health',
                current_address_exposome_a='exposome')) %>%
  filter(gis_study_event %in% c('enrollment','health_and_exposure','current_address_exposome_a')) %>%
  filter(gis_state=='NC') %>%
  # fix negative sign in one obs
  mutate(gis_longitude = ifelse(gis_longitude>0, -gis_longitude, gis_longitude)) %>%
  st_as_sf(coords=c('gis_longitude','gis_latitude'), crs=st_crs('WGS84')) %>%
  mutate(datetime=mdy(gis_event_date)) %>%
  select(epr_number, gis_study_event, gis_event_date, datetime)
  
# QC: check projection (close, looks like there are ~4 points outside of NC)
plot(st_geometry(to_join))

# Create a spatial template to facilitate merge
spatial_template = spatial_data %>%
  group_by(geometry_id) %>%
  slice(1) %>%
  select(geometry_id, geometry)



# calculate one-year estimates
estimates_1yr = spatial_data %>%
  data.frame() %>%
  mutate(year = year(datetime)) %>%
  select(geometry_id, year, everything()) %>%
  group_by(year, geometry_id) %>%
  select(-geometry, -datetime) %>%
    summarise(across(.cols = everything(), mean))

# calculate twenty-year estimates

estimates_20yr = spatial_data %>%
  data.frame() %>%
  select(-geometry, -datetime) %>%
  group_by(geometry_id) %>%
    summarise(across(everything(), mean)) %>%
  rename_with(.cols=!c(geometry_id), .fn = function(i) paste0('y19_', i) )

# prepare ids for merging on participant side
ppt_indexed = to_join %>%
  st_join(spatial_template) %>%
  data.frame() %>%
  mutate(year = year(datetime)) %>%
  select(-geometry)

# now we can merge by spatial and temporal index
merged = ppt_indexed %>%
  left_join(estimates_1yr, by=c('geometry_id','year')) %>%
  left_join(estimates_20yr, by=c('geometry_id')) %>%
  select(-geometry_id, -datetime, -year)

##### Generate Template -------------------------------------------------------#
# Not needed if this is already set up (import existing template below)

# load earthdata configuration file
earthdata.config = read_xlsx(earthdata_config, sheet=2) %>%
  mutate(label = paste0(label,' (',units,')')) %>%
  select(variable_name = var_name,
         description = label)

# set up sections for 1 and 19-year integrations
earthdata.config_1 = earthdata.config %>%
  mutate(description = paste0(description, ' (1-year average)'))

earthdata.config_19 = earthdata.config %>%
  mutate(variable_name = paste0('y19_', variable_name) ) %>%
  mutate(description = paste0(description, ' (19-year average)'))

# combine into one set of variable names and descriptions
earthdata.config_all = 
  rbind(earthdata.config_1,
        earthdata.config_19)



if(config_generate_metadata_template) {
  meta_default = function(default) {
    rep(default, ncol(pegs.earthdata))
  }
  
  metadata_template = data.frame(
    old_name      = names(pegs.earthdata),
    variable_name = names(pegs.earthdata),
    description   = meta_default(""),
    sas_format    = meta_default(""),
    long_variable_name = names(pegs.earthdata),
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
  
  write_csv(metadata_template, file=earthdata_template )
  
}

##### -------------------------------------------------------------------- #####


# append descriptions and update to reflect time intervals
epr_earthdata_meta = read.csv(earthdata_meta_in) %>%
  # fix variable types
  mutate(
    # true class
    true_class = ifelse(variable_name == 'gis_study_event', 'character', true_class),
    true_class = ifelse(variable_name == 'gis_event_date', 'date', true_class),
    # class
    class = ifelse(variable_name == 'gis_study_event', 'character', class),
    class = ifelse(variable_name == 'gis_event_date', 'character', class),
    variable_name = ifelse( variable_name %in% c('gis_study_event','gis_event_date',
                                                 'epr_number'), variable_name,
                            paste0('earthdata_', variable_name))
  ) %>%
  # wrap up variable names
  mutate(
    variable_name = tolower(variable_name),
    long_variable_name = variable_name
  )

# update dataset based on metadata
epr_earthdata = copy(merged)  # make a copy (set names replaces by reference)
data.table::setnames(
  x = epr_earthdata,
  old = epr_earthdata_meta$old_name,
  new = epr_earthdata_meta$long_variable_name
)

# apply descriptions
table(names(epr_earthdata) == epr_earthdata_meta$variable_name)
label(epr_earthdata) = as.list(pegs.earthdata.meta$description)
  
# clean up metadata
epr_earthdata_meta = epr_earthdata_meta %>%
  select(-old_name)

# save results
save(epr_earthdata, epr_earthdata_meta, 
  file=paste0('redacted',DATE_STAMP,'.RData')) 

write.csv(epr_earthdata, 
  paste0('redacted',DATE_STAMP,'.csv'), 
  row.names = FALSE)

write.csv(epr_earthdata_meta, 
  paste0('redacted',DATE_STAMP,'.csv'), 
  row.names = FALSE)


# Load to rename objects and variables
load('redacted')
epr.earthdata = epr_earthdata %>%
  select(-gis_event_date)
epr.earthdata.meta = epr_earthdata_meta %>%
  filter(variable_name != 'gis_event_date') %>%
  relocate(levels, .after=class)

head(names(epr.earthdata),30)
head(epr.earthdata.meta$variable_name)

# Save results
save(epr.earthdata, epr.earthdata.meta, 
     file=paste0('redacted',DATE_STAMP,'_v3.RData')) 

write.csv(epr.earthdata, 
          paste0('redacted',DATE_STAMP,'fmtd_v3.csv'), 
          row.names = FALSE)


# ------------------------ END OF CODE ------------------------ #
