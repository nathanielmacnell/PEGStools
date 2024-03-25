######## exposomea_map.R #######
# exposomea_map.R
# map example of simulated Exposome A data
###

##### 0. Setup #####

### packages
library(dplyr)
library(sf)
library(ggplot2)
library(tigris)

### options
sf_use_s2(FALSE) # turn off spherical geometry to enable simpler geo transforms
options(tigris_use_cache = TRUE)  # cache downloads locally to save time

### prepare geographic reference data for NC

# state boundary
nc <- tigris::states(cb=TRUE) %>%
  dplyr::filter(STUSPS == 'NC')
plot(st_geometry(nc)) # inspect

# zip code tabulation areas
zcta5 <- tigris::zctas(cb=TRUE, year=2020) %>%
  # restrict to areas within NC shape (can ignore warning)
  st_intersection(st_geometry(nc))
plot(st_geometry(zcta5)) # inspect

# first three digits of zip code tabulation area
zcta3 = zcta5 %>%
  mutate(ZCTA3 = substr(ZCTA5CE20, 1, 3)) %>%
  group_by(ZCTA3) %>%
  summarize()
plot(st_geometry(zcta3)) # inspect

# counties
county <- tigris::counties(cb=TRUE, state='NC')
plot(st_geometry(county)) # inspect

### data

# swap with real datasets (these are simulated, but names are similar)
# these are created by R/simulate_pegs.R or can be downloaded from this github
load('data/gis_simulated.RData')
load('data/exposomea_simulated.RData')

### merge
pegs <- epr.gis %>%
  # use inner join to only keep obs in both datasets, since we can't plot
  # the ones without geospatial or exposure data
  inner_join(epr.ea, by='epr_number') %>%
  # we'll need to "promote" the data frame to a spatial object so it can
  # be referenced against the other datasets
  st_as_sf(coords = c('gis_longitude','gis_latitude'),
           # coordinate reference system is the same as the reference datasets
           crs = st_crs(nc))

##### 1. Static point map examples #####

# review variable names in dataset
names(pegs)

### a. plot individual points
# this is useful for checking that the CRS was correctly set - the points
# should line up with the state outline
ggplot(pegs) +
  geom_sf(data=nc, fill=NA) + # start with reference outline
  geom_sf(aes(color=factor(ea_b104a_bleach, 
                           levels=c(0,1), 
                           labels=c('No','Yes'))), size=0.5) + # add points
  labs(
    title='Bleach use in [simulated] PEGS cohort',
    color='Bleach Use'
  )

### b. plot average proportion of bleach use by county

county %>%  # note that we start with the geometry we want to see in map
  # append point data to get a multi-record dataset
  st_join(pegs) %>%
  # do the aggregation to get back to one row per county
  group_by(COUNTYFP) %>%   # name of county variable within county dataset
  summarize(mean_bleach = mean(ea_b104a_bleach)) %>% # variable definition
  # pipe resulting dataset to ggplot
  ggplot() + 
    geom_sf(aes(fill=mean_bleach))
    
### b. plot average proportion of bleach use by zcta5
# code is almost the same except change the name of the starting dataset
# and the name of the group_by variable (within this dataset). 

zcta5 %>% 
  st_join(pegs) %>%
  group_by(ZCTA5CE20) %>%   
  summarize(mean_bleach = mean(ea_b104a_bleach)) %>% 
  ggplot() + 
  geom_sf(aes(fill=mean_bleach))

### c. plot average proportion of bleach use by zcta5

zcta3 %>% 
  st_join(pegs) %>%
  group_by(ZCTA3) %>%   
  summarize(mean_bleach = mean(ea_b104a_bleach)) %>% 
  ggplot() + 
  geom_sf(aes(fill=mean_bleach))








