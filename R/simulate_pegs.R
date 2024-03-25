####### simulate_pegs.R #######
# simulate_pegs.R
# create simulated PEGS dataset for examples
###

##### 0. Setup #####
library(dplyr)
library(tigris)
library(sf)
library(ggplot2)

n_gis = 10000
n_ea = 4000

##### 1. simulate epr.gis #####

# pull shapefile of NC to simulate participant locations
nc <- tigris::states(cb=TRUE) %>%
  dplyr::filter(STUSPS == 'NC')

# sample
points <- sf::st_sample(nc, size=n_gis)
points.coords <- sf::st_coordinates(points)

# simulate gis dataset
epr.gis <- data.frame(
  epr_number = 1:n_gis,
  gis_longitude = points.coords[ ,'X'],
  gis_latitude = points.coords[ ,'Y']
)

# inspect
ggplot(epr.gis) +
  geom_sf(data=nc) +
  geom_point(aes(x=gis_longitude, y=gis_latitude), size=0.05)

# save
save(epr.gis, file='data/gis_simulated.RData')

##### 2. Simulate epr.ea #####
epr.ea = data.frame(
  epr_number = 1:n_ea,
  ea_b104a_bleach = rbinom(n=n_ea, size=1, prob=0.25),
  ea_a021a_stove_electricity = rbinom(n=n_ea, size=1, prob=0.74),
  ea_a021b_stove_natural_bas = rbinom(n=n_ea, size=1, prob=0.22)
)

# save
save(epr.ea, file='data/exposomea_simulated.RData')
