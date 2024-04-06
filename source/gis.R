# ------------------------------------------------------------- #
# PEGS GIS data build script
# ------------------------------------------------------------- #

library(dplyr)
library(stringr)

DATE_STAMP = ''
PATH_FREEZE2 = 'redacted'
PATH_GISDATA = paste0(PATH_FREEZE2, 'redacted')
PATH_FREEZE3 = 'redacted'

# Load GIS data
load(PATH_GISDATA)

# set working name
epr.legacy.meta = epr.gis.meta
epr.legacy = epr.gis

# Split into geocodes and hazards

### Hazards
epr.hazards.meta = epr.legacy.meta %>%
  filter(!(variable_name %in% names(epr.legacy)[3:15])) %>%
  mutate(
    variable_name = ifelse(
      (variable_name %in% c('epr_number','gis_study_event')), 
      variable_name,
      str_replace_all(variable_name, 
                      'gis_', 'hazards_')),
    long_variable_name = variable_name
  ) %>%
  relocate(levels, .after=class) %>%
  mutate(
    child_vars = NA,
    parent_vars = NA,
    is_child = NA,
    is_parent = NA,
    long_variable_name = variable_name
  )

epr.hazards = epr.legacy %>%
  select(epr.legacy.meta$variable_name[-3:-15]) %>%
  rename_with(function(i) epr.hazards.meta$variable_name) 

names(epr.hazards) # inspect names
all(names(epr.hazards) == epr.hazards$variable_name) # check names

### GIS
epr.gis.meta = epr.legacy.meta[1:15, ] %>%
  relocate(levels, .after=class)

epr.gis = epr.legacy %>%  # will rename to epr.gis at the end
  select(1:15)

names(epr.gis) # inspect names
all(names(epr.gis) == epr.gis.meta$variable_name) # check names

# Save data
# R datasets
save(epr.hazards, epr.hazards.meta, file=paste0(PATH_FREEZE3, 'Cache/hazards_',DATE_STAMP,'_v3.RData'))
save(epr.gis, epr.gis.meta, file=paste0(PATH_FREEZE3, 'Cache/gis_',DATE_STAMP,'_v3.RData'))

# csv datasets
write.csv(epr.gis, file=paste0(PATH_FREEZE3, 'Cache/gis_',DATE_STAMP,'_fmtd_v3.csv'), row.names=FALSE)
write.csv(epr.hazards, file=paste0(PATH_FREEZE3, 'Cache/hazards_',DATE_STAMP,'_fmtd_v3.csv'), row.names=FALSE)

# ------------------------ END OF CODE ------------------------ #
