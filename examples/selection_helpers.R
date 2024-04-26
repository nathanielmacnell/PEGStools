######## selection_helpers.R #######
# example of using selection helpers in PEGS variable names
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

### inspect data in a segment
# each .RData segment contains a dataset representing a specific data collection,
# along with a metadata set giving some more information about those variables
# for instance, the healthexposure file contains (your names may vary slightly)
# epr.he        <the dataset>
# epr.he.meta   <the metadata>

View(epr.he.meta)  # look at metadata in data browser

# if you want to check for a specific text in a variable name
# you can search the metadata description
epr.he.meta |>
  dplyr::filter( stringr::str_detect('asthma')) |>
  View()

### select a subset of variables based on names
# you can select multiple variables at once using select helpers in dplyr
epr.hazards.meta |>
  # extract benzene and ethylbenzene
  dplyr::select( dplyr::contains('benzene')) |>
  # get summary statistics for each column
  sapply(summary) |>
  # transpose to make reading results easier
  t()

### select a subset of variables based on metadata
# you can also use the metadata to select variables with specific properties
# in this example, we'll build a table of all of the yes/no variables on the
# health and exposure survey, including phenotypes we might be interested in.
# This works because all of the names in data and the metadata match
# You can verify these match (and this will work properly) with:
all(names(epr.he)==epr.he.meta$long_variable_name)

# Select by metadata attributes
epr.he |>
  # select() wants numeric indexes, so which() converts from boolean test
  dplyr::select( which(epr.he.meta$true_class=='binary')) |>
  # apply table to each column, and assemble into a data frame
  # named with a new variable called "variable" containing the column name
  purrr::map_df(table, .id='variable')


