dynamicUI <- function() {
  tagList(
    selectInput(inputId = "selectVariable", label = "Select Variable(s)",
                choices = c('weasd', 'snowc','snod')),
    dateRangeInput(inputId = 'dateRange', label = "Select Date Range", min = "1990-01-01", max = Sys.Date(),
                   start = "2022-01-01", end = "2022-01-05")
  )
}