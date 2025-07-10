library(rvest)
library(curl)

url <- "https://gaftp.epa.gov/air/nei/2020/data_summaries/"
page <- read_html(url)

files <- page %>%
  html_elements("a[href]") %>%
  html_attr("href") %>%
  grep("\\.zip$|\\.xls", ., value = TRUE)  # Get only zip or xls files

files = files[!grepl(pattern = "trib", x = files)]

# Build full URLs
base <- "https://gaftp.epa.gov/air/nei/2020/data_summaries/"
full_links <- paste0(base, files)

library(curl)

# Robust download function using curl
download_with_timeout <- function(url, dest, timeout = 1600) {
  h <- new_handle()
  handle_setopt(h,
                timeout = timeout,        # Total time allowed per download (in seconds)
                connecttimeout = 60,      # Max time to establish connection
                low_speed_limit = 1,      # Treat as failed if <1 byte/sec for...
                low_speed_time = 30)      # ...30 seconds
  
  tryCatch(
    {
      curl_download(url, destfile = dest, handle = h)
      message(sprintf("✅ Downloaded: %s", basename(dest)))
    },
    error = function(e) {
      message(sprintf("❌ Failed: %s\nReason: %s", basename(dest), e$message))
    }
  )
}

# Download to folder
# dir.create("for_host", showWarnings = FALSE)

for (i in seq_along(full_links)) {
  dest <- file.path("for_host/2020", basename(full_links[i]))
  download_with_timeout(full_links[i], dest)
}

