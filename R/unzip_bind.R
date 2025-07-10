library(dplyr)

x = list.files(path = "for_host", recursive = TRUE, full.names = TRUE)

x2 = x[grep(pattern = "facility|road|nonpoint|eventfire", x = x)]

f = x2[-31]

f_year = f[grep(pattern = "2020", x = f)]

unzipped_data <- list()

temp_dir <- tempdir()

for (i in 1:length(f_year)) {
  zip_path = f_year[i]
  message("Processing: ", zip_path)
  
  # Create a unique folder for each ZIP extraction
  unzip_folder <- file.path(temp_dir, tools::file_path_sans_ext(basename(zip_path)))
  dir.create(unzip_folder, recursive = TRUE, showWarnings = FALSE)
  
  # Unzip all contents into that folder
  unzip(zip_path, exdir = unzip_folder)
  
  # Find all CSV files recursively
  csv_files <- list.files(unzip_folder, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  csv_files = csv_files[!grepl(pattern = "trib", x = csv_files)]
  
  # Read each CSV
  df_list <- lapply(csv_files, function(file) {
    tryCatch(
      read.csv(file, stringsAsFactors = FALSE),
      error = function(e) {
        message("Failed to read ", file)
        NULL
      }
    )
  })
  
  # Remove NULLs
  df_list <- Filter(Negate(is.null), df_list)
  
  # Combine into one data frame
  combined_df <- do.call(bind_rows, df_list)
  
  # Store in list with a cleaned-up key name
  key <- gsub("[/.]", "_", tools::file_path_sans_ext(zip_path))
  unzipped_data[[key]] <- combined_df
}

for(i in 1:length(unzipped_data)){
  tmp_df = unzipped_data[[i]]
  
  saveRDS(tmp_df, file = paste0("for_host/2020/",names(unzipped_data)[i], ".rds"))
}
