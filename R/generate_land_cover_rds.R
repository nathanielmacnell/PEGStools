files_1985_1994 = list.files(path = "C:/Users/nick.amato/Downloads/Annual_NLCD_LndCov_1985-1994_CU_C1V1", pattern = "\\.tif$", full.names = TRUE)
files_1995_2004 = list.files(path = "C:/Users/nick.amato/Downloads/Annual_NLCD_LndCov_1995-2004_CU_C1V1", pattern = "\\.tif$", full.names = TRUE)
files_2005_2014 = list.files(path = "C:/Users/nick.amato/Downloads/Annual_NLCD_LndCov_2005-2014_CU_C1V1", pattern = "\\.tif$", full.names = TRUE)
files_2015_2024 = list.files(path = "C:/Users/nick.amato/Downloads/Annual_NLCD_LndCov_2015-2024_CU_C1V1", pattern = "\\.tif$", full.names = TRUE)

all_tif = c(files_1985_1994, files_1995_2004, files_2005_2014, files_2015_2024)

for(i in 1:length(all_tif)){
  file_year = str_match(string = all_tif[i], pattern = "LndCov_(\\d+)_CU_C1V1")[,2]
  tmp = rast(all_tif[i])
  
  saveRDS(tmp, paste0("for_host/nlcd/Land_Cover_",file_year,".rds"))
}
