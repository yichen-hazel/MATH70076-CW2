# 01_download.R

# use either package or download form website
require("nhanesA")
library(nhanesA)

# 2015–2018 and 2021–2023 of NHANES
cycles <- c("I", "J", "L") 
#depression and health-related modules
prefixes <- c("DPQ", "DEMO", "ALQ", "PAQ", "SMQ", "MCQ", "DIQ")
save_dir <- "data/raw"
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)



download_module <- function(module, cycle) {
  file_code <- paste0(module, "_", cycle)
  dest_path <- file.path(save_dir, paste0(file_code, ".rds"))
  

  if (file.exists(dest_path)) {
    message("Already exists: ", file_code)
    return(NULL)
  }
  
  message("Downloading: ", file_code)
  df <- tryCatch(nhanes(file_code), error = function(e) NULL)
  if (!is.null(df)) {
    saveRDS(df, file = dest_path)
  } else {
    warning("Failed to download: ", file_code)
  }
}


for (cycle in cycles) {
  for (module in prefixes) {
    download_module(module, cycle)
  }
}

