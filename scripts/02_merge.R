# merge data

require("dplyr")
require("purrr")
library(dplyr)
library(purrr)
merge_module_data <- function(module_prefix, cycles = c("I", "J", "L"), dir = "data/raw") {
  # Read data for each year
  data_list <- lapply(cycles, function(cycle) {
    file_path <- file.path(dir, paste0(module_prefix, "_", cycle, ".rds"))
    data <- readRDS(file_path)
    data$cycle <- cycle  
    data[] <- lapply(data, function(col) if (is.factor(col)) as.character(col) else col)
    return(data)
  })
  
  # find common columns across all datasets
  common_columns <- Reduce(intersect, lapply(data_list, names))
  
  # subset each dataset to common columns and combine them
  data_common <- lapply(data_list, function(df) df[, common_columns])
  combined_data <- do.call(rbind, data_common)
  
  return(combined_data)
}


# Combine the data from different year for each module
alq_data <- merge_module_data("ALQ")
demo_data <- merge_module_data("DEMO")
diq_data <- merge_module_data("DIQ")
dpq_data <- merge_module_data("DPQ")
mcq_data <- merge_module_data("MCQ")
paq_data <- merge_module_data("PAQ")
smq_data <- merge_module_data("SMQ")

# Combine all datasets into a list
data_list <- list(alq_data, demo_data, diq_data, dpq_data, mcq_data, paq_data, smq_data)

# Perform joint by 'SEQN'
combined_data <- reduce(data_list, ~ inner_join(.x, .y, by = "SEQN"))

saveRDS(combined_data, file = "data/combined/combined_data.rds")

