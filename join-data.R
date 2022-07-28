###--- Joins Datasets ---###

library(dplyr)

full_data_files <- list.files("out-data", pattern = "full-.*")
full_data_list <- lapply(paste0("out-data/", full_data_files), read.csv, na.strings = "")

all_data <- full_data_list[[1]]
for (i in full_data_list[2:length(full_data_list)]) {
  all_data <- rbind(all_data, i)
}

agg_data <- distinct(all_data, profile_link, .keep_all = T)
