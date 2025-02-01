# this is a script for fig. 2 average data plot
# import necessary libraries
library(dplyr)
library(tools)

# clean up the brain
rm(list = ls())

# prepare input data folder
folder_path <- "C:/Users/Muser/Desktop/240628_averageSpectra/SD_Lj"
file_list <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# create wave numbers
wave_numbers <- seq(4000, 400, length = 3601)

# create column names
colnames <- c("filename", "species", "area", "side", as.character(wave_numbers))

# create an empty output dataframe with appropriate column types
output_df <- data.frame(
  filename = character(),
  species = character(),
  area = character(),
  side = character(),
  matrix(ncol = length(wave_numbers), nrow = 0)
)
colnames(output_df)[5:ncol(output_df)] <- as.character(wave_numbers)

#Only used for graphing
#Calculate the value to add based on the number of files(ABS -> 0.1)
num_files <- length(file_list)
adjustments <- seq((num_files - 1) * 0.1, 0, by = -0.1)#ABS


#Load data and add calculated results to output data frame
for (i in seq_along(file_list)) {
  data <- read.csv(file_list[[i]])
  
  #Extract information from file name and remove extension
  file_base <- file_path_sans_ext(basename(file_list[[i]]))
  file_info <- strsplit(file_base, "_")[[1]]
  filename <- paste(file_info[2:5], collapse = "_")
  species <- file_info[4]
  area <- substr(file_info[5], 1, 1)
  side <- substr(file_info[5], 2, 2)
  
  #Add information to the data frame
  data$filename <- filename
  data$species <- species
  data$area <- area
  data$side <- side
  
  #Add the corresponding value for each file
  data[, 5:ncol(data)] <- data[, 5:ncol(data)] + adjustments[i]
  
  #Remove “X” from column name
  colnames(data)[5:ncol(data)] <- sub("^X", "", colnames(data)[5:ncol(data)])
  
  #Add to output data frame
  output_df <- bind_rows(output_df, data)
}

#Create a destination folder path
folder_path <- file.path("C:/users/", Sys.getenv("USERNAME"), "/desktop/", paste0(format(Sys.Date(), "%y%m%d"), "_specpile_avr"))
if (!dir.exists(folder_path)) {
  dir.create(folder_path)
}

#Create a destination folder path
alldata_file <- file.path(folder_path, paste0(format(Sys.Date(), "%y%m%d"), "_spec.csv"))

#Save data as csv with column names
write.csv(output_df, file = alldata_file, row.names = FALSE)

#Store the row names of output_df
row_names <- row.names(output_df)

#Swap columns and rows
output_df_transposed <- as.data.frame(t(output_df))

#Create new column at the top
new_column <- c("filename", "species", "area", "side", wave_numbers)  
output_df_transposed <- cbind(variable = new_column, output_df_transposed)

#Create a destination folder path
metadata_file <- file.path(folder_path, paste0(format(Sys.Date(), "%y%m%d"), "_spectransposed.csv"))

#Save data as csv
write.csv(output_df_transposed, file = metadata_file, row.names = FALSE)

