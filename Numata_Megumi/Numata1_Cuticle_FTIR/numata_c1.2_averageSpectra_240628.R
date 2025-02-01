#This is a script for average spectra
#Import necessary libraries
library(dplyr)
library(readr)

#Clean up the brain
rm(list = ls())

#Prepare input data folder
DesktopPath <- file.path("C:/users/", Sys.getenv("USERNAME"), "/desktop/", paste0(format(Sys.Date(), "%y%m%d"), "_averageSpectra/"))
if (!dir.exists(DesktopPath)) {
  dir.create(DesktopPath)
}
setwd(DesktopPath)

#Select csv
print("ABS_(Ck,Ic,Lj) or SD_(Ck,Ic, Lj)) should be selected")
inputfile <- file.choose()
#Read CSV
df <- read.csv(inputfile)

#Create column names
colnames <- c("filename", "species", "area", "side")#Manual input later

#Create an empty output dataframe
output_df <- data.frame(matrix(ncol = length(colnames)))
colnames(output_df) <- colnames

#Calculate the average for each wavenumber and store in output_df
for (wave in 4000:400) {
  col_name <- paste0("X", wave)
  if (col_name %in% colnames(df)) {
    output_df[[col_name]] <- mean(df[, col_name], na.rm = TRUE)
  } else {
    output_df[[col_name]] <- NA  #If column does not exist in df, insert NA
    cat("Column", col_name, "does not exist in df. NA values inserted.\n")
  }
}

#Display of calculation results
print(output_df)

#Prepare output folder on desktop
DesktopPath <- file.path("C:/users/", Sys.getenv("USERNAME"), "/desktop/", paste0(format(Sys.Date(), "%y%m%d"), "_averageSpectra/"))
if (!dir.exists(DesktopPath)) {
  dir.create(DesktopPath)
}

#Get the “_ABS_Ck_po” part from the file name
file_info <- strsplit(basename(inputfile), "_")[[1]]
file_info <- paste(file_info[length(file_info) - 2], file_info[length(file_info) - 1], file_info[length(file_info)], sep = "_")

#Create file name for saving
filename_average <- paste0("spectra_avr_", file_info)
filepath_average <- file.path(DesktopPath, filename_average)

#Save as CSV file
write.csv(output_df, filepath_average, row.names = FALSE) 

