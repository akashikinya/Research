#This is a script for average spectra

#Import necessary libraries
library(ggplot2)
library(dplyr)

#Clean up the brain
rm(list=ls())

#Select csv
print("_transposed should be selected")#use "specpile_avr" data
inputfile <- file.choose()

#Prepare output folder and its path
DesktopPath <- file.path("C:/users/", Sys.getenv("USERNAME"), "/desktop/", paste0(format(Sys.Date(), "%y%m%d"), "_averagePlot/"))
if (!dir.exists(DesktopPath)) {
  dir.create(DesktopPath)
}
setwd(DesktopPath)

#Read CSV
df <- read.csv(inputfile)
meta_data <- df[1:4, ]

#Reverse order data frames
numeric_data <- df[5:nrow(df), ]
numeric_data_reverse <- numeric_data[nrow(numeric_data):1, ]


#Function to generate a new column name
generate_new_column_name <- function(old_name) {
  if (startsWith(old_name, "avr_ABS_")) {
    return(sub("^avr_ABS_", "", old_name))
  } else if (startsWith(old_name, "avr_SD_")) {
    return(sub("^avr_SD_", "", old_name))
  } else {
    return(old_name)
  }
}

#Generate new column names by referencing the data in the first row of df.
new_column_names <- sapply(df[1, ], generate_new_column_name)

#Rename columns in numeric_data_reverse using new column names
colnames(numeric_data_reverse) <- new_column_names
#Change column name
colnames(numeric_data_reverse)[1] <- "variable"

#Data Formatting
df_reshape <- reshape2::melt(numeric_data_reverse, id.vars = "variable")
#Split the second column with underscores and convert only the back part to uppercase
df_reshape[, 2] <- sapply(as.character(df_reshape[, 2]), function(x) {
  parts <- strsplit(x, "_")[[1]]
  if (length(parts) > 1) {
    parts[2] <- toupper(parts[2])
    return(paste(parts, collapse = "_"))
  } else {
    return(x)
  }
})
#Change column name
colnames(df_reshape)[1] <- "wavenumber"
#Convert wavenumber columns to numeric type
df_reshape$wavenumber <- as.numeric(df_reshape$wavenumber)
#Convert value column to numeric type
df_reshape$value <- as.numeric(as.character(df_reshape$value))
#Check the converted data type
str(df_reshape)

#Get the last point of data for adding labels
label_data <- df_reshape %>%
  group_by(variable) %>%
  filter(row_number() == n())



#for alldata
#Draw a graph using ggplot2 (scripts for Ck, Ic, Lj below)
#Creating Plots
avr_plot <- ggplot(df_reshape, aes(x = wavenumber, y = value, color = variable, linetype = variable)) +
  geom_line() +
  scale_color_manual(values = c("darkred", "darkred", "darkblue", "darkblue", "darkgreen", "darkgreen")) +
  scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1)) +
  scale_x_continuous(breaks = seq(min(df_reshape$wavenumber, na.rm = TRUE), max(df_reshape$wavenumber, na.rm = TRUE), by = 200)) +  #Set horizontal axis in increments of 200
  labs(x = "wavenumber", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  #Remove legend
    panel.border = element_rect(color = "black", fill = NA),  #Draw a black line around the outer border of the plot
    panel.grid.major = element_blank(),  #Delete main ruled line
    panel.grid.minor = element_blank(),   #Delete sub-ruled lines
    axis.line = element_line(color = "black"),  #Set axis line to black line
    axis.ticks = element_line(color = "black"), #Set the axis scale line to black line
    axis.ticks.length = unit(-0.1, "cm"),  #Set axis scale inward
    axis.text = element_text(color = "black", size = 16),  #Black text color and slightly larger size for the numbers on the axis
    axis.title.x = element_text(size = 16),  #Slightly increase the font size of the X-axis title
    axis.title.y = element_text(size = 16)   #Slightly increase the text size of the Y-axis title
  )
#View Plot
print(avr_plot)



#for Ck
avr_plot <- ggplot(df_reshape, aes(x = wavenumber, y = value, color = variable, linetype = variable)) +
  geom_line() +
  scale_color_manual(values = c("lightcoral", "lightcoral", "tomato", "tomato", "red", "red", "darkred", "darkred")) +
  scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
  scale_x_continuous(breaks = seq(min(df_reshape$wavenumber, na.rm = TRUE), max(df_reshape$wavenumber, na.rm = TRUE), by = 200)) +
  labs(x = "wavenumber", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    panel.border = element_rect(color = "black", fill = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(-0.1, "cm"),  
    axis.text = element_text(color = "black", size = 16),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16)   
  )
print(avr_plot)



#for Ic
avr_plot <- ggplot(df_reshape, aes(x = wavenumber, y = value, color = variable, linetype = variable)) +
  geom_line() +
  scale_color_manual(values = c("lightskyblue", "lightskyblue", "darkblue", "darkblue")) +
  scale_linetype_manual(values = c(1, 1, 1, 1)) +
  scale_x_continuous(breaks = seq(min(df_reshape$wavenumber, na.rm = TRUE), max(df_reshape$wavenumber, na.rm = TRUE), by = 200)) +
  labs(x = "wavenumber", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    panel.border = element_rect(color = "black", fill = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),   
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(-0.1, "cm"),  
    axis.text = element_text(color = "black", size = 16),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16)   
  )
print(avr_plot)



#for Lj
avr_plot <- ggplot(df_reshape, aes(x = wavenumber, y = value, color = variable, linetype = variable)) +
  geom_line() +
  scale_color_manual(values = c("mediumspringgreen", "mediumspringgreen", "green", "green", "darkgreen", "darkgreen")) +
  scale_linetype_manual(values = c(1, 1, 1, 1, 1, 1)) +
  scale_x_continuous(breaks = seq(min(df_reshape$wavenumber, na.rm = TRUE), max(df_reshape$wavenumber, na.rm = TRUE), by = 200)) +
  labs(x = "wavenumber", y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  
    panel.border = element_rect(color = "black", fill = NA),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"), 
    axis.ticks.length = unit(-0.1, "cm"),  
    axis.text = element_text(color = "black", size = 16),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16)   
  )
print(avr_plot)


#Save as an image
filename_averagePlot <- paste0("plot_average", format(Sys.Date(), "%y%m%d"), ".png")
filepath_averagePlot <- file.path(DesktopPath, filename_averagePlot)
ggsave(filepath_averagePlot, width = 8, height = 6, dpi = 400)

