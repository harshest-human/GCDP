########### PACKAGES ######################
getwd()
library(tidyverse)
library(psych)
library(dplyr)
library(writexl)
library(readr)
library(vroom)
library(lubridate)
library(purrr)
library(data.table)

######## FTIR FUNCTION #########
FDP <- function(raw_path, clean_path) {
  # Read the TXT file using data.table's fread function
  data <- fread(raw_path, header = TRUE, fill = TRUE)
  
  # Filter out non-numeric rows in Messstelle column
  data <- data[grepl("^\\d+$", Messstelle)]
  
  # Create DateTime column
  data[, DateTime := as.POSIXct(paste(Datum, Zeit), format = "%Y-%m-%d %H:%M:%S")][, c("Datum", "Zeit") := NULL]
  
  # Select final columns
  data <- data[, .(DateTime, Messstelle, H2O, CO2, NH3, CH4)]
  
  # Mutate columns
  data[, c("Messstelle", "CO2", "NH3", "CH4", "H2O") := .(
    as.numeric(Messstelle),
    as.numeric(CO2),
    as.numeric(NH3),
    as.numeric(CH4),
    as.numeric(H2O))]
  
  # Rename the columns
  colnames(data) <- c("DateTime", "Sampling.point", "CO2.F", "NH3.F", "CH4.F","H2O.F")
  
  # Create the output file name
  output_file <- file.path(clean_path, paste0(format(Sys.Date(), "%Y%m%d"), "_FDP", ".CSV"))
  
  # Write the processed data to the new CSV file
  write.csv(data, file = output_file, row.names = FALSE)
  
  cat("Processed data has been saved as", output_file, "\n")
}

# Call the function with the inputs (Example)
#FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023-07-08_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
#FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230830_FDP.CSV")



######### OTICE FUNCTION ##########
ODP <- function(raw_path, clean_path) {
  # Create an empty data frame
  OTICE_data <- data.frame()
  
  # Get a list of CSV files in the directory
  csv_files <- list.files(path = raw_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Iterate over each CSV file, read its contents, and append to the OTICE_data data frame
  for (file in csv_files) {
    delimiter <- ifelse(grepl(";", readLines(file, n = 1)), ";", "\t")  # Determine the delimiter based on the first line of each file
    
    data <- read.table(file, header = TRUE, sep = delimiter)  # Adjust the sep parameter based on the delimiter
    
    # Select specific columns by index and rename column 1 to "DateTime"
    selected_data <- data[, c(1, 2, 3, 4, 6, 10, 13)]
    colnames(selected_data)[1] <- "DateTime"
    
    # Remove decimal points from seconds in the "DateTime" column
    selected_data$DateTime <- sub("\\.\\d+", "", selected_data$DateTime)
    
    # Convert the "DateTime" column to a POSIXct object
    selected_data$DateTime <- as.POSIXct(selected_data$DateTime, format = "%Y-%m-%d %H:%M:%S")
    
    # Adjust the time by adding 2 hours (120 minutes)
    selected_data$DateTime <- selected_data$DateTime + minutes(120)
    
    OTICE_data <- rbind(OTICE_data, selected_data)}
  
   # Rename the columns
  colnames(OTICE_data) <- c("DateTime", "Sampling.point", "temperature", "H2O.O", "NH3.O", "CO2.O", "CH4.O")
  
  # Create the output file name
  output_file <- file.path(clean_path, paste0(format(Sys.Date(), "%Y%m%d"), "_ODP", ".CSV"))
  
  # Write the processed data to the new CSV file
  write.csv(OTICE_data, file = output_file, row.names = FALSE)
  
  cat("Processed data has been saved as", output_file, "\n")
}


# Call the function to combine and save the data (Example)
#ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
#OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230830_ODP.CSV")


