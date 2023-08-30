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
  
  # Create Date.time column
  data[, Date.time := as.POSIXct(paste(Datum, Zeit), format = "%Y-%m-%d %H:%M:%S")][, c("Datum", "Zeit") := NULL]
  
  # Select final columns
  data <- data[, .(Date.time, Messstelle, CO2, NH3, CH4, H2O)]
  
  # Rename the columns
  colnames(data) <- c("Date.time", "Sampling.point.F", "CO2.F", "NH3.F", "CH4.F","H2O.F")
  
  # Mutate columns
  data[, c("Date.time", "Sampling.point.F", "CO2.F", "NH3.F", "CH4.F","H2O.F") := .(
    as.POSIXct(Date.time, format = "%d/%m/%Y %H:%M:%S"),
    as.factor(Sampling.point.F),
    as.numeric(CO2.F),
    as.numeric(NH3.F),
    as.numeric(CH4.F),
    as.numeric(H2O.F))]
  
  # Create the output file name
  output_file <- file.path(clean_path, paste0(format(Sys.Date(), "%Y%m%d"), "_FDP", ".CSV"))
  
  # Write the processed data to the new CSV file
  write.csv(data, file = output_file, row.names = FALSE)
  
  cat("Processed data has been saved as", output_file, "\n")
}

# Call the function with the inputs (Example)
FDP(raw_path="D:/Data Analysis/Gas_data/Raw_data/FTIR_raw/2023-07-08_FTIR.TXT", clean_path="D:/Data Analysis/Gas_data/Clean_data/FTIR_clean")
FTIR_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/FTIR_clean/20230830_FDP.CSV")



######### OTICE FUNCTION ##########
ODP <- function(raw_path, clean_path) {
  # Create an empty data table
  OTICE_data <- data.table()
  
  # Get a list of CSV files in the directory
  csv_files <- list.files(path = raw_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Iterate over each CSV file, read its contents, and append to the OTICE_data data frame
  for (file in csv_files) {
    delimiter <- ifelse(grepl(";", readLines(file, n = 1)), ";", "\t")  # Determine the delimiter based on the first line of each file
    
    data <- read.table(file, header = TRUE, sep = delimiter)  # Adjust the sep parameter based on the delimiter
    
    # Select specific columns by index and rename column 1 to "Date.time"
    selected_data <- data[, c(1, 2, 3, 4, 6, 10, 13)]
    colnames(selected_data)[1] <- "Date.time"
    
    # Remove decimal points from seconds in the "Date.time" column
    selected_data$Date.time <- sub("\\.\\d+", "", selected_data$Date.time)
    
    # Convert the "Date.time" column to a POSIXct object
    selected_data$Date.time <- as.POSIXct(selected_data$Date.time, format = "%Y-%m-%d %H:%M:%S")
    
    # Adjust the time by adding 2 hours (120 minutes)
    selected_data$Date.time <- selected_data$Date.time + minutes(120)
    
    OTICE_data <- rbind(OTICE_data, selected_data)
  }
  
  # Rename the columns
  colnames(OTICE_data) <- c("Date.time", "Sampling.point", "temperature", "H2O", "NH3", "CO2", "CH4")
  
  # Mutate columns
  OTICE_data[, c("Date.time", "Sampling.point", "temperature", "H2O", "NH3", "CO2", "CH4") := .(
    as.POSIXct(Date.time, format = "%d/%m/%Y %H:%M:%S"),
    as.factor(Sampling.point),
    as.numeric(temperature),
    as.numeric(H2O),
    as.numeric(NH3),
    as.numeric(CO2),
    as.numeric(CH4))]
  
  # Create the output file name
  output_file <- file.path(clean_path, paste0(format(Sys.Date(), "%Y%m%d"), "_ODP", ".CSV"))
  
  # Write the processed data to the new CSV file
  write.csv(OTICE_data, file = output_file, row.names = FALSE)
  
  cat("Processed data has been saved as", output_file, "\n")
  
  # Read the written data
  ODP_data <- read.csv(output_file)
  
  # Print the first few rows of the read data
  print(head(ODP_data))
}

# Call the function to combine and save the data (Example)
ODP(raw_path="D:/Data Analysis/Gas_data/Raw_data/OTICE_raw", clean_path="D:/Data Analysis/Gas_data/Clean_data/OTICE_clean")
OTICE_data <- read.csv("D:/Data Analysis/Gas_data/Clean_data/OTICE_clean/20230830_ODP.CSV")


