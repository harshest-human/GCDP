getwd()
library(data.table)

GCDP <- function(file_path, start_datetime, end_datetime, output_path) {
  # Read the TXT file using data.table's fread function
  data <- fread(cmd = paste0('"', file_path, '"'), header = TRUE)
  
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
  
  # Filter data based on start_datetime and end_datetime
  data <- data[DateTime >= start_datetime & DateTime <= end_datetime]
  
  # Create the output file name
  output_file <- file.path(output_path, paste0("GC_", format(Sys.Date(), "%Y%m%d"), ".CSV"))
  
  # Write the processed data to the new CSV file
  write.csv(data, file = output_file, row.names = FALSE)
  
  cat("Processed data has been saved as", output_file, "\n")
}


# Call the function with the inputs
#GCDP(input_path, start_datetime, end_datetime, output_path)


#Example
GCDP("D:/Data Analysis/Master Thesis data/Ansyco FTIR Data/Ansyco_Data/Harsh vertical pipe setup.TXT",
     "2021-09-02 11:40:45",
     "2021-09-05 14:06:50",
     "D:/Data Analysis/OTICE_gas_calibration/2023_FTIR_data")

