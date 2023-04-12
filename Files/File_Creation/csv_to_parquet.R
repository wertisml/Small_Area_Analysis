library(arrow)
library(data.table)

path <- setwd("~/Gasparini/Files/Regions")

convert_csv_to_parquet <- function(file_name, parquet_name) {
  # Read in CSV file
  file_data <- fread(file_name)
  
  table <- as_arrow_table(file_data)
  
  # Write Parquet file
  arrow::write_parquet(table, parquet_name)
}

data <- convert_csv_to_parquet(file_name = "Sheps_Temp_Regions_Race.csv",
                               parquet_name = "Sheps_Temp_Regions_Race.parquet")
