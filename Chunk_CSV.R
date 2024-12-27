library(tidyverse)

# Function to split a CSV into chunks less than a specified file size
split_csv_by_size <- function(input_csv, output_prefix, max_file_size_mb = 100, output_dir = ".") {
  # Step 1: Load the dataset
  data <- read_csv(input_csv)
  
  # Step 2: Estimate file size per row
  temp_file <- tempfile()  # Create a temporary file to test size
  write_csv(data[1:1000, ], temp_file)  # Write a subset to estimate size
  size_per_1000_rows <- file.size(temp_file)  # File size in bytes for 1000 rows
  file.remove(temp_file)  # Clean up temporary file
  
  size_per_row <- size_per_1000_rows / 1000  # Average size per row in bytes
  
  # Step 3: Calculate chunk size for the specified file size limit
  chunk_size <- floor((max_file_size_mb * 1024^2) / size_per_row)  # Size in rows
  
  # Step 4: Split dataset into chunks
  split_data <- data %>%
    mutate(chunk = ceiling(row_number() / chunk_size)) %>%
    group_split(chunk, .keep = FALSE)
  
  # Step 5: Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Step 6: Save each chunk as a CSV
  walk2(
    split_data,
    seq_along(split_data),
    ~ write_csv(.x, file.path(output_dir, paste0(output_prefix, "_chunk_", .y, ".csv")))
  )
  
  # Step 7: List chunk files and confirm sizes
  chunk_files <- list.files(output_dir, pattern = paste0(output_prefix, "_chunk_\\d+\\.csv"), full.names = TRUE)
  chunk_sizes <- map_dbl(chunk_files, ~ file.size(.x) / 1024^2)  # Sizes in MB
  
  # Return a summary
  tibble(
    file = chunk_files,
    size_mb = chunk_sizes
  )
}

# Chunk E&W Disability Data
split_summary <- split_csv_by_size("./data/census/E_W_Disability.csv", "E_W_Disability", max_file_size_mb = 100, output_dir = "./data/census/")

# View the summary of chunked files
print(split_summary)


