get_large_files <- function(path = ".", threshold_mb = 10) {
  files <- list.files(path, full.names = TRUE, recursive = TRUE)
  file_info <- file.info(files)
  
  # Filter files over threshold
  big_files <- subset(file_info, size > threshold_mb * 1024^2)
  
  # Return just file names and sizes (in MB)
  big_files <- big_files[order(-big_files$size), ]
  big_files$size_mb <- round(big_files$size / 1024^2, 2)
  
  return(data.frame(
    file = rownames(big_files),
    size_mb = big_files$size_mb,
    row.names = NULL
  ))
}

# Example usage
get_large_files()
