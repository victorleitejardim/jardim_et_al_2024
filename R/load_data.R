load.data <- function(file){
  ## Check if file exists ----
  
  if (!file.exists(file)) {
    stop("The file '", file, "' does not exist. Please run ", 
         "bccomp_med to download it.", call. = FALSE)
  }
  
  data_name <- load(file, .GlobalEnv)
  get(data_name)
}
