# 0. Set Up ------------------------------------------------------------------------
library(magrittr)
R <- sessionInfo()
R <- as.numeric(paste0(R$R.version$major, R$R.version$minor))
assertthat::assert_that(R > 35, msg = 'must use R version 3.5.1 or greater')

# There were some issues installing ed4forte on pic. 
# Download from https://github.com/FoRTExperiment/ed4forte 
ED4FORTE_DIR <- '/people/dorh012/forte-workflow/testing-ensemble/ed4forte'
assertthat::assert_that(dir.exists(ED4FORTE_DIR))
source(file.path(ED4FORTE_DIR, 'R', 'read-output.R'))

data <- readRDS(file.path(ED4FORTE_DIR, 'R', 'ed2_variable_info.rds'))
ed2_variable_info <- function(){return(data)}

# 1. Define Functions -------------------------------------------------------------
# Using the ed4forte function read the monthly data in to transform from
# the hdf5 files into a data frame. 
# Args 
#   DIR a data directory that contains the output results 
# Return: writes out an rds file and returns information about the rds file
read_monthly_fxn <- function(DIR){
  assertthat::assert_that(dir.exists(DIR))
  name <- paste0(basename(DIR), '.rds') 
  x    <- file.path(DIR, name)
  read_monthly_dir(DIR, save_file = x)
  return(x)
}

# Process the output directories 
# Args 
#   BASE_DIR: base directory for the project 
#   exp_dirs: a vector of output directories 

#process_outputs(here::here(), c('test'))
process_outputs <- function(BASE_DIR = here::here(), exp_dirs){
  
  # Take a look at the experiment directories and check out their sub dirs. 
  to_check_out <- paste0(BASE_DIR,'/', exp_dirs)
  # Make sure that we are not including the experimental directories. 
  subdir_list  <- list.dirs(to_check_out, recursive = TRUE)
  subdir_list  <- subdir_list[!subdir_list %in% to_check_out]
  
  # Determine which subdirs are missinng the rds file, there is no point
  # in writing over rds files. 
  is_missing <- sapply(subdir_list, function(x){
    
    path <- list.files(x, '.rds', full.names = TRUE)
    if(isTRUE(file.exists(path))){
      return(FALSE)
    } else {
      return(TRUE)
    }
    
  }, USE.NAMES = TRUE)
  is_missing <- names(is_missing)[is_missing]
  
  if(length(is_missing) == 0) {
    message('No files to process')
    out <- NA
  } else {
    out <- sapply(is_missing, function(x){
      message(x, '\n---------')
      read_monthly_fxn(x)
      
    })
    
  }
  
  return(out)
  
}














message('0B.post_processing_functions.R loaded')
