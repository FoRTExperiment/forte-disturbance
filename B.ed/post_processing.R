## Extract the ED results from the h5 files 
## There is some extra post processing that will have to happen (the plan is 
## to use drake to make it reproduciable)

# 0. Set Up  ------------------------------------------------------------------------------------
# Define directories. 
BASE_DIR  <- here::here()

# Determine the dirs to process, it should be the name of the 
to_process <- c('exp-1', 'test')

# Load the required pacakges and functions. 
to_source <- file.path(BASE_DIR, "B.ed", "0B.post_processing_functions.R")
stopifnot(file.exists(to_source))
source(to_source)

# 1. To Processes ------------------------------------------------------------------------------
# There has probably got to be a better way to do this, but here we are. 
# Start by gathering a list of all the sub directories. 
to_check_out <- paste0(BASE_DIR,'/', to_process)
dir_list     <- list.dirs(to_check_out, recursive = TRUE)
dir_list     <- dir_list[!dir_list %in% to_check_out]

is_missing <- sapply(dir_list, function(x){
  
  path <- list.files(x, '.rds', full.names = TRUE)
  if(isTRUE(file.exists(path))){
    return(FALSE)
  } else {
    return(TRUE)
  }
  
}, USE.NAMES = TRUE)
is_missing <- names(is_missing)[is_missing]

sapply(is_missing, function(x){
  
  message(x, '\n---------')
  read_monthly_fxn(x)
  
})

# Done 







