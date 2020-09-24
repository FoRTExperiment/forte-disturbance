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

# Define a function 
outdir <- getwd() 
name <- paste0(basename(outdir), '.rds')

read_monthly_fxn <- function(DIR){
  assertthat::assert_that(dir.exists(DIR))
  name <- paste0(basename(DIR), '.rds') 
  x    <- file.path(DIR, save_file)
  read_monthly_dir(DIR, save_file = x)
  return(x)
}

