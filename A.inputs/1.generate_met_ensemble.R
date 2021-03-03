# Note that this requires that 1A.met_input_processing.R has already been run.

# Load the libraries 
library(magrittr)
library(here)
library(data.table)
library(ncdf4)
library(purrr)
library(lubridate)
library(assertthat)
library(hdf5r)

# There were some issues installing pecan specfic scripts should be sourced from the pecan projects. 
# See https://github.com/PecanProject/pecan to downlaod peacn 
# git show-branch 
# [develop] Merge pull request #2570 from infotroph/book-headers
# commit b264ce700a1892726a7e4ec21c3187152297a190 
# brach devlop
# These files have been committed to this repo for purposes of reproducibility. 
# TODO is this legal or no? 
BASE_DIR  <- here::here()
#PECAN_DIR <- '/people/dorh012/forte-workflow/pecan'
#assertthat::assert_that(dir.exists(PECAN_DIR))
#source(file.path(PECAN_DIR,  'models', 'ed', 'R', 'read_ed2in.R'))
#source(file.path(PECAN_DIR,  'models', 'ed', 'R', 'write_ed2in.R'))

# Define the directories
BASE_DIR <- here::here()
MET_DIR  <- file.path(BASE_DIR, 'A.inputs', 'NARR-ED2-main')
WRITE_TO <- here::here("ED-outputs", "met_results")

# Make sure that the met data dir exists. 
assertthat::assert_that(dir.exists(MET_DIR))

source(here::here('A.inputs', 'helper_functions.R'))

# Prepare a met directory 
# Args
#   out_dir: str path of the directory 
# Return: NULL, copy over a set of files 
prep_dir <- function(out_dir){
  
  assert_that(dir.exists(out_dir))
  
  modify_met_header(out_dir)
  
  files_to_copy <- get_year_month(list.files(file.path(BASE_DIR, 'A.inputs', 'NARR-ED2-main'),
                                             pattern = 'h5', full.names = TRUE))
  more_files_to_copy <- get_year_month(list.files(file.path(BASE_DIR, 'A.inputs', 'NARR-ED2'), 
                                                  pattern = 'h5', full.names = TRUE))[year <= min(files_to_copy$year)]
  
  files_to_copy <- c(files_to_copy$name, more_files_to_copy$name)
  out <- copy_files_func(files_to_copy, out_dir)
  
}

# TODO there should probably be some way to itterate over this 
# -----------------------------------------------------------------------------------------------

complete_years <- 1980:2008 

start <- seq(from = min(complete_years), by = 5,length.out = 8)
end   <- start + 4

periods <- mapply(function(x,y){x:y}, x = start, y = end, SIMPLIFY = FALSE)
names(periods) <- paste0('met', 1:length(periods))


for(m in names(periods)){
  
  main_dir <- here::here(BASE_DIR, 'A.inputs', 'NARR-ED2-main')

  dir <- here::here(BASE_DIR, 'A.inputs', paste0('NARR-ED2_', m))
  assert_that(!dir.exists(dir))
  dir.create(dir, showWarnings = FALSE)

  modify_met_header(dir)
  files <- copy_files_func(list.files(main_dir, pattern = 'h5', full.names = TRUE), new_dir = dir)
  generate_early_history(dir)

  generate_future_met(out_dir = dir, use_yrs = periods[[m]], startYr = 2000, endYr = 2050)

  # process the meterology files 
  out <- procss_met_func(m, DIR = here::here("A.inputs"))
  write.csv(out$monthly, file = file.path(WRITE_TO, paste0(m, '_monthly.csv')), row.names = FALSE)
  write.csv(out$annual, file = file.path(WRITE_TO, paste0(m, '_yr.csv')), row.names = FALSE)
  
}











