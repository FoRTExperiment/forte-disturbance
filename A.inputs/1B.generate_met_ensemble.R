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
PECAN_DIR <- '/people/dorh012/forte-workflow/pecan'
assertthat::assert_that(dir.exists(PECAN_DIR))
source(file.path(PECAN_DIR,  'models', 'ed', 'R', 'read_ed2in.R'))
source(file.path(PECAN_DIR,  'models', 'ed', 'R', 'write_ed2in.R'))

# Define the directories
BASE_DIR <- here::here()
MET_DIR <- file.path(BASE_DIR, 'A.inputs', 'NARR-ED2-main')

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
met2_dir <- here::here(BASE_DIR, 'A.inputs', 'NARR-ED2-met2')
dir.create(met2_dir, showWarnings = FALSE)

# Prepare the dir and then generate the future met data as some average of data over several years.
prep_dir(met2_dir)
generate_future_met(out_dir = met2_dir, use_yrs = 2000:2005, startYr = 2000, endYr = 2035)












