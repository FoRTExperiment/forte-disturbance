# Generate the ensemble of climate runs 


# So there really should be a set of unchanged met data, that is dowlaoded 
# bcause everytime we manipualte it we loose the data in the recent history that was
# use :( 

# Also because we look the climatological average over this period in time I think we are looking at the 
# ensemble mean hmmmmm hmmmm would it be as simple as shifting the runs. 

# bleh it is really confusing and i dont wannt tho think about this at all!




# 0. Set Up -----------------------------------------------------------------------------------------
# Load the libraries 
library(magrittr)
library(here)
library(data.table)
library(ncdf4)
library(purrr)
library(ncdf4)
library(lubridate)


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
MET_DIR <- file.path(BASE_DIR, 'A.inputs', 'NARR-ED2')

# Make sure that the met data dir exists and that there is data for the last met year. 
last_met_year <- 2030
assertthat::assert_that(dir.exists(MET_DIR))

# 1. Find Met Inputs -----------------------------------------------------------------------
h5_file_names <- list.files(MET_DIR, '*.h5')
assertthat::assert_that(any(grepl(pattern = last_met_year, h5_file_names)))







