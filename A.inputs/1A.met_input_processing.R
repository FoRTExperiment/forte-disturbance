# This script only needs and should only be run once. After the met data is generated 
# there is no need to re run it. See READ ME before running. 
#
# It is fairly hacky but cdo/netcdf/ncdf4 won't can't be used to write the h5 file out. 
# 
# 0. Set Up -----------------------------------------------------------------------------------------
# Load the libraries 
library(magrittr)
library(here)
library(data.table)
library(ncdf4)
library(purrr)
library(lubridate)
library(assertthat)
library(hdf5r) # Unfortunately this package cannot be installed on pic the met files must be devvloped on 
# local machine then transfered to pic.

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
METOUT_DIR <- file.path(BASE_DIR, 'A.inputs', 'NARR-ED2')
dir.create(METOUT_DIR, showWarnings = FALSE)

# Make sure that the met data dir exists. 
assertthat::assert_that(dir.exists(MET_DIR))

source(here::here('A.inputs', 'helper_functions.R'))

# -----------------------------------------------------------------------
# Start by creating a new met header file 
new_header <- modify_met_header(to = METOUT_DIR)

# Copy over all of the files exist in the origial copy into the new location 
files     <- list.files(MET_DIR, '*.h5', full.names = TRUE)
new_files <- copy_files_func(files = files, new_dir = METOUT_DIR)

generate_early_history(out_dir = METOUT_DIR)
generate_future_met(out_dir = METOUT_DIR, use_yrs = 2008:2013, startYr = 2000, endYr = 2035)

# -----------------------------------------------------------------------



# 4. Met Data Consolidate and Viz --------------------------------------------------------------
# So the met data ends up being this rather large pile of hdf5 files. Which make is hard to 
# plot this chunk  of the script takes the met files and saves a csv file that can be used 
# to make plots, this is probably the best way to check out the time series for abnormalities. 

# Start by parsing out all of the variables from the header file. 
header_file <- list.files(MET_DIR, 'HEADER', full.names = TRUE)
header_contents <- readLines(header_file)
vars <- unlist(strsplit(header_contents[[6]], split = ' '))
vars <- vars[vars != 'co2']

h5_files <- list.files(MET_DIR, '.h5', full.names = TRUE)


results <- c()
temp_dir <- '/people/dorh012/temp'
for(path in h5_files){
  
  nc <- nc_open(path)
  values <- sapply(X = vars, FUN = ncvar_get, nc = nc, USE.NAMES = TRUE)
  values <- as.data.frame(values)
  
  year_month <- gsub(basename(path), pattern = '.h5', replacement = '')
  year <- as.integer(substr(year_month, 1, 4))
  month <- gsub(pattern = year, replacement = '', x = year_month)
  month_list <- toupper(month.abb)
  month <-  which(month == month_list)
  
  out <- cbind(year = year, month = month,  t_index =  1:nrow(values), values)
  out_file <- file.path(temp_dir, paste0(basename(path),'.csv'))
  message(out_file)
  write.csv(out, file = out_file, row.names = FALSE)
  results <- c(results, out_file)
  
}

monthly_mean <- rbindlist(lapply(results, function(f){
  dat  <- read.csv(f)
  mean_vals <- apply(dat, 2, mean)
  mean_vals <-  as.data.table(t(as.matrix(mean_vals, nrow = 1)))
  return(mean_vals)
}))

write.csv(monthly_mean, file = file.path(MET_DIR, 'monthly_mean_met.csv'), row.names = FALSE)



