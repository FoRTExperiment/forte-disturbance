# This script only needs and should only be run once. After the met data is generated 
# there is no need to re run it. 
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
#MET_DIR <- '/Users/dorh012/Documents/2020/FoRTE/pracitceED/ed-input-data/NARR-ED2-test'
# Make sure that the met data dir exists. 
assertthat::assert_that(dir.exists(MET_DIR))

# 1. Modify Header File -----------------------------------------------------------------------
# Rename the location of the met header file so that it points to the directory location. 
metdriver_file <- file.path(MET_DIR, 'ED_MET_DRIVER_HEADER')
met_file       <- readLines(metdriver_file)
met_file[[1]]  <- "Modified for the forte-disturbance project"
met_file[[3]]  <- paste0(file.path(BASE_DIR, 'A.inputs', 'NARR-ED2'), '/')
writeLines(met_file, metdriver_file)

# 2. Manipulate Early History (1990-1970s) ----------------------------------------------------
# Start by figuring out what files exsit in the current met data directory. 
h5_file_names <- list.files(MET_DIR, '*.h5')
years         <- as.integer(substr(h5_file_names, start = 1, stop = 4))
months        <- substr(h5_file_names, start = 5, stop = 7)
data_we_have  <- data.table(year = years, month = months, exists = TRUE)

# Determine what files are missing that we would have expected. 
expected_years  <- rep(1900:max(years), each = 12)
expected_months <- rep(toupper(month.abb), length(expected_years)/12)
expected_data   <- data.table(year = expected_years, 
                              month = expected_months)

# Identify the files that are missing. 
data_comparison <- data_we_have[expected_data, on = c('year', 'month'), nomatch = NA]
data_we_need    <- data_comparison[is.na(exists), ]

if(nrow(data_we_need) == 0){
  
  message('There are no missing early history files.')
  
} else {
  
  message('Copying over early history missing files.')
  
  data_we_need_month_list <- split(data_we_need, data_we_need$month)
  
  # Okay so select the files we want to use, we want to use the last 120 files, because we want to use the 
  # last decade of data. 
  pull_from            <- data_we_have[, file := file.path(MET_DIR, paste0(year, month, '.h5'))][1:120, list(month, file, year)]
  pull_from_month_list <- split(pull_from, pull_from$month)
  
  # Create a table that matches a missing met data month with the filler data that loops over at leaset a decade. 
  to_copy <- rbindlist(lapply(months, function(m){
    
    # Determine how many times the data we do have has to be replicated over to fill in 
    # monthly data for the met data we are missing. 
    n_needed_obs <- nrow(data_we_need_month_list[[m]])
    n_we_have <- nrow(pull_from_month_list[[m]])
    rep_times <- ceiling(n_needed_obs / n_we_have) 
    assertthat::assert_that(rep_times > 1, msg = 'More data is being used to fill missing data than there is actual missing data.')
    file_vector <- rep(pull_from_month_list[[m]]$file,  rep_times)
    
    out <- data_we_need_month_list[[m]][ , list(year, month)]
    out$file <- file_vector[1:n_needed_obs]
    
    return(out)
    
  }))
  
  # Rename a copy of the existing met data and save it in the new met directory. 
  copied_over <- apply(to_copy, MARGIN = 1, function(x){
    
    out_file <- file.path(MET_DIR, paste0(x[['year']], x[['month']], '.h5'))
    system2('cp', args = c(x[['file']], out_file))
    
  })
  
}


# 3. Manipulating Recent History / Near Future Files (2015-2030) ----------------------------------------------------
# This chunk of codes calculate the a climatology from the most recent decade of met files. 
# Which will be used starting in 2015. 

# First calculate the decadal average. 
# Start by figuring out what files exsit in the current met data directory. 
h5_file_names <- list.files(MET_DIR, '*.h5', full.names = TRUE)
years         <- as.integer(substr(basename(h5_file_names), start = 1, stop = 4))
months        <- substr(basename(h5_file_names), start = 5, stop = 7)
data_we_have  <- data.table(year = years, month = months, index = 1:length(months), exists = TRUE)
files_to_use  <- data_we_have[year %in% 2008:2018, ]


if(2030 %in% years){
  
  message('Future climate data has already been generated.')
  
} else {
  
  # Load this library! It is not loaded in section 0 because of some odd dependency issues, 
  # it may be easier to execute this chunk of code on a local machine.
  library(hdf5r)
  
  
  months    <- unique(files_to_use$month)
  # Create a temporary directory where a copy of the decadal average. 
  inter_dir <- tempdir() 
  # Define the variables from the variables. 
  vars <- c('dlwrf', 'hgt', 'nbdsf', 'nddsf', 'prate', 'pres', 
            'sh', 'tmp', 'ugrd', 'vbdsf', 'vddsf', 'vgrd')

  # Calculate the monthly average and save the data. 
  for(mon in months){
    
    ncs <- paste0(MET_DIR, '/', unique(files_to_use$year), mon, '.h5')
    
    # The dimensions of the data in the netcdf file and names of the variables. 
    n_vars  <- length(vars)
    n_steps <- length(ncvar_get(nc_open(ncs[[1]]), 'tmp'))
    
    if(mon == 'FEB'){n_steps <- 224}
    
    # Extract the values for the variables and then flatten into  a single data frame. 
    extracted_values <- lapply(ncs, function(f){
      
      nc     <- nc_open(f)
      values <- sapply(vars, ncvar_get, nc = nc, USE.NAMES = TRUE, simplify = TRUE)
      
      nc_close(nc)
      return(values)
      
    })
    flat_matrix     <- do.call(what = 'rbind', args = extracted_values)

    # Determine the number of the years we are using from the climate model. 
    n_years <- nrow(flat_matrix) / n_steps
    
    # Create an empty data frame to store the data. 
    df_out <- matrix(NA, nrow = n_steps, ncol = n_vars, dimnames = list(NULL, vars))
    
    for(var in vars){
      # Create a matrix of values from a single variable, so that each row are the time steps of the met 
      # data and each column is a different year of data that will be used to find the climatological average.
      single_matrix <- matrix(data = flat_matrix[ , var], nrow = n_steps, ncol = n_years, byrow = FALSE)
      mean_value <- apply(single_matrix, 1,  mean)
      
      # Insert the average value into the output data frame. 
      df_out[ , var] <- apply(single_matrix, 1,  mean)
    }
    
    # Now that we have a data frame of the average values! We can save them as a netcdf. 
    # Start by copying over a netcdf file in the temp directory. 
    
    # This code was based off of https://github.com/PecanProject/pecan/blob/3b0cb56bba1eb90d1f7bbe3b0da3510f681519ba/models/ed/R/met2model.ED2.R
    # and requires hdf5 dependencies (might not work on pic). 
    # TODO it might only work with a fresh h5 file, must doubble check. 
    temp_out <- file.path(paste0(mon, '.h5')) 
    system2(paste(c('cp', ncs[[1]], temp_out), sep = ' '))
    
    ed_met_h5 <- hdf5r::H5File$new(temp_out)
    
    # Start by removing the old values.
    ed_met_h5$link_delete('nbdsf')
    ed_met_h5$link_delete('nddsf')
    ed_met_h5$link_delete('vbdsf')
    ed_met_h5$link_delete('vddsf')
    ed_met_h5$link_delete('prate')
    ed_met_h5$link_delete('dlwrf')
    ed_met_h5$link_delete('pres')
    ed_met_h5$link_delete('hgt')
    ed_met_h5$link_delete('ugrd')
    ed_met_h5$link_delete('vgrd')
    ed_met_h5$link_delete('sh')
    ed_met_h5$link_delete('tmp')
    
    
    # Define data dimensions. 
    dims  <- c(n_steps, 1, 1)
    
    # Shape the array. 
    nbdsf <- array(df_out[ ,"nbdsf"], dim = dims)
    nddsf <- array(df_out[ ,"nddsf"], dim = dims)
    vbdsf <- array(df_out[ ,"vbdsf"], dim = dims)
    vddsf <- array(df_out[ ,"vddsf"], dim = dims)
    prate <- array(df_out[ ,"prate"], dim = dims)
    dlwrf <- array(df_out[ ,"dlwrf"], dim = dims)  
    pres  <- array(df_out[ ,"pres"], dim = dims)
    hgt   <- array(df_out[ ,"hgt"], dim = dims)
    ugrd  <- array(df_out[ ,"ugrd"], dim = dims)
    vgrd  <- array(df_out[ ,"vgrd"], dim = dims)
    sh    <- array(df_out[ ,"sh"], dim = dims)
    tmp   <- array(df_out[ ,"tmp"], dim = dims)
    
    # Now insert the values into the h5 file. 
    ed_met_h5[["nbdsf"]] <- nbdsf
    ed_met_h5[["nddsf"]] <- nddsf
    ed_met_h5[["vbdsf"]] <- vbdsf
    ed_met_h5[["vddsf"]] <- vddsf
    ed_met_h5[["prate"]] <- prate
    ed_met_h5[["dlwrf"]] <- dlwrf
    ed_met_h5[["pres"]] <- pres
    ed_met_h5[["hgt"]] <- hgt
    ed_met_h5[["ugrd"]] <- ugrd
    ed_met_h5[["vgrd"]] <- vgrd
    ed_met_h5[["sh"]] <- sh
    ed_met_h5[["tmp"]] <- tmp
    
    # Close the connection to the h5 file for all of the files. 
    ed_met_h5$close_all()
    
    # Now copy over the netcdf files to all of the files. 
    new_files <- file.path(MET_DIR, paste0(2015:2030, mon, '.h5'))
    lapply(new_files, function(f){ system2('cp', args = c(temp_out, f)) })
    
    # Remove the temp file
    file.remove(temp_out)
    
  }
  
}



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
#h5_files <- h5_files[grepl(pattern = '20', x = basename(h5_files))]


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



