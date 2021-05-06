# Define helper functions 

# Copy a list of files to a new location 
# Args 
#   files: vector str of the path to files to copy over
#   new_dir: str path of the directory location to move to, if chosing a new name for files then new_dir should be set to NULL 
#   out_files: vector str of the file location and new name 
# Return: str of path to files 
copy_files_func <- function(files, new_dir, out_files = NULL){
  
  assert_that(all(file.exists(files)))
  
  if(!is.null(new_dir)){
    
    assert_that(dir.exists(new_dir))
    
    lapply(files, FUN = file.copy, to = new_dir, overwrite = TRUE, recursive = TRUE)
    out <- file.path(new_dir, basename(files))
    
  }
  
  if( all(is.null(new_dir), !is.null(out_files)) ) {
    
    file.copy(from = files, to = out_files, overwrite = TRUE, recursive = TRUE)
    out <- out_files 
    
  }
  
  
  return(out)
  
}


# Modify the met header file 
# Args 
#   to: str file path locaiton
# Return: str name of the where the new met file header was written out to that has been updated 
# with the correct header path within the file. 
modify_met_header <- function(to){
  
  assert_that(dir.exists(to))
  
  metdriver_file <- file.path(here::here('A.inputs', 'NARR-ED2-main'), 'ED_MET_DRIVER_HEADER')
  met_file       <- readLines(metdriver_file)
  met_file[[1]]  <- "Modified for the forte-disturbance project"
  
  bd <- basename(to)
  pic_dir <- '/people/dorh012/forte-disturbance/A.inputs'
  met_file[[3]]  <- paste0(pic_dir, '/', bd, '/')
  
  out_file <- file.path(to, 'ED_MET_DRIVER_HEADER')
  writeLines(met_file, out_file)
  return(out_file)
  
}


# Get the yera/month information from files 
# Arg
#   files: str path of the hdf5 files of met data
# Return: dt with columns of the met files, year, and month 
get_year_month <- function(files){
  
  assert_that(all(file.exists(files)))
  assert_that(all(grepl(pattern = '.h5', x = files)))
  h5_file_names <- basename(files)
  years         <- as.integer(substr(h5_file_names, start = 1, stop = 4))
  months        <- substr(h5_file_names, start = 5, stop = 7)
  dt <- data.table(name = files, year = years, month = months)
  
  return(dt)
}


# Find the missing data 
# Args 
#   search_dir: str directory with data to serach
#   startYr: int beginning period where need data 
#   endYr: int final period where data need 
# Return: data.table of the year and month where data is missing 
find_missing_files <- function(serach_dir, startYr, endYr){
  
  assert_that(is.numeric(c(startYr, endYr)))
  assert_that(endYr > startYr)
  
  # Start by figuring out what files exsit in the current met data directory. 
  files <- list.files(serach_dir, '*.h5', full.names = TRUE)
  data_we_have <- get_year_month(files = files)[ , list(year, month, exists = TRUE)]
  
  # Determine what files are missing that we would have expected. 
  expected_years  <- rep(startYr:endYr, each = 12)
  expected_months <- rep(toupper(month.abb), length(expected_years)/12)
  expected_data   <- data.table(year = expected_years, month = expected_months)
  
  # Identify the files that are missing. 
  data_comparison <- data_we_have[expected_data, on = c('year', 'month'), nomatch = NA]
  needed_data     <- data_comparison[is.na(exists), list(year, month)]
  
  return(needed_data)
  
}


# Generate the missing early history files 
# Args 
#   out_dir: str directoy location of met data that is missing historical hdf5 files 
#   startYr: int the first year of data we expect 
# Return: 
generate_early_history <- function(out_dir, startYr = 1900){
  
  data_we_have <- get_year_month(list.files(path = out_dir, pattern = 'h5', full.names = TRUE))
  missing_early_files <- find_missing_files(serach_dir = out_dir, startYr = startYr, endYr = 2000)
  
  if(nrow(missing_early_files) == 0){
    
    message('There are no missing early history files.')
    
  } else {
    
    message('Generating early history files.')
    data_we_need_month_list <- split(missing_early_files, missing_early_files$month)
    
    # Okay so select the files we want to use, we want to use the last 120 files, because we want to use the 
    # last decade of data. 
    pull_from            <- data_we_have[, file := file.path(MET_DIR, paste0(year, month, '.h5'))][1:120, list(month, file, year)]
    pull_from_month_list <- split(pull_from, pull_from$month)
    
    # Determine how many times the data we do have has to be replicated over to fill in 
    # monthly data for the met data we are missing.
    to_copy <- rbindlist(lapply(toupper(month.abb), function(m){
      
      n_needed_obs <- nrow(data_we_need_month_list[[m]])
      n_we_have <- nrow(pull_from_month_list[[m]])
      rep_times <- ceiling(n_needed_obs / n_we_have) 
      assertthat::assert_that(rep_times > 1, msg = 'More data is being used to fill missing data than there is actual missing data.')
      file_vector <- rep(pull_from_month_list[[m]]$file,  rep_times)
      
      out <- data_we_need_month_list[[m]][ , list(year, month)]
      out$file <- file_vector[1:n_needed_obs]
      
      return(out)
      
    }))
    to_copy$out_file <- file.path(out_dir, paste(to_copy$year, to_copy$month, '.h5', sep = ''))
    
    # Depending on how many files are being copies over this could take some time 
    new_files <- copy_files_func(files = to_copy$file, new_dir = NULL, out_files = to_copy$out_file)
    
  }
  
  
}


# Extract the data from the ncdf files, calculate the multi-year mean 
# Args 
#   ncds: vector str path of the netcdf files to process 
#   vars: vector str of the variable names, default vaules set to
#         c('dlwrf', 'hgt', 'nbdsf', 'nddsf', 'prate', 'pres', 'sh', 'tmp', 'ugrd', 'vbdsf', 'vddsf', 'vgrd')
# Return: a df of the mean values per variable per time step within a month 
get_nc_mean <- function(ncs, vars = c('dlwrf', 'hgt', 'nbdsf', 'nddsf', 'prate', 'pres', 
                                      'sh', 'tmp', 'ugrd', 'vbdsf', 'vddsf', 'vgrd')){
  
  n_vars <- length(vars)
  n_steps <- length(ncvar_get(nc_open(ncs[[1]]), 'tmp'))
  
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
  
  
  return(df_out)
  
}


# Generate the future met files starting at the year 2010 (by design we want the met data to all start
# some period before the disturbance event). Note that if future data is detected this function 
# will not over write data. 
# Args 
#   out_dir: str directory location containing the met data to use
#   use_yrs: vector int of the years to use in the average 
#   startYr: int the start of the future period 
#   endYr: int the end of the future period 
# Returns: null but creates hdf5 files 
generate_future_met <- function(out_dir, use_yrs, startYr, endYr){
  
  # Check the inputs 
  assert_that(dir.exists(out_dir))
  assert_that(all(is.integer(use_yrs)))  
  assert_that(startYr < endYr)
  
  # Figure out which files we do have 
  files     <- list.files(out_dir, '*.h5', full.names = TRUE)
  exists_data <- get_year_month(files)
  
  assert_that(all(use_yrs %in% exists_data$year), msg = 'use_yrs contains future years')
  files_to_use <- exists_data[year %in% use_yrs, ]
  
  we_have <- get_year_month(list.files(out_dir, '*.h5', full.names = TRUE))
  missing_future_files <- find_missing_files(serach_dir = out_dir, startYr = startYr, endYr = endYr)
  
  if(nrow(missing_future_files) == 0){
    
    message('Future climate data has already been generated.')
    
  } else {
    
    message('Caculating future climate.')
    
    # Create a temporary directory where a copy of the decadal average. 
    inter_dir <- tempdir() 
    # Define the variables from the variables. 
    vars <- c('dlwrf', 'hgt', 'nbdsf', 'nddsf', 'prate', 'pres', 
              'sh', 'tmp', 'ugrd', 'vbdsf', 'vddsf', 'vgrd')
    
    # Calculate the monthly average and save the data. 
    for(mon in toupper(month.abb)){
      
      ncs <- paste0(MET_DIR, '/', unique(files_to_use$year), mon, '.h5')
      
      # Account for the leap years 
      if(mon == 'FEB'){
        n_steps <- 224
      }  else {
        n_steps <- length(ncvar_get(nc_open(ncs[[1]]), 'tmp'))
      } 
      
      # Caculate the daily average value for each variable 
      df <- get_nc_mean(ncs)
      
      
      # This code was based off of https://github.com/PecanProject/pecan/blob/3b0cb56bba1eb90d1f7bbe3b0da3510f681519ba/models/ed/R/met2model.ED2.R
      # and requires hdf5 dependencies (might not work on pic). 
      temp_out <- file.path(paste0(mon, '.h5')) 
      file.copy(ncs[[1]], temp_out)
      fill_in_h5(file = temp_out, df = df, n_steps = n_steps)
      
      
      # Now copy over the netcdf files to all of the files.
      new_files <- file.path(out_dir, paste0(2010:endYr, mon, '.h5'))
      file.copy(temp_out, new_files, overwrite = TRUE)
      
      # Remove the temp file
      file.remove(temp_out)
      
    } # monthly for loop 
    
  } # if else statement
  
}



# Fill in a hdf5  ed met file with data 
# Args
#   file: str path to the hdf5 file of met data that can act as a template
#   n_steps: the number of time steps per month 
# Return: srt path to the hdff file with the data in it 
fill_in_h5 <- function(file, df, n_steps){
  
  assert_that(file.exists(file))
  ed_met_h5 <- hdf5r::H5File$new(file)
  
  assert_that(is.matrix(df))
  assert_that(has_name(x = as.data.frame(df), 
                       which = c('dlwrf', 'hgt', 'nbdsf', 'nddsf', 'prate', 'pres', 'sh', 'tmp', 
                                 'ugrd', 'vbdsf', 'vddsf', 'vgrd')))
  
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
  nbdsf <- array(df[ ,"nbdsf"], dim = dims)
  nddsf <- array(df[ ,"nddsf"], dim = dims)
  vbdsf <- array(df[ ,"vbdsf"], dim = dims)
  vddsf <- array(df[ ,"vddsf"], dim = dims)
  prate <- array(df[ ,"prate"], dim = dims)
  dlwrf <- array(df[ ,"dlwrf"], dim = dims)  
  pres  <- array(df[ ,"pres"], dim = dims)
  hgt   <- array(df[ ,"hgt"], dim = dims)
  ugrd  <- array(df[ ,"ugrd"], dim = dims)
  vgrd  <- array(df[ ,"vgrd"], dim = dims)
  sh    <- array(df[ ,"sh"], dim = dims)
  tmp   <- array(df[ ,"tmp"], dim = dims)
  
  
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
  
  # Return the file name 
  return(file)
  
}


# Extract data from the h5 files and save as csv files 
# 
# Args 
#   DIR: str directory path that contains the different met data 
#   met: str name of the specific meteorology dir to process 
# Return: str vector of the csv files, located in a temporary directory 
extract_met_data <- function(DIR, met_dir){
  
  dir <- file.path(DIR, basename(met_dir))
  assert_that(dir.exists(dir))
  
  header_file <- list.files(dir, 'HEADER', full.names = TRUE)
  header_contents <- readLines(header_file)
  vars <- unlist(strsplit(header_contents[[6]], split = ' '))
  vars <- vars[vars != 'co2']
  
  h5_files <- list.files(dir, '.h5', full.names = TRUE)
  
  results <- c()
  temp_dir <- tempdir()
  
  for(path in h5_files){
    
    nc <- nc_open(path)
    values <- sapply(X = vars, FUN = ncvar_get, nc = nc, USE.NAMES = TRUE)
    values <- as.data.frame(values)
    
    year_month <- gsub(basename(path), pattern = '.h5', replacement = '')
    year <- as.integer(substr(year_month, 1, 4))
    month <- gsub(pattern = year, replacement = '', x = year_month)
    month_list <- toupper(month.abb)
    month <-  which(month == month_list)
    
    out      <- cbind(year = year, month = month,  t_index =  1:nrow(values), values)
    out_file <- file.path(temp_dir, paste0(basename(path),'.csv'))
    write.csv(out, file = out_file, row.names = FALSE)
    results <- c(results, out_file)
    
  }
  
  return(results)
  
}


# Calculate the monthly values from the met data files 
# Args 
#   files: str vector path to the files  
# Return: a data frame of the monthly values
get_monthly_mean <- function(files){
  
  # Check the inputs 
  assert_that(all(file.exists(files)))
  assert_that(all(grepl('h5', files)))
  
  # Calculate the monthly mean for each variable.
  monthly_mean <- rbindlist(lapply(files, function(f){
    dat  <- read.csv(f)
    mean_vals <- apply(dat, 2, mean)
    mean_vals <-  as.data.table(t(as.matrix(mean_vals, nrow = 1)))
    return(mean_vals)
  }))
  
  # Reshape the monthly values 
  monthly_mean$date <- ymd(paste(monthly_mean$year, monthly_mean$month, '01', sep = '-')) 
  monthly_mean[ , list(date, nbdsf, nddsf, vbdsf, vddsf, prate, dlwrf, pres, ugrd, vgrd, sh, tmp)] %>%  
    melt(id.vars = c("date"), measure.vars = c("nbdsf", "nddsf", "vbdsf", "vddsf", 
                                               "prate", 'dlwrf', "pres", "ugrd", "vgrd",
                                               "sh", 'tmp')) -> 
    monthly_mean 
  
  # Return the monthly mean 
  return(monthly_mean)
  
}


# Proccess the h5 files into average annual and average monthly values 
# Args 
#   met: str the name of the meteorological dir to process
# Return: list length 2, monthly data frame and annual data frame
procss_met_func <- function(met, DIR = "/qfs/people/dorh012/forte-disturbance/A.inputs"){
  
  files <- extract_met_data(DIR, met_dir = met)
  
  monthly_data <- get_monthly_mean(files)
  
  data <- monthly_data[ , list(year = year(date), value, variable)] 
  data <- data[ , list(value = mean(value)), by = list(year, variable)]
  
  return(list(monthly = monthly_data, annual = data))
  
}

