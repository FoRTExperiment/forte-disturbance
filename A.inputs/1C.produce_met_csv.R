# Process all of the met h5 files and save monthly and annual csv files. 
# 0. Set Up ----------------------------------------------------------------------------------
library(lubridate)
library(magrittr)
library(data.table)
library(ncdf4)
library(assertthat)

WRITE_TO <- '/qfs/people/dorh012/forte-disturbance/ED-outputs/met_results'

# Extract data from the h5 files and save as csv files 
# 
# Args 
#   DIR: str directory path that contains the different met data 
#   met: str name of the specific meteorology dir to process 
# Return: str vector of the csv files, located in a temporary directory 
extract_met_data <- function(DIR, met_dir){
  
  dir <- file.path(DIR, paste0('NARR-ED2_', met_dir))
  
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


# 1. Process Met --------------------------------------------------------------

for(m in  c('met1', 'met2')){
  
  # process the meterology files 
  out <- procss_met_func(m)
  write.csv(out$monthly, file = file.path(WRITE_TO, paste0(m, '_monthly.csv')), row.names = FALSE)
  write.csv(out$annual, file = file.path(WRITE_TO, paste0(m, '_yr.csv')), row.names = FALSE)
  
}





  
  