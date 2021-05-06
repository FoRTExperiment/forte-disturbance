# Process met files so get summary information! 
# Run time is proportional to the number of files to run.  

# 0. Set UP --------------------------------------------------------------------------
# Libraries 
library(magrittr)
library(here)
library(data.table)
library(ncdf4)
library(purrr)
library(lubridate)
library(assertthat)
library(hdf5r)

BASE_DIR  <- here::here()
WRITE_TO <- here::here("ED-outputs", "met_results")

# Make sure that the met data dir exists. 
assertthat::assert_that(dir.exists(WRITE_TO))

source(here::here('A.inputs', 'helper_functions.R'))

# Extract data from a single ED met file 
# Args 
#   f: the hf5 file
# Return: a data table of the variables 
single_met_file_extraction <- function(f){
  
  # open the hdff5 file 
  nc <- nc_open(f)
  
  # extracct the variables and format into a data frame 
  vars <- c("nbdsf", "nddsf", "vbdsf", "vddsf", "prate", "dlwrf", "pres",  "hgt", "ugrd", "vgrd", "sh", "tmp")
  values <- lapply(vars, ncvar_get, nc = nc)
  d <- as.data.table(do.call("cbind", values))
  names(d) <- vars
  
  # close the connetion to the nc, there are too many files 
  # to be opened to have sloppy coding practices.
  nc_close(nc)
  return(d)
  
}

# Calculate the annual met data, is uses the single_met_file_extraction
# Args 
#   m: the directory containing the met files 
# Return: the file path of the csv file 
get_annual_met_files <- function(m){
  
  assert_that(all(dir.exists(m)))
  f <- list.files(m, pattern = ".h5", full.names = TRUE)
  
  # read in the header file to figure out the variable names! 
  header_file <- list.files(m, 'HEADER', full.names = TRUE)
  header_contents <- readLines(header_file)
  vars <- unlist(strsplit(header_contents[[6]], split = ' '))
  #vars <<- vars[vars != 'co2']
  
  # Find all  of the h5 files to process and sort them in annual data, 
  # the longer runs take up too much memory space that causes the R 
  # session to fail. 
  pattern <- paste0(paste0(toupper(month.abb), ".h5"), collapse = "|")
  yr <- gsub(pattern = pattern, replacement = "", x = basename(f))
  table <- data.table(file = f, year = yr)
  table_list <- split(table, table$year)
  
  mapply(function(l, y){
    
    lapply(l$file, single_met_file_extraction) %>%  
      do.call(what = "rbind") -> 
      data 
    
    out <- as.data.table(t(apply(data, MARGIN = 2, mean)))
    out <- cbind(year = y, out)
    return(out)
    
    
  }, l = table_list, y = names(table_list), SIMPLIFY = FALSE) %>%  
    do.call(what = "rbind") -> 
    annual_data
  
  ofile = file.path(WRITE_TO, paste0(basename(m), '_yr.csv'))
  write.csv(annual_data, file = ofile, row.names = FALSE)
  
  return(ofile)
  
}

# Calculate the monthly met data, is uses the single_met_file_extraction, note 
# that it only works with the constant data. 
# Args 
#   m: the directory containing the met files 
# Return: the file path of the csv file 
get_monthly_met_files <- function(m){
  
  assert_that(all(dir.exists(m)))
  f <- list.files(m, pattern = ".h5", full.names = TRUE)
  f <- f[grepl(pattern = "2000", x = f)]
  
  # read in the header file to figure out the variable names! 
  header_file <- list.files(m, 'HEADER', full.names = TRUE)
  header_contents <- readLines(header_file)
  vars <- unlist(strsplit(header_contents[[6]], split = ' '))
  #vars <<- vars[vars != 'co2']
  
  # Find all  of the h5 files to process and sort them in annual data, 
  # the longer runs take up too much memory space that causes the R 
  # session to fail. 
  pattern <- paste0(paste0(toupper(month.abb), ".h5"), collapse = "|")
  yr <- gsub(pattern = pattern, replacement = "", x = basename(f))
  month <- gsub(pattern = "2000|.h5", replacement = "", x = basename(f))
  table <- data.table(file = f, year = yr, month = month)
  table_list <- split(table, table$month)
  
  mapply(function(l, m){
    
    lapply(l$file, single_met_file_extraction) %>%  
      do.call(what = "rbind") -> 
      data 
    
    data <- cbind(month = m, data)
    data$time <- 1:nrow(data)
    return(data)
    
    
  }, l = table_list, m = names(table_list), SIMPLIFY = FALSE) %>%  
    do.call(what = "rbind") -> 
    monthly_data
  
  ofile = file.path(WRITE_TO, paste0(basename(m), '_month.csv'))
  write.csv(monthly_data, file = ofile, row.names = FALSE)
  
  return(ofile)
}

# 1. Annual Data --------------------------------------------------------------------------
if (FALSE){
  # Look for the files we want to process
  met_files <- list.files(here::here("A.inputs"), "-constant", full.names = TRUE)
  # Process the files! 
  lapply(met_files, get_annual_met_files)
}

# 2. Monthly data --------------------------------------------------------------------------
# This really only works when there is a constant met. 

if (FALSE){
met_files <- list.files(here::here("A.inputs"), "-constant", full.names = TRUE)
lapply(met_files, get_monthly_met_files)
} 

# 3. Summarize data met data  ----------------------------------------------------------
# Summarise the met data into single csv files that are easy to load and share with others. 
# Description of the met variables. 
var_info <- data.table::data.table(variable = c('dlwrf', 'nbdsf', 'nddsf', 'vbdsf',
                                    'vddsf', 'prate', 'pres', 'hgt', 
                                    'ugrd', 'vgrd', 'sh', 'tmp'), 
                       description = c('downward long wave radiation', 
                                       'near infrared beam downward solar radiation', 
                                       'near IR diffuse downward solar radiation', 
                                       'visible beam downward solar radiation',
                                       'visible diffuse downward solar radiation', 
                                       'precipitation rate', 
                                       'atmospheric pressure', 
                                       'geopotential height', 
                                       'zonal wind',
                                       'meridional wind', 
                                       'specific humidity',
                                       'air temperature'),  
                       units = c('W m-2', 'W m-2', 'W m-2', 'W m-2',
                                 'W m-2', 'kgH2O m-2 s-1', 'Pa', 'm',
                                 'm s-1', 'm s-', 'kgH2O kgAir-1', 'K'), stringsAsFactors=FALSE)
# annual data 
annual_met_data <- do.call(rbind, 
        lapply(list.files(WRITE_TO, '-constant_yr', full.names = TRUE), 
               function(x){
    
    dat <- read.csv(x, stringsAsFactors = FALSE)
    met <- gsub(x = basename(x), pattern = "NARR-ED2_met-|-constant_yr.csv", replacement = "")
    dat$met <- met
    dat <- data.table::as.data.table(dat)
    
    data <- data.table::melt.data.table(dat, id.vars = c("year", "met"), variable.name = "variable", 
                                    value.name = "value") 
    data <- data[var_info, on = 'variable']
    data$met <- as.character(data$met)
    return(data)
  }))

write.csv(annual_met_data, file=file.path('ED-outputs/results/constant_annual_met_data.csv'), row.names=FALSE)

# The monthly values 
list.files(file.path(BASE_DIR, 'ED-outputs', 'met_results'), "constant_month", full.names = TRUE) %>%  
  lapply(function(x){
    d <- read.csv(x, stringsAsFactors = FALSE)
    met <- gsub(pattern = '-constant_month.csv|NARR-ED2_met-',replacement = "", basename(x))
    d$met <- met
    return(d)
  }) %>% 
  do.call(what = "rbind") %>% 
  as.data.table() -> 
  monthly_data


monthly_data <- melt.data.table(monthly_data, id.vars = c("time", "met", "month"), variable.name = "variable", 
                                value.name = "value") 
monthly_data <- monthly_data[var_info, on = 'variable']
monthly_data <- na.omit(monthly_data)
monthly_data$month <- factor(x = monthly_data$month, levels = toupper(month.abb), ordered = TRUE)
monthly_data$met <- as.character(monthly_data$met)

mon <- data.table(month = toupper(month.abb), mon_time = 1:12)
monthly_data <- monthly_data[mon, on = 'month']

monthly_data %>% 
  group_by(met, month, mon_time, variable, description, units) %>%  
  dplyr::summarize(value = mean(value)) %>% 
  ungroup  ->  
  monthly_data
  
write.csv(monthly_data, file=file.path('ED-outputs/results/constant_annual_met_data-monthly.csv'), row.names=FALSE)



