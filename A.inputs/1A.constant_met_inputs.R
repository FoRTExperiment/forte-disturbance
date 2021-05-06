# Looking at the ED results in the baseline scenario it is somewhat concerning
# that we see this periodicity in the historical period, is that a function of ED
# or is that a function of the meterolgical inputs. Another outstanding question is 
# why the climate ensemble begins to converge. It is a coding problem or a numerical 
# feature of ED? 

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
library(assertthat)


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


# 1. Set up the input files -------------------------------------------------------------------
# Set up the years of data to used 
years <- floor(seq(from = 1979, to = 2019, length.out = 20)) 
tag       <- paste0("_met-", years)
dir_names <- paste0("NARR-ED2", tag)

# Come up with the names of the met dir. 
# met_dirs  <- list.files(path = file.path(BASE_DIR, 'A.inputs'), pattern = paste0(dir_names, collapse = "|"), full.names = TRUE)
met_dirs  <- file.path(BASE_DIR, 'A.inputs', dir_names)

# For each of the met dirs create the input files. 
mapply(FUN = function(dir, y){

  # Make the directory to store the data in. 
  write_to <- paste0(dir, "-constant")
  dir.create(write_to, showWarnings = FALSE)

  # Find the data files to process
  data_files <- list.files(path = MET_DIR, full.names = TRUE)
  data_files <- data_files[grepl(pattern = as.character(y), x = data_files)]
  
  assert_that(length(data_files) == 12)
  
  month <- gsub(x = basename(data_files), pattern = paste0(y,"|.h5"), replacement = "")
  data_to_copy <- data.table(to_copy = data_files, month = month)

  # Make a data frame of the new data files and their month column, this will be joined
  # with the one year of data we are actually using.
  #
  # Make the list of the years we will want in the file names, each year will be reapted
  # 12 times, one for each month.
  yrs    <- rep(1900:2050, each = 12)
  # Make a list of the months, that will be paired with the different years.
  months <- rep(toupper(month.abb), length(yrs) / 12)
  # Format the year and month information into the new h5 file name.
  data.table(year = yrs, month = months) %>%
    # make the new file name based on the year and the month
    .[ , new_file := file.path(write_to, paste0(year, month, ".h5"))] %>%
    .[, .(month, new_file)] ->
    missing_data

  # The data frame of the files to copy and their new location.
  df <- missing_data[data_to_copy, on = "month"][, .(to_copy, new_file)]
  mapply(FUN = file.copy, from = df$to_copy, to = df$new_file,overwrite = TRUE)


  # Now that the files have been copied over we need to generate the new ED MET header!
  out <- modify_met_header(write_to)

  return(out)

}, dir = met_dirs, y = years) ->
  list








  
  









