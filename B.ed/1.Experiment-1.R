## FoRTE ED Disturbence: Experiment  1 
## Bare-ground, 100 year run to mature stand, 0-45-65-85 harvest of C storage pool, 
## run for 10 more years, using climatology (average of 5 years of data), ED default parameters
## monthly NPP, NEE, LAI, biomass by cohort later we'll want to run multiple times to average out randomness
## See https://github.com/FoRTExperiment/FoRTE-mgmt/issues/77

# 0. Set Up  ------------------------------------------------------------------------------------
# Define directories. 
BASE_DIR  <- here::here()
WRITE_TO  <- file.path(BASE_DIR, 'exp-1'); dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE) # This is the directory to write where the different cases should be set up. 
EVENT_DIR <- file.path(BASE_DIR, 'A.inputs', 'events')

# Load the required pacakges and functions. 
to_source <- file.path(BASE_DIR, "B.ed", "0A.dependencies_functions.R")
stopifnot(file.exists(to_source))
source(to_source)


# 1. Define ED Runs ------------------------------------------------------------------------------------
# Use the case data frame to set up the different runs with the 0-45-65-85 harvest of the C storage pool. 

# Start by defining the ED run information. 
IYEARA <- 1900
IYEARZ <- 2034

# Find the event files
event_file <- list.files(EVENT_DIR, pattern = '1day_above.xml', full.names = TRUE)
cases      <- gsub(pattern = '.xml', replacement = '', x = basename(event_file))
assertthat::assert_that(all(file.exists(event_file)))
# Important! When reading in the event_file paths they must be written out as a string, 
# otherwise ED will fail but will throw an uninformative error message realted to 
# missing a different ED input. 
event_file <- paste0("'", event_file, "'") 

# Save all of the ED information as a single data frame. 
case <- data.frame(casename = cases, IYEARA = IYEARA, IYEARZ = IYEARZ, EVENT_FILE = event_file)


met_headers <- data.table(ED_MET_DRIVER_DB = c(file.path(INPUT_DIR, "NARR-ED2", "ED_MET_DRIVER_HEADER"), 
                                               file.path(INPUT_DIR, "NARR-ED2", "ED_MET_DRIVER_HEADER_met2")), 
                          met_name = c('met1', 'met2'))

case <- merge(case, met_headers)
case$casename <- paste(case$casename, case$met_name, sep = '_')
case <- case[c("casename", "IYEARA", "IYEARZ", "EVENT_FILE", "ED_MET_DRIVER_DB")]

# For each entry in the case data frame generate the ed run set up. 
for(i in 1:nrow(case)){ 
  setup_ed_run(case[i,], write_to = WRITE_TO) 
  }

# Create the sh script. 
setup_sh(WRITE_TO)

# End
