## FoRTE ED Disturbence: TESTING ED2IN
## Bare-ground, shorter runs, without pines. Also what happens when SLA and Vcmax are adjusted. 

# 0. Set Up  ------------------------------------------------------------------------------------
# Define directories. 
BASE_DIR  <- here::here()
WRITE_TO  <- file.path(BASE_DIR, 'no_pines'); 
dir.create(WRITE_TO, showWarnings = FALSE, recursive = TRUE) # This is the directory to write where the different cases should be set up. 
EVENT_DIR <- file.path(BASE_DIR, 'A.inputs', 'events')

# Packages that can be installed through devtools or CRAN. 
library(fortebaseline) # devtools::install_github('ashiklom/fortebaseline')
library(dplyr)
library(tidyr)
library(readr)
library(udunits2) # Inorder to install on pic on R/3.4.3 had to contact pic (support because it required specific pacakge dependencies.)
library(assertthat)
library(purrr)

# There were some issues installing pecan specfic scripts should be sourced from the pecan projects. 
# See https://github.com/PecanProject/pecan to downlaod peacn 
# git show-branch 
# [develop] Merge pull request #2570 from infotroph/book-headers
# commit b264ce700a1892726a7e4ec21c3187152297a190 
# brach devlop
PECAN_DIR <- '/people/dorh012/forte-workflow/pecan'
assertthat::assert_that(dir.exists(PECAN_DIR))
source(file.path(PECAN_DIR,  'models', 'ed', 'R', 'read_ed2in.R'))
source(file.path(PECAN_DIR,  'models', 'ed', 'R', 'write_ed2in.R'))

# Define the path to the inputs to use
INPUT_DIR <- here::here('A.inputs')

# Define a vector of the ed settings and parameters that could be read into ED2IN file, not the params might take 
# a bit of work to get the formatting to play with the pfts. 
potential_ed_settings  <- c('IYEARA', 'IYEARZ', 'IMONTHA', 'IMONTHZ', 'IDATEA', 'IDATEZ', 'ED_MET_DRIVER_DB', 'EVENT_FILE')
potential_ed_params    <- c("param_id", "name", "c2n_leaf", "clumping_factor", "f_labile", 
                            "fineroot2leaf", "growth_resp_factor", "leaf_reflect_nir",  "leaf_reflect_vis", "leaf_respiration_rate_m2",  
                            "leaf_trans_nir", "leaf_trans_vis", "leaf_turnover_rate", "mort1",                   
                            "mort2", "mort3", "nonlocal_dispersal", "orient_factor", "quantum_efficiency", "r_fract",                 
                            "repro_min_h", "root_respiration_rate", "root_turnover_rate", "seedling_mortality",      
                            "SLA", "stomatal_slope", "Vcmax", "water_conductance")

# Set up the infrastructure for an ed2 run, writes the ED2IN file out. 
# 
# Arguments 
#   case: a dataframe containing information about a specific ed run 
#   input_dir: a directory containing the ed inputs 
#   write_to: the location where the cases should be written to 
#  Returns: writes out the ed directory set up for a single ed case
# 
# Example 
# case <- data.frame(casename = c('test1', 'test2'), IYEARA = 2005, IYEARZ = 2006)
# for(i in 1:nrow(case)){ setup_ed_run(case[i,], write_to = here::here('test')) }
# cd /qfs/people/dorh012/forte-disturbance/test_case/test
# /people/dorh012/ed-source-code/ed_2.2-opt ./ED2IN > run_log.txt
setup_ed_run <- function(case, input_dir = INPUT_DIR, write_to = WRITE_TO){
  
  # Check the case set up 
  assert_that(is.data.frame(case))
  assert_that(nrow(case) ==  1)
  # At a minimum there needs to be a casename column and the rest of the names need to be something 
  # that is a known ED setting. 
  assert_that(any(c('casename' %in% names(case))))
  unrecognized  <- !names(case) %in% c('casename', potential_ed_settings, potential_ed_params, 'data')
  assert_that(sum(unrecognized) == 0, msg = paste0(paste(names(case)[unrecognized], collapse = ', '), ' unrecognized ed settings in case data frame.'))
  
  # Create dir to write the files out to. 
  
  if('param_id' %in% names(case)){
    file_n <- paste0(case[['casename']], '-', case[['param_id']])
  } else{
    file_n <- case[['casename']]
  }
  
  OUTOUT <- file.path(write_to, file_n)
  dir.create(OUTOUT, showWarnings = FALSE, recursive = TRUE)
  
  # If there is data column check to make sure that the contents are all good. 
  if('data'%in% names(case)){
    
    d <- tidyr::unnest(case)
    d <- d[, names(d) %in% potential_ed_params]
    
    # Only if there is paramter information being passed in with the cases.
    # The wrtie_ed2_xml function from frotebasline is very important, not only does it change the 
    # formatting in  prep to write out the config file in the ED format but it also converts the 
    # units of the parameters! 
    config_xml  <- fortebaseline::write_ed2_xml(as_pft_list(d))
    config_path <- file.path(OUTOUT, "config.xml")
    XML::saveXML(config_xml, config_path)
    
  } else {
    
    config_path <- NULL 
  }
  
  # Check to make sure the directory to write the files out to exists 
  assert_that(dir.exists(OUTOUT))
  
  # Read in the tempale ED2IN file. 
  ed2in_template_file <- file.path(INPUT_DIR, "ED2IN-no_pines")
  assert_that(file.exists(ed2in_template_file), msg = paste0('template ED2IN file is missing from ', INPUT_DIR))
  ed2in_template <- read_ed2in(ed2in_template_file) # the read_ed2in if a function defined in read_ed2in.R
  
  # Import the UMBS soil data this function comes from the fortebaseline pacakge 
  # which was set up for the Shiklomanov et al. 2020. 
  soil_data <- fortebaseline::umbs_soil()
  
  # The default ED2 FoRTE ini set up tags. 
  # These values were used in Shiklomanov et al. 2020. 
  ed2in_tags <- list(
    # ED run start and end run dates.
    #IYEARA = 2000, 
    IMONTHA = 6, IDATEA = 1,
    #IYEARZ = 2010, 
    IMONTHZ = 6, IDATEZ = 1,
    
    # The first and last year of met data. 
    METCYC1 = 1900, METCYCF = 2030,
    
    # Site information 
    POI_LAT = 45.5625, POI_LON = -84.6975,
    
    # Define where to write output files to. 
    FFILOUT = file.path(OUTOUT, "analysis"),
    SFILOUT = file.path(OUTOUT, "history"),
    
    # Define the paths to the inputs. 
    VEG_DATABASE = file.path(INPUT_DIR, "EDI", "oge2OLD", "OGE2_"),    # this is the path to the vegetation data base, this data is not relevant  see https://github.com/FoRTExperiment/ed4forte/issues/4
    SOIL_DATABASE = file.path(INPUT_DIR, "EDI", "faoOLD", "FAO_"),     # Path to a  soil data base that contains information about soil texture ect.
    
    LU_DATABASE = file.path(INPUT_DIR, "EDI", "ed_inputs", "glu"),
    THSUMS_DATABASE = file.path(INPUT_DIR, "EDI", "ed_inputs/"),
    ED_MET_DRIVER_DB = file.path(INPUT_DIR, "NARR-ED2", "ED_MET_DRIVER_HEADER"),
    
    # UMBS soil characteristics (from Gough et al. 2010 FEM)
    ISOILFLG = 2,  # A flag that allows the soil characteristics to be defined below. 
    NSLCON = 1,    # A lag indicator that the sand and clay information is defined below. 
    SLXCLAY = 0.01, # Prescribed fraction of clay  [0-1].
    SLXSAND = 0.92, # Prescribed fraction of sand  [0-1].
    # Soil moisture data from Ameriflux
    # See analysis/scripts/soil-moisture.R of the fortebaseline project, https://github.com/ashiklom/fortebaseline
    # This soil information is pulled from the fortebaseline::umbs_soil(). 
    NZG = nrow(soil_data),          # SLZ for each grid cell
    SLZ = soil_data[["depth"]],     # Depth (m) of bottom of soil model grid levels
    SLMSTR = soil_data[["slmstr"]], # Initial soil moisture (fraction of saturation)
    
    # Misc ED parmaters 
    CROWN_MOD = 0, # Specifies how tree crowns are represent in the canopy radiation model the default is 0
    ECONOMICS_SCHEME = 0,  # Temporary variable for testing the relationship amongst traits in the tropics, but required by our version of ED. Default is set to 0.
    IHRZRAD = 0,     # Specifies how horizontal canopy radiation is solved. Default is set to 0. 
    TRAIT_PLASTICITY_SCHEME = 0, # No trait plasticity. Trait parameters for each PFT are fixed.  
    
    # Intergration solver set up
    INTEGRATION_SCHEME = 1, # Runge–Kutta integration solver
    RK4_TOLERANCE = 1e-2,   # Faster
    
    # Output file set up (0 means no 3 means HDF5 output)
    IMOUTPUT = 3,      # Return monthly means, 1 HDF5 file per month
    IOOUTPUT = 0,      # Observation time output, turned off 
    MONTH_YRSTEP = 7,  # Month in which the yearly time step (patch dynamics) should occur, the default is set to 7
    IGOUTPUT = 0       # If IHRZRAD is not 0 then write patch table and gap relization files. 
  )
  
  if(!is.null(config_path)){
    config_tags <- list(IEDCNFGF = config_path)
  } else {
    config_tags <- NULL
  }
  d         <- unnest(case)
  from_case <- distinct(d[names(d) %in%  potential_ed_settings])
  
  if(ncol(from_case) > 0){
    from_case_tags <- as.list(from_case)
  } else{
    from_case_tags <- NULL
  }
  
  ed2in_tags <- append(append(ed2in_tags, config_tags), from_case_tags)
  
  # Modify the ED2IN file with the updated information
  ed2in <- modifyList(ed2in_template, ed2in_tags)
  
  # Write the ed2 in file out 
  file <- file.path(OUTOUT, "ED2IN")
  write_ed2in(ed2in, file, barebones = TRUE) 
  file
  
}


# Write a sh script that will launch a set of ed runs/cases
# 
# Arguments 
#   dir: the location of the different cases that have been set up and are ready to run
#   ed_exe: the path to the ed executable to run
# Returns: the path to the sh sript that launches a  series of ed runs
# setup_sh(here::here('test'))
setup_sh <- function(dir, ed_exe = '/people/dorh012/ed-source-code/ed_2.2-opt'){
  
  # Check the inputs
  assert_that(file.exists(ed_exe))
  assert_that(dir.exists(dir))
  
  d <- list.dirs(dir, full.names = TRUE)
  d <- d[d != dir]
  lines <- list('#!/bin/bash', '')
  
  for(i in seq_along(d)){
    lines <- append(lines, paste0('cd ', d[[i]] ))
    lines <- append(lines, list(paste0(ed_exe, ' ./ED2IN > run_log.txt'), ''))
  }
  lines <- unlist(lines)
  
  out <- file.path(dir, 'run_ed.sh')
  writeLines(text = lines, con = out)
  return(out)
}




# 1. Define ED Runs ------------------------------------------------------------------------------------
# Use the case data frame to set up the different runs with the 0-45-65-85 harvest of the C storage pool. 

# Start by defining the ED run information. 
IYEARA <- 1900
IYEARZ <- 1950

### A. Baseline Run ####
case <- data.frame(casename = 'baseline', IYEARA = IYEARA, IYEARZ = IYEARZ)

for(i in 1:nrow(case)){ 
  setup_ed_run(case[i,], write_to = WRITE_TO) 
}

### B. What happens when ED uses UMBS SLA  ####
param_id <- 1 # there is only one param id because configuration is only for a single run. 
name     <- c('umbs.early_hardwood', 'umbs.late_hardwood', 'umbs.mid_hardwood')
SLA      <- c(19.984, 10.020, 23.000)
umbs_params <- data.frame(param_id = param_id,  name = name, SLA = SLA)

# Save all of the ED information as a single data frame. 
data.frame(casename = 'UMBS_SLA', IYEARA = IYEARA, IYEARZ = IYEARZ) %>% 
  cbind(umbs_params)  %>%  
  tidyr::nest(c(-casename, -param_id)) ->
  case

# For each entry in the case data frame generate the ed run set up. 
for(i in 1:nrow(case)){ 
  setup_ed_run(case[i,], write_to = WRITE_TO) 
}

### C. What happens when ED uses UMBS Vcmax #####
cases  <- c('UMBS_Vcmax')

param_id <- 1 # there is only one param id because configuration is only for a single run. 
name     <- c('umbs.early_hardwood', 'umbs.late_hardwood', 'umbs.mid_hardwood')
Vcmax      <- c(64.95, 62.60, 47.30)
umbs_params <- data.frame(param_id = param_id,  name = name, Vcmax = Vcmax)

# Save all of the ED information as a single data frame. 
data.frame(casename = cases, IYEARA = IYEARA, IYEARZ = IYEARZ) %>% 
  cbind(umbs_params)  %>%  
  tidyr::nest(c(-casename, -param_id)) ->
  case

# For each entry in the case data frame generate the ed run set up. 
for(i in 1:nrow(case)){ 
  setup_ed_run(case[i,], write_to = WRITE_TO) 
}


### D. What happens when ED uses both Vcmax and SLA? #####
param_id <- 1 # there is only one param id because configuration is only for a single run. 
name     <- c('umbs.early_hardwood', 'umbs.late_hardwood', 'umbs.mid_hardwood')
Vcmax      <- c(64.95, 62.60, 47.30)
umbs_params <- data.frame(param_id = param_id,  name = name, Vcmax = Vcmax, SLA = SLA[1:3])

# Save all of the ED information as a single data frame. 
data.frame(casename = 'UMBS_Vcmax-SLA', IYEARA = IYEARA, IYEARZ = IYEARZ) %>% 
  cbind(umbs_params)  %>%  
  tidyr::nest(c(-casename, -param_id)) ->
  case

# For each entry in the case data frame generate the ed run set up. 
for(i in 1:nrow(case)){ 
  setup_ed_run(case[i,], write_to = WRITE_TO) 
}


# 2. Create sh! ------------------------------------------------------------------------------------
setup_sh(WRITE_TO)


