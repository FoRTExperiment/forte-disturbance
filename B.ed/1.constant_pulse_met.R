## FoRTE ED Disturbence: met ensemble
## Bare-ground, 100 year run to mature stand constant met with the expection of a different met pulse 
## duirng a 1, 2, 5, and 10 year periods. 
## question is it wrong that during the "pulse" the met is changing and is not held constant. 
## Hmmm what am I trying to answer with this? I guess I am intrested in how long the met has a memory/legacy 
## effect. Let's jsut start with the 1 yr and 5 year run. 

library(data.table)

# Define directories. 
BASE_DIR  <- here::here()
WRITE_TO <- file.path(BASE_DIR, "constant-pulse-met")
dir.create(WRITE_TO)
  
# Load the required pacakges and functions. 
to_source <- file.path(BASE_DIR, "B.ed", "0A.dependencies_functions.R")
stopifnot(file.exists(to_source))
source(to_source)



# 1. Define ED Runs ------------------------------------------------------------------------------------

# Start by defining the ED run information. 
IYEARA <- 1900
IYEARZ <- 2020


# Save all of the ED information as a single data frame. 
case <- data.frame(IYEARA = IYEARA, IYEARZ = IYEARZ)
met_headers <- data.table(ED_MET_DRIVER_DB = c(file.path(INPUT_DIR, "NARR-ED2met1-constant_1yr", "ED_MET_DRIVER_HEADER"), 
                                               file.path(INPUT_DIR, "NARR-ED2met1-constant_5yr", "ED_MET_DRIVER_HEADER")), 
              met_name = c('1_yr', '5_yr'))

case <- merge(case, met_headers)
case$casename <- case$met_name
as.data.frame(case) %>% 
  dplyr::select("casename", "IYEARA", "IYEARZ", "ED_MET_DRIVER_DB") %>%  
  as.data.table -> 
  case

# For each entry in the case data frame generate the ed run set up. 
for(i in 1:nrow(case)){ 
  setup_ed_run(case[i,], write_to = WRITE_TO) 
}

# Create the sh script. 
setup_sh(WRITE_TO)