# Load helper functions. 
source(here::here('C.analysis', 'packages.R'))
source(here::here('C.analysis', 'ED_functions.R'))
source(here::here('C.analysis', 'metric_functions.R'))

# Define the inputs and output files. 
ED_OUTPUT_DIR <- here::here('ED-outputs')
WRITE_TO      <-  here::here('ED-outputs', 'results')
dir.create(WRITE_TO, showWarnings = FALSE)

# The carbon fluxes to process. 
carbon_flux_var_names <- c("GPP", "NPP", "Rh", "NEP")

# Load the formated ED data. 
file.path(ED_OUTPUT_DIR, "results", "exp-constant-yr.csv") %>%  
  read.csv(stringsAsFactors = FALSE) %>% 
  dplyr::filter(variable %in% carbon_flux_var_names) -> 
  ED_data

# Get the time series of the log ratio aka the disturbance. 
ln_ratio_ts <- calculate_ln_ratio(ED_data)

# Calculate our metrics of stability.
resistance <- get_through_resistance(ED_data)
resilience <- get_resilience(ED_data)
