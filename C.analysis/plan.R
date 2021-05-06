# Load helper functions. 
source(here::here('C.analysis', '0.packages.R'))
source(here::here('C.analysis', '0.func_ED.R'))
source(here::here('C.analysis', '0.func_metric.R'))


# Define the inputs and output files. 
ED_OUTPUT_DIR <- here::here('ED-outputs')
WRITE_TO      <-  here::here('ED-outputs', 'results')
dir.create(WRITE_TO, showWarnings = FALSE)

# Process ED output into csv files ---------------------------------------------------------------------
# Define the drake plan that is used to extract the data from the PECAN (Alexey) formmated 
# ED rds file into the units/resolution (cohort/annnual/monthly) that can be used. 
exp_constant <- drake_plan(exp = "exp-constant", 
            # Determine which files to import. 
            files = list.files(file.path(ED_OUTPUT_DIR, exp), pattern = '.rds', full.names = TRUE), 
            scns = gsub(x = basename(files), pattern ="1day_above_met-|-constant|.rds", replacement = ""),
                                
            # Process the monthly values 
            monthly = rbindlist(mapply(get_monthly_data, path = files, scn = scns, SIMPLIFY = FALSE)),
                                mon_data = target({
                                  monthly <- monthly[, met := stringr::str_sub(scn, -4)]
                                  monthly <- monthly[, scn := gsub('.{5}$', '', scn)]
                                  fname = file.path(WRITE_TO, paste0(exp, '-mon.csv'))
                                  write.csv(monthly, file = fname, row.names = FALSE)
                                }), 
                                
            # Calculate the annual values 
            annual = calculate_annual_values(monthly), 
            an_data = target({
              annual <- annual[, met := stringr::str_sub(scn, -4)]
              annual <- annual[, scn := gsub('.{5}$', '', scn)]
              fname = file.path(WRITE_TO, paste0(exp, '-yr.csv'))
              write.csv(annual, file = fname, row.names = FALSE)
              })
) 

# Get ecosystem metrics of stability -----------------------------------------------------------
metric_dir <-  file.path(WRITE_TO, 'metric')
dir.create(metric_dir, showWarnings = FALSE)

metric_values <- drake_plan(
  # The carbon fluxes to process. 
  carbon_flux_var_names = c("GPP", "NPP", "Rh", "NEP"), 
  ED_data = target({
    # Import the ED data and select the variables to procecss.
    file.path(ED_OUTPUT_DIR, "results", "exp-constant-yr.csv") %>%  
      read.csv(stringsAsFactors = FALSE) %>% 
      dplyr::filter(variable %in% carbon_flux_var_names) 
  }), 
  ln_ratio_ts = target({
    data  <- calculate_ln_ratio(ED_data)
    fname <- file.path(metric_dir, 'ln_ratio_timeseries.csv')
    write.csv(data, file = fname, row.names = FALSE)
  }), 
  # Now calculate the resistance and resilience
  resistance = target({
    data <- get_through_resistance(ED_data)
    fname <- file.path(metric_dir, 'resistance_values.csv')
    write.csv(data, file = fname, row.names = FALSE)
  }), 
  resilience = target({
    data <- get_resilience(ED_data)
    fname <- file.path(metric_dir, 'resilience_values.csv')
    write.csv(data, file = fname, row.names = FALSE)
  })
)


