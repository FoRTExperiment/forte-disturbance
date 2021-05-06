# Custom functions for the drake workflow that is used to process the 
# monthly ED output files into data frames that are easy to work with. 


# Add the ED pft names of the data table 
#   Args  
#     dt: the data table of ED information, it must have PFT column 
#   Return: a data table with the pft names 
add_pft_fullnames <- function(dt){
  
  # Check the inputs 
  assert_that(is.data.table(dt))
  assert_that('PFT' %in% names(dt), msg = 'PFT column is missing from the dt')
  
  # Data table of the plant functional types in ED. 
  pft_names <- data.table(pft = c(6, 8, 9, 10, 11, 7) , 
                          pft_name = c('Northern North American pines', 
                                       'Late-successional North American conifers', 
                                       'Temperate broadleaf, early successional',  
                                       'Temperate broadleaf, mid-successional', 
                                       'Temperate broadleaf, late successional', 
                                       'Southern North American pines'))
  
  
  # Add the pft names to the pft data table. 
  named_dt <- na.omit(dt[pft_names, on = c("PFT" = "pft")])
  return(named_dt)
  
}


# Add FoRTE variable information to the data table 
#   Args
#     dt: a data table of ED output 
# Return: a data table of ED output with the the full variable name and unit information
add_var_info <- function(dt){
  
  # Check inputs 
  assert_that(is.data.table(dt))
  assert_that('variable' %in% names(dt), msg = 'dt is missing the column named variable')
  assert_that(unique(dt[['variable']]) %in% ed4forte::ed2_variable_info()[['variable']], msg = 'dt variable is missing from ed4forte::ed2_variable_info')
  
  # Add the variable information to the data table. 
  dt <- dt[as.data.table(ed4forte::ed2_variable_info())[, list(variable, description, unit)], on = 'variable',  nomatch=0]
  return(dt)
  
}


# Label the cohorts 
# Args 
#   dt: the data table of the cohort data 
# Return: a data frame of the data with cohort information 
add_cohort_labels <- function(dt){
  
  req_names <- c("datetime", "PFT", "pft_name", "variable", "description", "unit", "value")
  assert_that(all(req_names %in% names(dt)), msg = 'data table missing required variables')
  
  # Use the data.table syntax to determine the corhort label per entry in each datetime/PFT. Note 
  # that you cannot include value in the grouping information because each cohort refers to the 
  # unique values at each time step. 
  dt_co <- dt[ , .(cohort = LETTERS[1:.N]), by = c("datetime", "PFT", "pft_name", "variable", "description", "unit")]
  dt_co$value <- dt$value # Add the values 
  
  return(dt_co)
  
}

# Assert that the object is an ed object 
# Args 
#   object: a list created from ED hdf5 files 
# Return: Issues an error message of if object is not an ED object otherwise returns nothing
assert_ed_object <- function(object){
  
  # Make sure that the object being read is ED output. 
  req_names <- c("basename", "df_scalar", "df_cohort", "df_soil", "df_pft", "outdir")
  missing_names <- req_names %in% names(object)
  assert_that(all(missing_names), msg = 'Not an ED object')
  
  return(NULL)
}


# Process the monthly NPP values 
# Args 
#   object: the object returned by ED 
#   scn: a string containing the sceanrio name 
# Return: a data frame of monthly NPP values per PFT/CO. 
monthly_NPP <- function(object, scn){
  
  assert_ed_object(object)
  assert_that(is.string(scn))
  
  # Create a data frame of the NPP values 
  out <- as.data.table(object$df_cohort[[1]])[  ,list(datetime, value = MMEAN_NPP_CO, PFT, DBH, NPLANT)] #Extract from the ED output object
  out <- add_pft_fullnames(out)  # Add the full name for each PFT    
  out$variable <- 'MMEAN_NPP_CO' # Add the variable name 
  out <- add_var_info(out)       # Add the variable information 
  
  
  # Convert the units from per year to a monthly value 
  out$value <- out$value / 12 
  out$unit <- gsub(pattern = '/yr', replacement = " month-1", x = out$unit)
  out$description <- gsub(pattern = ' mean - ', replacement = ' ', x = out$description)
  
  # Convert the NPP units from per plant to unit area
  out$value <- out$value * out$NPLANT
  out$unit  <- gsub(pattern = '/pl', replacement = ' m-2', out$unit)
  
  # Add the cohort labels! 
  out <- add_cohort_labels(out)
  
  # Add the scenario name and change the variable name to reflect the data better
  out <- cbind(scn = scn, out)
  out$variable <- 'NPP'
  
  # Convert from kgC m-2 to MgC ha-1
  out$value <- out$value * 10 
  out$unit <- gsub(pattern = "kgC m-2", replacement = "MgC ha-1", x = out$unit)
  
  return(out)
}


# Process the monthly NEP values 
# Args 
#   object: the object returned by ED 
#   scn: a string containing the sceanrio name 
# Return: a data frame of monthly NEP per ED patch aka plot. 
monthly_NEP <- function(object, scn){
  
  # Check the inputs 
  assert_ed_object(object)
  assert_that(is.string(scn))
  
  # Create a data frame of the NEP values 
  out <- as.data.table(object$df_scalar[[1]])[  ,list(datetime, value = MMEAN_NEP_PY)]
  out$variable <- 'MMEAN_NEP_PY' # Add the variable name 
  out <- add_var_info(out)       # Add the variable information 
  # Because NEP data is reported on the patch level add NA for the PFT and the cohort groups
  out$PFT    <- NA
  out$cohort <- NA

  # Convert to a monthly flux value 
  out$value <- out$value / 12
  out$unit <- gsub(pattern = '/yr', replacement = " month-1", x = out$unit)
  out$description <- gsub(pattern = ' mean - ', replacement = ' ', x = out$description) 
  
  # Add the scenario name 
  out <- cbind(scn = scn, out)
  out$variable <- 'NEP'

  # Convert from kgC m-2 to MgC ha-1
  out$value <- out$value * 10 
  out$unit <- gsub(pattern = "kg/m2", replacement = "MgC ha-1", x = out$unit)
  
  return(out)
}


# Process the monthly GPP values 
# Args 
#   object: the object returned by ED 
#   scn: a string containing the sceanrio name 
# Return: a data frame of monthly GPP values per PFT/CO.
monthly_GPP <- function(object, scn){
  
  assert_ed_object(object)
  assert_that(is.string(scn))
  
  
  # Create a data frame of the GPP values 
  out <- as.data.table(object$df_cohort[[1]])[  ,list(datetime, value = MMEAN_GPP_CO, PFT, DBH, NPLANT)] #E xtract from the ED output object
  out <- add_pft_fullnames(out)  # Add the full name for each PFT    
  out$variable <- 'MMEAN_GPP_CO' # Add the variable name 
  out <- add_var_info(out)       # Add the variable information 
  
  
  # Convert the units from per year to a monthly value 
  out$value <- out$value / 12 
  out$unit <- gsub(pattern = '/yr', replacement = " month-1", x = out$unit)
  out$description <- gsub(pattern = ' mean - ', replacement = ' ', x = out$description)
  
  # Convert the NPP units from per plant to unit area
  out$value <- out$value * out$NPLANT
  out$unit  <- gsub(pattern = '/pl', replacement = ' m-2', out$unit)
  
  # Add the cohort labels! 
  out <- add_cohort_labels(out)
  
  # Convert from kgC m-2 to MgC ha-1
  out$value <- out$value * 10 
  out$unit <- gsub(pattern = "kgC m-2", replacement = "MgC ha-1", x = out$unit)
  
  # Add the scenario name 
  out <- cbind(scn = scn, out)
  out$variable <- 'GPP'

  return(out)
}


# Process the monthly Rh values 
# Args 
#   obect: the ED object 
#   scn: a string containing the scenario name 
# Return: a data frame of the monthly Rh vluaes 
monthly_Rh <- function(object, scn){
  
  assert_ed_object(object)
  assert_that(is.string(scn))
  
  # Create a data frame of the Rh values 
  out <- as.data.table(object$df_scalar[[1]])[  ,list(datetime, value = MMEAN_RH_PY)] #E xtract from the ED output object
  out$variable <- 'MMEAN_RH_PY' # Add the variable name 
  out <- add_var_info(out)       # Add the variable information 
  
  # Convert the units from per year to a monthly value 
  out$value <- out$value / 12 
  out$unit <- gsub(pattern = '/yr', replacement = " month-1", x = out$unit)
  out$description <- gsub(pattern = ' mean - ', replacement = ' ', x = out$description)
  
  # Convert from kgC m-2 to MgC ha-1
  out$value <- out$value * 10 
  out$unit <- gsub(pattern = "kg/m2", replacement = "MgC ha-1", x = out$unit)
  
  # Add the scenario name 
  out <- cbind(scn = scn, out)
  out$variable <- 'Rh'
  
  return(out)
  
}


monthly_Rs <- function(object, scn){
  
  assert_ed_object(object)
  assert_that(is.string(scn))
  
  
  # Rs is the sum of the Rh and the root respiration!
  
  # Create a data frame of the Rh values 
  rh <- as.data.table(object$df_scalar[[1]])[  ,list(datetime, rh = MMEAN_RH_PY)] #E xtract from the ED output object
  root <- as.data.table(object$df_scalar[[1]])[  ,list(datetime, root = MMEAN_ROOT_RESP_PY)] #E xtract from the ED output object
  
  rh[root, on = .(datetime)] %>% 
    # Based on the entries in the ed2 variable info table we know that rh and root respraition 
    # have the same units so they can be added together to get the Rs per tim step. 
    .[, value := rh + root] %>% 
    # Because Rs is not output returned by ED we are not going to be able to use 
    # a function to add the unit and description info. Instead it has to be done manually. 
    .[, variable := "Rs"] %>%  
    .[, unit := "kg/m2/yr"] %>% 
    .[, description := "Monthly Soil respiration"] -> 
    out 

  # Convert the units from per year to a monthly value 
  out$value <- out$value / 12 
  out$unit <- gsub(pattern = '/yr', replacement = " month-1", x = out$unit)
  out$description <- gsub(pattern = ' mean - ', replacement = ' ', x = out$description)
  
  # Convert from kgC m-2 to MgC ha-1
  out$value <- out$value * 10 
  out$unit <- gsub(pattern = "kg/m2", replacement = "MgC ha-1", x = out$unit)
  
  # Add the scenario name 
  out <- cbind(scn = scn, out)
  return(out)
}




# Process the monthly LAI 
# Args 
#   object: the object returned by ED 
#   scn: a string containing the sceanrio name 
# Return: a data frame of monthly LAI values per PFT 
# monthly_LAI <- function(object, scn){
#   
#   assert_ed_object(object)
#   assert_that(is.string(scn))
#   
#   # Get the LAI data 
#   out <- as.data.table(object$df_pft[[1]])[  , list(datetime, value = MMEAN_LAI_PY, PFT = pft)]
#   out$variable <- 'MMEAN_LAI_PY'
#     
#   # Determine which PFTs to keep based on the data returned in the PFTs beccause the df_pfts 
#   # values will always report all of the values. Use this information to subset the 
#   # LAI pfts. 
#   pft_to_keep <- unique(object$df_cohort[[1]][['PFT']])
#   out <- out[PFT %in% pft_to_keep, ]
#   
#   # Add the variable, pft name, and scenario name  
#   cbind(scn = scn, out) %>%  
#   add_var_info %>% 
#     add_pft_fullnames -> 
#     out
#   
#   out$variable <- "LAI_PFT"
#   out$unit <- paste0(out$unit, " per month")
#   
#   return(out)
#   
# }


# Process the monthly AGB values 
# Args 
#   object: the object returned by ED 
#   scn: a string containing the sceanrio name 
# Return: a data frame of monthly AGB values per PFT/CO.
monthly_AGB <- function(object, scn){
  
  # Check inputs 
  assert_ed_object(object)
  assert_that(is.string(scn))
  
  # Extract the above ground biomass data from the ed object. 
  out <- as.data.table(object$df_cohort[[1]])[  ,list(datetime, value = AGB_CO, PFT, DBH, NPLANT)]
  out <- add_pft_fullnames(out)  # Add the full name for each PFT    
  out$variable <- "AGB_CO"       # Add variable name 
  out <- add_var_info(out)       # Add the variable information 
  
  # Convert from a per plant to per area value. 
  out$value <- out$NPLANT * out$value
  out$unit  <- gsub(pattern = '/plant', replacement = 'm-2', out$unit)
  
  # Add scenario name and cohort labels 
  out <- cbind(scn = scn, add_cohort_labels(out)) 
  out$variable <- "AGB"
  out$unit <- paste0(out$unit, " month-1")
  
  return(out)
  
}


# Process ED monthly data for the variables of intrest 
# Args 
#   path: the file location of the rds file to process 
#   scn: the scenario name 
# Return: a data table of monthly values 
get_monthly_data  <- function(path, scn){
  
  assertthat::assert_that(file.exists(path))
  object <- readRDS(path)
  
    data <- rbind(monthly_NPP(object, scn), 
                #  monthly_LAI(object, scn), 
                  monthly_Rh(object, scn),
                  monthly_NEP(object, scn),
                  monthly_AGB(object, scn), 
                  monthly_GPP(object, scn),
                  monthly_Rs(object, scn), fill = TRUE)
  
  return(data)
}



# From the monthly values calcaulte the annual values 
# Args 
#   dt: the data frame returned by get_monthly_data
# Return: a data frame of the annual values 
calculate_annual_values <- function(dt){
  
  # Check the inputs 
  req_names <- c('scn', 'datetime', 'PFT', 'pft_name', 'variable', 'description', 'unit', 'cohort', 'value')
  assert_that(all(req_names %in% names(dt)))
  
  req_vars <- c('NPP', 'NEP', 'GPP', 'AGB', "Rh", "Rs")
  assert_that(all(req_vars %in% dt$variable))
  
  # Determine which years have 12 months of data 
  dt$year  <- year(dt$datetime)
  dt$month <- month(dt$datetime)
  dt[ , list(year, month)] %>% 
    .[ , .(month_count = uniqueN(month)), by = c("year")] %>% 
    .[month_count == 12] ->
    complete_years
  
  # Only process the data that had 12 months of data
  dt <- dt[year %in% complete_years$year]
  dt$unit <- gsub(pattern = " month-1", replacement = " year-1", x = dt$unit)
  
  # Subset the data for the carbon fluxes and calculate the total flux per year 
  carbon_fluxes <- c('NPP', 'NEP', 'GPP', "Rh", "Rs")
  dt[variable %in% carbon_fluxes] %>% 
    .[ , list(value = sum(value)), by = c("scn", "year", "variable", "description", "unit")] -> 
    annual_carbon
    
  #annual_carbon$variable <- gsub(pattern = "_PFT_CO", replacement = "_patch", x = annual_carbon$variable)
  annual_carbon$description <- gsub(pattern = "Monthly", replacement = "Annual ", x = annual_carbon$description)
  
  
  # Now process the variables that will report and annual average. Note that this will have to 
  # be the sum across the cohorts and plant functionn types and then an annual average. 
  annual_avg_vars <- c("AGB") 
  dt[variable %in% annual_avg_vars] %>%  
    # Start by calculating the patch total
    .[ , list(value = sum(value)), by = c("scn", "year", "month", "variable", "description", "unit")]  %>% 
    # Now calcualte the annual average 
    .[ , list(value = mean(value)), by = c("scn", "year", "variable", "description", "unit")]  -> 
    annual_avg
  
  # Update the unit information 
  annual_avg$variable <- gsub(pattern = "_PFT_CO", replacement = "_patch", x = annual_avg$variable)
  annual_avg$description <- paste('Annual',  annual_avg$description, sep = ' ')
  
  # Combine the annual results into a singel data table 
  out <- rbind(annual_carbon, annual_avg)
  
  return(out)
  }


# Parse out the scenario name from the files 
# Args 
#   f: a vector of rds files 
# Return: a vector string of the scenario name
get_scn_names <- function(f){
  
  # Make sure that all the files actually exist & that they are 
  # all .rds files
  assert_that(all(file.exists(f)))
  assert_that(all(grepl(f, pattern = '.rds')))
  
  scns <- basename(f)
  scns <- gsub(x = scns, pattern = '.rds', replacement = '')
  return(scns)
  
}





