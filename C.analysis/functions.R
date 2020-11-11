# Custom functions for the drake workflow that is used to process the 
# monthly ED output files into data frames that are easy to work with. 


# Add the ED pft names of the data table 
#   Args  
#     dt: the data table of ED information, it must have PFT column 
#   Return: a data table with the pft names 
add_pft_names <- function(dt){
  
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

# KALYN DOING THIS ONE WRONG, IT IS SUPPOSED TO BE THE OTHER WAY 
# Add the UMBS species and the pft name to a ED data table
#   Args 
#     dt: a data table of ED output that contains the PFT column
# Return: a data table of ED output with the PFT names and UMBS species 
# UMBS_to_ED <- function(dt){
#   
#   # Check the inputs 
#   assert_that(is.data.table(dt))
#   assert_that('PFT' %in% names(dt), msg = 'PFT column is missing from the dt')
#   
#   # A Data table that maps the ED PFT to the UMBS species 
#   mid_success       <- data.table(pft = 10, UMBS = c('ACRU','ACPE',  'FAGR', 'ACSA'), stringsAsFactors = FALSE)
#   temp_broadl_late  <- data.table(pft = 11, UMBS = c('QURU', 'TCSA'), stringsAsFactors = FALSE)
#   nor_pines         <- data.table(pft = 6, UMBS = c('PIST', 'PIRE'), stringsAsFactors = FALSE)
#   temp_broadl_early <- data.table(pft = 9, UMBS = c("AMEL", "POGR", "BEPA", "BEAL", "POTR"), stringsAsFactors = FALSE)
#   
#   umbs_ed <- rbind(mid_success, temp_broadl_late, nor_pines, temp_broadl_early)
#   
#   # If needed add the pft names to the data table. 
#   if(!'pft_name' %in% names(dt)){
#     dt <- add_pft_names(dt)
#   }
#   
#   
# }

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


# Process ED NPP output 
#   Args 
#     ed_object: an object returned by ED
#     scn_name: string of the scenario name
#   Return: a data frame of annual NPP per area 
process_NPP <- function(ed_object, scn_name){
  
  # Make sure that the object being read is ED output. 
  req_names <- c("basename", "df_scalar", "df_cohort", "df_soil", "df_pft", "outdir")
  missing_names <- req_names %in% names(ed_object)
  assert_that(all(missing_names), msg = 'Not an ED object')
  
  # Extract the NPP data and add the pft names. 
  out <- add_pft_names(as.data.table(ed_object$df_cohort[[1]])[  ,list(datetime, MMEAN_NPP_CO, PFT, DBH, NPLANT)])
  
  # Format the data frame
  out$year     <- year(out$datetime)
  out$month    <- month(out$datetime)
  out$variable <- 'MMEAN_NPP_CO'
  
  # Add the ED variable information
  out <- add_var_info(out)
  
  # Convert the NPP units from per plant to unit area
  out$value <- out$MMEAN_NPP_CO * out$NPLANT
  out$unit  <- gsub(pattern = '/pl', replacement = '/m2', out$unit)
  
  # Aggregate to annual NPP value
  out     <- out[, list(value = mean(MMEAN_NPP_CO, na.rm = TRUE)), by = list(pft_name, year, variable, unit, description)]
  out$scn <- scn_name
  return(out)
  
}


# Process ED NEP output 
#   Args 
#     ed_object: an object returned by ED
#     scn_name: string of the scenario name
#   Return: a data frame of annual NEP 
process_NEP <- function(ed_object, scn_name){
  
  # Make sure that the object being read is ED output. 
  req_names <- c("basename", "df_scalar", "df_cohort", "df_soil", "df_pft", "outdir")
  missing_names <- req_names %in% names(ed_object)
  assert_that(all(missing_names), msg = 'Not an ED object')
  
  # Extract the NEP data, because it is reported on the cohort level we do not need to add the PFT name. 
  NEP <- as.data.table(ed_object$df_scalar[[1]])[  ,list(datetime, MMEAN_NEP_PY)]
  
  # Format the NEP data frame 
  NEP$datetime <- ymd(NEP$datetime)
  NEP$year     <- year(NEP$datetime)
  NEP$month    <- month(NEP$datetime)
  NEP$variable <- 'MMEAN_NEP_PY'
  
  # Add the ED variable information
  NEP <- add_var_info(NEP)
  
  # Calculate the annual NEP
  NEP <- NEP[, list(value = mean(MMEAN_NEP_PY, na.rm = TRUE)), by = list(year, variable, unit, description)]
  NEP$scn <- scn_name
  return(NEP)
  
}


# Process ED LAI output 
#   Args 
#     ed_object: an object returned by ED
#     scn_name: string of the scenario name
#   Return: a data frame of monthly LAI by pft
process_LAI <- function(ed_object, scn_name){
  
  # Make sure that the object being read is ED output. 
  req_names <- c("basename", "df_scalar", "df_cohort", "df_soil", "df_pft", "outdir")
  missing_names <- req_names %in% names(ed_object)
  assert_that(all(missing_names), msg = 'Not an ED object')
  
  # Get the LAI data 
  LAI <- as.data.table(ed_object$df_pft[[1]])[  ,list(datetime, value = MMEAN_LAI_PY, PFT = pft)]
  
  # Determine which PFTs to keep based on the data returned in the PFTs beccause the df_pfts 
  # values will always report all of the values. Use this information to subset the 
  # LAI pfts. 
  pft_to_keep <- unique(ed_object$df_cohort[[1]][['PFT']])
  LAI <- LAI[PFT %in% pft_to_keep, ]
  
  LAI$datetime <- ymd(LAI$datetime)
  LAI$year     <- year(LAI$datetime)
  LAI$month    <- month(LAI$datetime)
  LAI$variable <- 'MMEAN_LAI_CO'
  
  # Add the variable information 
  LAI <- add_var_info(LAI)
  LAI <- add_pft_names(LAI)
  
  LAI$scn <- scn_name
  return(LAI)
  
}


# Process ED above ground biomass 
#   Args 
#     ed_object: an object returned by ED
#     scn_name: string of the scenario name
#   Return: a data frame of monthly AGB by pft
process_ABG <- function(ed_object, scn_name){
  
  # Make sure that the object being read is ED output. 
  req_names <- c("basename", "df_scalar", "df_cohort", "df_soil", "df_pft", "outdir")
  missing_names <- req_names %in% names(ed_object)
  assert_that(all(missing_names), msg = 'Not an ED object')
  
  AGB <- as.data.table(ed_object$df_cohort[[1]])[  ,list(datetime, value = AGB_CO, PFT, DBH, NPLANT)]
  
  # Add a unique identifier for each of the cohorts (reminder that cohorts = unique species at DBH)
  split(AGB, AGB$datetime) %>%  
    lapply(function(x){
      
      x$CO <- LETTERS[1:nrow(x)]
      return(x)
      
    }) %>% 
    rbindlist() -> 
    AGB
  
  # Add the PFT names 
  AGB <- add_pft_names(AGB)
  
  # Format the data table
  AGB$year  <- year(AGB$datetime)
  AGB$month <- month(AGB$datetime)
  AGB$variable <- "AGB_CO"
  
  # Add the variable name 
  AGB <- add_var_info(AGB)
  
  # Convert from a per plant to per area value. 
  AGB$value <- AGB$NPLANT * AGB$value
  AGB$unit  <- gsub(pattern = '/plant', replacement = '/m2', AGB$unit)
  AGB$scn   <- scn_name 
  
  return(AGB)
  
}


# From that the list from by scn to by variable
#   Args 
#     name: the name of the data frame to sort by 
#     to_merge: a list of the ed output lists to save.
#   Return: a data frame of the ED output results 
ed_format_list <- function(name, to_merge){
  
  assert_that(is.list(to_merge))
  assert_that(length(to_merge) >= 1, msg = 'more input files needed to merge')
  assert_that(name %in% names(to_merge[[1]]))
  
  # Create an empty data table and select name of the list to save 
  out <- data.table()
  for(i in to_merge){
    out <- rbind(out, i[[name]])
  }
  
  return(out)
  
}

