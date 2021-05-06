# Define the functions used to calculate the metrics of ecosystem stability. 


# Add the severity label names to make is so that it is easy to use the FORTE color palates 
# Args 
#   dt: data frame of FoRTE data must have a scn column
add_severity_lables <- function(dt){
  
  out <- dt[ , severity := gsub(pattern = "harvest_|_1day_above", replacement = "", x = scn)]
  out$severity <- paste0(out$severity, " %")
  return(out)
  
}


# There is a problem some negative values, or also 0 values... this needs to be addressed some how! 
# Calculate the log ratio for disturbance this is the data that is used to calcualte the dimensions of stability metrics 
# Args 
#   d: processed monthly/annual ED results, must contain a baseline scenario.
# Return: a data frame with the value now beinng the log(treatment/baseline)
calculate_ln_ratio <- function(d){
  
  d <- data.table::as.data.table(d)
  
  # Check inputs 
  req_names <- c('scn', 'year', 'variable', 'description', 'unit', 'value', 'met')
  assert_that(all(has_name(d, req_names)))
  
  baseline_scn <- "harvest_0" 
  assert_that(baseline_scn %in% d$scn)
  
  # Separate the control or baseline scneario values from the output from the other 
  # treatment groups. 
  baseline_values <- d[scn == baseline_scn][ , c("year", "variable", "value", "met")]
  names(baseline_values) <- c("year", "variable", "baseline", "met")
  treatment_values <- d[scn != baseline_scn][ , c("scn", "year", "variable", "description", 
                                                  "unit", "treatment" = "value", "met")]
  names(treatment_values) <- c("scn", "year", "variable", "description", 
                               "unit", "treatment", "met")
  
  # Combine the control and the treatment values as a wide df, then take the 
  # ln(treatment/control) per year / variable / met realization. 
  disturbance_d <- treatment_values[baseline_values, on = c("year", "variable", "met")]
  disturbance_d$year <- disturbance_d$year - 2019
  disturbance_d <- disturbance_d[year >= 0]
  disturbance_d <- disturbance_d[ , value := log(treatment/baseline)]
  disturbance_d <- add_severity_lables(disturbance_d)
  
  disturbance_d %>% 
    select(-treatment) %>% 
    mutate(uint = 'ln(ratio)')
  
  return(disturbance_d)
}



# Calculate the resistance from the trough and when that trough occurs 
# Args
#   data: processed monthly/annual ED results. 
# Return: a data frame of the trough resistance value 
get_through_resistance <- function(data){
  
  data <- as.data.table(data)
  d <- calculate_ln_ratio(data)
  
  dd <- d[, .(year, scn, variable, met, value)]
  info <- unique(data[, .(variable, description)])
  
  rslt <- do.call(what = "rbind", 
                  args = split(dd, interaction(dd$scn, dd$variable, dd$met), drop = TRUE) %>% 
                    lapply(function(x){
                      index <- which.min(x$value)
                      out <- x[index, ]
                      out <- out[ , .(scn, trough_year = year, variable, met, trough_resistance = value)]
                      return(out)
                    }))
  
  out <- rslt[info, on = "variable"]
  out$unit <- "unitless"
  out <- add_severity_lables(out)
  
  return(out)
}


# Get data fits 
# Args 
#   d: a data frame for a single met/varaible/treatment. 
# return: a list of the slopes 
fit_data <- function(d){
  
  x <- d$year
  y <- d$value
  
  lin.mod <- lm(y ~ x)
  segmented.mod <- lin.mod
  
  tryCatch({
    segmented.mod <- segmented(lin.mod, seg.Z = ~x)
  }, 
  error = function(e){lin.mod})
  
  out <- list("lin.mod" = lin.mod, "seg.mod" = segmented.mod)
  return(out)
  
}

# Extract the slope from the list of teh fits 
# Args 
#   l: a list of the data frames 
#   n: the list of the slopes freates by fit_data 
# Return: a data frame of the slope fits
extract_fit_data <- function(l, n){
  
  ln_slope <- l[["lin.mod"]]$coefficients[2]
  
  # If it was determined that there was no segment
  if(length(l[["seg.mod"]]) == 12){
    seg_slope1 <- l[["seg.mod"]]$coefficients[2]
    seg_slope2 <- NA
    seg_brk <- NA 
    
  } else {
    slopes <- slope(l[["seg.mod"]])
    seg_slope1 <- slopes$x[1,1] # Extract the slope of the first segment
    seg_slope2 <- slopes$x[2,1] # Extract the slope of the second segment
    seg_brk <- l[["seg.mod"]]$psi[1,3] # Extract the year of the break point 
  }
  
  # Extract the data about the fit. 
  info <- unlist(strsplit(n, split = "~"))
  
  # Format the slope into a data table 
  dt <- data.table(scn = info[1], 
                   variable = info[2], 
                   met = info[3],
                   lin_slope = ln_slope, 
                   seg_slope1 = seg_slope1, 
                   seg_slope2 = seg_slope2, 
                   seg_brk = seg_brk) 
  
}


# Calculate the resilience
# Args 
#   d: ED output object 
# return: adata frame of resilience values for eacch treatment/variable/met.
get_resilience <- function(d){
  
  d <- as.data.table(d)
  disturbance <- calculate_ln_ratio(d=d)
  resistance <- get_through_resistance(d=d)
  
  # Get the data that will be used to calculate the resilience.
  # We will only use the data from after the 
  disturbance %>% 
    left_join(select(resistance, met, variable, severity, trough_year),     
              by = c("variable", "met", "severity")) %>% 
    dplyr::filter(year >= trough_year) %>% 
    dplyr::mutate(year = year - trough_year) %>% 
    select(-trough_year) -> 
    data_for_resilience
  
  # Organize the data into a list to make is easy to apply the get fit functions to.
  df_list <- split(data_for_resilience, 
                   interaction(data_for_resilience$scn, data_for_resilience$variable, 
                               data_for_resilience$met, sep = "~"), 
                   drop = TRUE)
  
  fits <- lapply(df_list, fit_data)
  
  # TODO there might be an issue with the resillience calculation, something 
  # is up because it returns ann NA value. 
  mapply(FUN = extract_fit_data, l = fits, n = names(fits), SIMPLIFY = FALSE) %>% 
    do.call(what = "rbind") %>%  
    add_severity_lables() %>%  
    select(scn, severity, met, variable, resilience = seg_slope1) %>% 
    # TODO figure out where this is coming from 
    filter(!is.na(resilience))
}







