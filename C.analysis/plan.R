# Define drake plans that are used to process/format the ED experiment results. 

# 0. Set Up ------------------------------------------------------------------
source(here::here('C.analysis', 'packages.R'))
source(here::here('C.analysis', 'functions.R'))

ED_OUTPUT_DIR <- here::here('ED-outputs')

# The helper functions that chain together the 
prep_data <- function(path, scn){
  
  assertthat::assert_that(file.exists(path))
  object <- readRDS(path)
  
  data <- list(NPP = process_NPP(object, scn), 
               LAI = process_LAI(object, scn), 
               NEP = process_NEP(object, scn), 
               ABG = process_ABG(object, scn), 
               GPP = process_GPP(object, scn))
  
  return(data)
}
format_list <- function(l){
  
  assertthat::assert_that(is.list(l))
  list('NPP' = ed_format_list(name = 'NPP', to_merge = l),
       'LAI' = ed_format_list(name = 'LAI', to_merge = l), 
       'NEP' = ed_format_list(name = 'NEP', to_merge = l), 
       'ABG' = ed_format_list(name = 'ABG', to_merge = l), 
       'GPP' = ed_format_list(name = 'GPP', to_merge = l))
}



# 1. Test Results --------------------------------------
results_test <- drake_plan(
  
  # Baseline
  test_baseline_ed_object = target(readRDS(file.path(ED_OUTPUT_DIR, "test", "baseline.rds"))), 
  test_baseline_data = target({
    scn_name <- 'test-baseline'
    data <- list(NPP = process_NPP(test_baseline_ed_object, scn_name), 
                 LAI = process_LAI(test_baseline_ed_object, scn_name), 
                 NEP = process_NEP(test_baseline_ed_object, scn_name), 
                 ABG = process_ABG(test_baseline_ed_object, scn_name))
    data
  }),
  
  # Save Output 
  target({
    out <- list(NPP = ed_format_list('NPP', list(test_baseline_data)),
                LAI = ed_format_list('LAI', list(test_baseline_data)),
                NEP = ed_format_list('NEP', list(test_baseline_data)),
                ABG = ed_format_list('ABG', list(test_baseline_data)))
    saveRDS(object = out, file = file.path(ED_OUTPUT_DIR, "test-data.rds"))
    
  })
)

# 2. Exp 1 ------------------------------------------------------------------------------
results_exp1 <- drake_plan(
  
  DIR = file.path(ED_OUTPUT_DIR, 'exp-1'), 
  
  harvest_0 = target(prep_data(path = file.path(DIR, "harvest_0_above.rds"), scn = 'harvest 0')),
  harvest_45 = target(prep_data(path = file.path(DIR, "harvest_45_above.rds"), scn = 'harvest 45')),
  harvest_65 = target(prep_data(path = file.path(DIR, "harvest_65_above.rds"), scn = 'harvest 65')),
  harvest_85 = target(prep_data(path = file.path(DIR, "harvest_85_above.rds"), scn = 'harvest 85')),
  
  # Save output 
  target({
    out <-format_list(list(harvest_0, harvest_45, harvest_65, harvest_85))
    saveRDS(object = out, file = file.path(ED_OUTPUT_DIR, "exp-1.rds"))
  })
  
)

# 3. July Exp 1 ------------------------------------------------------
# What happens when the disturbance takes place in July instead of at the start of the growing seaon?
results_exp1_JULY <- drake_plan(
  
  # Baseline
  harvest_0_object = target(readRDS(file.path(ED_OUTPUT_DIR, "test", "baseline.rds"))), 
  harvest_0_data = target({
    scn_name <- 'harvest_0-JULY'
    data <- list(NPP = process_NPP(harvest_0_object, scn_name), 
                 LAI = process_LAI(harvest_0_object, scn_name), 
                 NEP = process_NEP(harvest_0_object, scn_name), 
                 ABG = process_ABG(harvest_0_object, scn_name))
    data
  }), 
  
  # 45 % FoRTE Treatment
  harvest_45_object = target(readRDS(file.path(ED_OUTPUT_DIR, "exp-1_JULY", "harvest_45.rds"))), 
  harvest_45_data = target({
    scn_name <- 'harvest_45-JULY'
    data <- list(NPP = process_NPP(harvest_45_object, scn_name), 
                 LAI = process_LAI(harvest_45_object, scn_name), 
                 NEP = process_NEP(harvest_45_object, scn_name), 
                 ABG = process_ABG(harvest_45_object, scn_name))
    data
  }), 
  
  # 65 % FoRTE Treatment 
  harvest_65_object = target(readRDS(file.path(ED_OUTPUT_DIR, "exp-1_JULY", "harvest_65.rds"))), 
  harvest_65_data = target({
    scn_name <- 'harvest_65-JULY'
    data <- list(NPP = process_NPP(harvest_65_object, scn_name), 
                 LAI = process_LAI(harvest_65_object, scn_name), 
                 NEP = process_NEP(harvest_65_object, scn_name), 
                 ABG = process_ABG(harvest_65_object, scn_name))
    data
  }), 
  
  # 85 % FoRTE Treatment 
  harvest_85_object = target(readRDS(file.path(ED_OUTPUT_DIR, "exp-1_JULY", "harvest_85.rds"))), 
  harvest_85_data = target({
    scn_name <- 'harvest_85-JULY'
    data <- list(NPP = process_NPP(harvest_85_object, scn_name), 
                 LAI = process_LAI(harvest_85_object, scn_name), 
                 NEP = process_NEP(harvest_85_object, scn_name), 
                 ABG = process_ABG(harvest_85_object, scn_name))
    data
  }), 
  
  # Save output 
  target({
    out <- list(NPP = ed_format_list('NPP', list(harvest_0_data, harvest_45_data, harvest_65_data, harvest_85_data)),
                LAI = ed_format_list('LAI', list(harvest_0_data, harvest_45_data, harvest_65_data, harvest_85_data)),
                NEP = ed_format_list('NEP', list(harvest_0_data, harvest_45_data, harvest_65_data, harvest_85_data)),
                ABG = ed_format_list('ABG', list(harvest_0_data, harvest_45_data, harvest_65_data, harvest_85_data)))
    saveRDS(object = out, file = file.path(ED_OUTPUT_DIR, "exp-1_JULY.rds"))
    
  })
  
)
# 4. Disturbance Treatments --------------------------------------------------


results_disturbance_treatments <- drake_plan(
  DIR = file.path(ED_OUTPUT_DIR, "disturbence-treatments"),
  
  harvest_0_original = target({object <- readRDS(file.path(DIR, 'harvest_0.rds')) 
  scn_name <- 'harvest 0 original'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }),
  
  harvest_0_uniform = target({object <- readRDS(file.path(DIR, 'harvest_0_uniform.rds')) 
  scn_name <- 'harvest 0 uniform'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_45_original = target({object <- readRDS(file.path(DIR, 'harvest_45.rds')) 
  scn_name <- 'harvest 45 original'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_45_other0 = target({object <- readRDS(file.path(DIR, 'harvest_45_other0.rds')) 
  scn_name <- 'harvest 45 other 0 '
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_45_uniform = target({object <- readRDS(file.path(DIR, 'harvest_45_uniform.rds')) 
  scn_name <- 'harvest 45 uniform'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_65_original = target({object <- readRDS(file.path(DIR, 'harvest_65.rds')) 
  scn_name <- 'harvest 65 original'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_65_other0 = target({object <- readRDS(file.path(DIR, 'harvest_65_other0.rds')) 
  scn_name <- 'harvest 65 other 0 '
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_65_uniform = target({object <- readRDS(file.path(DIR, 'harvest_65_uniform.rds')) 
  scn_name <- 'harvest 65 uniform'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_85_original = target({object <- readRDS(file.path(DIR, 'harvest_85.rds')) 
  scn_name <- 'harvest 85 original'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_85_other0 = target({object <- readRDS(file.path(DIR, 'harvest_85_other0.rds')) 
  scn_name <- 'harvest 85 other 0 '
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }), 
  
  harvest_85_uniform = target({object <- readRDS(file.path(DIR, 'harvest_85_uniform.rds')) 
  scn_name <- 'harvest 85 uniform'
  data <- list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  data
  }),
  
  harvest_0_jan = target(prep_data(path = file.path(DIR, "harvest_0_jan.rds"), scn = 'harvest 0 jan')),
  harvest_45_jan = target(prep_data(path = file.path(DIR, "harvest_45_jan.rds"), scn = 'harvest 45 jan')),
  harvest_65_jan = target(prep_data(path = file.path(DIR, "harvest_65_jan.rds"), scn = 'harvest 65 jan')),
  harvest_85_jan = target(prep_data(path = file.path(DIR, "harvest_85_jan.rds"), scn = 'harvest 85 jan')),
  
  harvest_0_above = target(prep_data(path = file.path(DIR, "harvest_0_above.rds"), scn = 'harvest 0 above')),
  harvest_45_above = target(prep_data(path = file.path(DIR, "harvest_45_above.rds"), scn = 'harvest 45 above')),
  harvest_65_above = target(prep_data(path = file.path(DIR, "harvest_65_above.rds"), scn = 'harvest 65 above')),
  harvest_85_above = target(prep_data(path = file.path(DIR, "harvest_85_above.rds"), scn = 'harvest 85 above')),
  
  
  
  # Save output 
  target({
    out <- format_list(list(harvest_0_original, harvest_45_original, harvest_65_original, harvest_85_original,
                            harvest_0_uniform, harvest_45_uniform, harvest_65_uniform, harvest_85_uniform,
                            harvest_45_other0, harvest_65_other0, harvest_85_other0, 
                            harvest_0_jan, harvest_45_jan, harvest_65_jan, harvest_85_jan, 
                            harvest_0_above, harvest_45_above, harvest_65_above, harvest_85_above))
    saveRDS(object = out, file = file.path(ED_OUTPUT_DIR, "disturbance-treatments.rds"))
    
  })
  
)




# 5. More disturbance treatments -------------------------------------



DIR <- file.path(ED_OUTPUT_DIR, "disturbence-treatments")

files <- list.files(DIR, pattern = '1yr', full.names = TRUE)

sapply(files, function(f){
  
  object <- readRDS(f)
  scn_name <- gsub(x = basename(f), pattern = '.rds', replacement = '')
  list(NPP = process_NPP(object, scn_name), 
               LAI = process_LAI(object, scn_name), 
               NEP = process_NEP(object, scn_name), 
               ABG = process_ABG(object, scn_name))
  
}, simplify = FALSE) -> 
  data_files 

scn_name <- gsub(x = basename(names(data_files)), pattern = '.rds', replacement = '')
names(data_files) <- scn_name
format_list(l = data_files$harvest_0_1yr)

ed_format_list('NPP', list(data_files$harvest_0_1yr, data_files$harvest_65_1yr))



# Final Plan --------------------------------------------------------- 
plan <- list(results_test, results_exp1, results_exp1_JULY)

