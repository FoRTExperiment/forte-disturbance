# Define drake plans that are used to process/format the ED experiment results. 

# 0. Set Up ------------------------------------------------------------------
source(here::here('C.analysis', 'packages.R'))
source(here::here('C.analysis', 'functions.R'))

ED_OUTPUT_DIR <- here::here('ED-outputs')

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
  
  # Baseline 
  harvest_0_object =  target(readRDS(file.path(ED_OUTPUT_DIR, "test", "baseline.rds"))), 
  harvest_0_data = target({
    scn_name <- 'harvest_0'
    data <- list(NPP = process_NPP(harvest_0_object, scn_name), 
                 LAI = process_LAI(harvest_0_object, scn_name), 
                 NEP = process_NEP(harvest_0_object, scn_name), 
                 ABG = process_ABG(harvest_0_object, scn_name))
    data
  }), 
  
  # 45 % FoRTE Treatment 
  harvest_45_object = target(readRDS(file.path(ED_OUTPUT_DIR, "exp-1", "harvest_45.rds"))), 
  harvest_45_data = target({
    scn_name <- 'harvest_45'
    data <- list(NPP = process_NPP(harvest_45_object, scn_name), 
                 LAI = process_LAI(harvest_45_object, scn_name), 
                 NEP = process_NEP(harvest_45_object, scn_name), 
                 ABG = process_ABG(harvest_45_object, scn_name))
    data
  }), 
  
  # 65 % FoRTE Treatment 
  harvest_65_object = target(readRDS(file.path(ED_OUTPUT_DIR, "exp-1", "harvest_65.rds"))), 
  harvest_65_data = target({
    scn_name <- 'harvest_65'
    data <- list(NPP = process_NPP(harvest_65_object, scn_name), 
                 LAI = process_LAI(harvest_65_object, scn_name), 
                 NEP = process_NEP(harvest_65_object, scn_name), 
                 ABG = process_ABG(harvest_65_object, scn_name))
    data
  }), 
  
  # 85 % FoRTE Treatment 
  harvest_85_object = target(readRDS(file.path(ED_OUTPUT_DIR, "exp-1", "harvest_85.rds"))), 
  harvest_85_data = target({
    scn_name <- 'harvest_85'
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
# Final Plan --------------------------------------------------------- 
plan <- list(results_test, results_exp1, results_exp1_JULY)

