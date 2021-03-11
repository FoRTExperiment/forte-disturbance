source(here::here('C.analysis', 'packages.R'))
source(here::here('C.analysis', 'ED_functions.R'))

ED_OUTPUT_DIR <- here::here('ED-outputs')
WRITE_TO      <-  here::here('ED-outputs', 'results')
dir.create(WRITE_TO, showWarnings = FALSE)

         
# Thoughts about this work flow, why doesn't drake trigger when more files were added? 
exp1_met_plan <- drake_plan(exp = "exp-1", 
                        files = list.files(file.path(ED_OUTPUT_DIR, exp), pattern = '.rds', full.names = TRUE), 
                        scns = get_scn_names(files),
                        
                        # Process the monthly values 
                        monthly = rbindlist(mapply(get_monthly_data, path = files, scn = scns, SIMPLIFY = FALSE)),
                        mon_data = target({
                          # TODO 
                          # Parse out the met name, this might want to go into a function
                          monthly <- monthly[, met := stringr::str_sub(scn, -4)]
                          monthly <- monthly[, scn := gsub('.{5}$', '', scn)]
                          
                          fname = file.path(WRITE_TO, paste0(exp, '-mon.csv'))
                          write.csv(monthly, file = fname, row.names = FALSE)
                        }), 
                        
                        # Calculate the annual values 
                        annual = calculate_annual_values(monthly), 
                        an_data = target({
                          # TODO this might need to be made innto a function 
                          # Parse out the met name, this might want to go into a function
                          annual <- annual[, met := stringr::str_sub(scn, -4)]
                          annual <- annual[, scn := gsub('.{5}$', '', scn)]
                          fname = file.path(WRITE_TO, paste0(exp, '-yr.csv'))
                          write.csv(annual, file = fname, row.names = FALSE)
                        })
)     
