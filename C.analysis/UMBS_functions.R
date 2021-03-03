

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
