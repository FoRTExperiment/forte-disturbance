# Use this file to set up the harvest event xml files that are used to apply the 
# the harvest FoRTE treatments to the ED runs. 
# 0. Set Up ---------------------------------------------------------------------------
library(xml2)
library(assertthat)
library(lubridate)
library(data.table)

# Define the paths 
BASE_DIR <- here::here()
WRITE_TO <- file.path(BASE_DIR, 'A.inputs', 'events')


# 1. Define functions --------------------------------------------------------------------------------
# Modify an ED harvest xml file 
# Args
#   table: a data frame of the year, doy, agb_frac, bgb_frac, fol_frac, and stor_frac to add to the harvest xml 
#   xml_name: the path to write the xml file out to 
#   xml_template_file: default set to the template in the A.inputs/events file. This is a 0 disturabnce event. 
# Return: the name of the xml file that was created. 
modify_xml <- function(table, xml_name, xml_template_file = here::here('A.inputs', 'events', 'harvest_template.xml')){
  
  # Check the function arguments
  assert_that(has_name(table, c("year", "doy", "agb_frac", "bgb_frac", "fol_frac", "stor_frac")))
  assert_that(sum(is.na(table)) == 0)
  assert_that(all(is.double(unlist(table))))
  assert_that(dir.exists(dirname(xml_name)))
  assert_that(file.exists(xml_template_file))
  assert_that(grepl(pattern = '.xml', x = xml_name))
  
  # Import the xml file 
  xml_template <- read_xml(xml_template_file)
  
  # Extract the childe node, this is the harvest event template that will be used to 
  # expand the template xml so there is a harvest entry for each entry in the table. 
  child_node <- xml_child(xml_template)
  
  # Add sibling nodes to the xml template for all of then entries in the harvest file. 
  # Because the xml file already contains a harvest event node add 1 less node than 
  # listed in the the table. 
  for(index in 1:(nrow(table) - 1)){
    xml_add_sibling(child_node, child_node, where = "after")
  }
  
  # Now that the xml has been modified to the appropriate size convert to a list. 
  xml_list <- as_list(xml_template)
  
  # Now modify 
  for(i in 1:nrow(table)){
    
    xml_list$eventlist[i]$event$year[[1]] <- table$year[i]
    xml_list$eventlist[i]$event$doy[[1]] <- table$doy[i]
    xml_list$eventlist[i]$event$harvest$agb_frac[[1]] <- table$agb_frac[i]
    xml_list$eventlist[i]$event$harvest$bgb_frac[[1]] <- table$bgb_frac[i]
    xml_list$eventlist[i]$event$harvest$fol_frac[[1]] <- table$fol_frac[i]
    xml_list$eventlist[i]$event$harvest$stor_frac[[1]] <- table$stor_frac[i]
    
  }
  
  # Convert list back into an xml file, write out 
  new_xml <- as_xml_document(xml_list)
  write_xml(new_xml, xml_name)
  
  # Return the xml file name 
  return(xml_name)
  
}


# 2. Create 1 year disturbance files ---------------------------------------------------------------------------
# Make 1 year of disturbances table 
ymd_year <- seq(ymd('2019-05-21'), ymd('2020-05-20'), by = 'days')
year <- year(ymd_year)
doy <- yday(ymd_year)

# 0% disturbance 
table <- data.table(year = year, doy = doy, agb_frac = 0, bgb_frac = 0, fol_frac = 0, stor_frac = 0)
xml0 <- file.path(WRITE_TO, 'harvest_0_1yr.xml')
modify_xml(table, xml0)

# 45% disturbance 
table45 <- table
table45$stor_frac <- 0.45
xml45 <- file.path(WRITE_TO, 'harvest_45_1yr.xml')
modify_xml(table45, xml45)

# 65% disturbance 
table65 <- table
table65$stor_frac <- 0.65
xml65 <- file.path(WRITE_TO, 'harvest_65_1yr.xml')
modify_xml(table65, xml65)

# 85% disturbance 
table85 <- table
table85$stor_frac <- 0.85
xml85 <- file.path(WRITE_TO, 'harvest_85_1yr.xml')
modify_xml(table85, xml85)



# 3. Create 1 month disturbance files ---------------------------------------------------------------------------
# Make 1 month of disturbances table 
ymd_year <- seq(ymd('2019-05-21'), ymd('2019-06-21'), by = 'days')
year <- year(ymd_year)
doy <- yday(ymd_year)

# 0% disturbance 
table <- data.table(year = year, doy = doy, agb_frac = 0, bgb_frac = 0, fol_frac = 0, stor_frac = 0)
xml0 <- file.path(WRITE_TO, 'harvest_0_1month.xml')
modify_xml(table, xml0)

# 45% disturbance 
table45 <- table
table45$stor_frac <- 0.45
xml45 <- file.path(WRITE_TO, 'harvest_45_1month.xml')
modify_xml(table45, xml45)

# 65% disturbance 
table65 <- table
table65$stor_frac <- 0.65
xml65 <- file.path(WRITE_TO, 'harvest_65_1month.xml')
modify_xml(table65, xml65)

# 85% disturbance 
table85 <- table
table85$stor_frac <- 0.85
xml85 <- file.path(WRITE_TO, 'harvest_85_1month.xml')
modify_xml(table85, xml85)

# 4. Create 1 day disturbance files ---------------------------------------------------------------------------
# Make 1 month of disturbances table 
ymd_year <- ymd('2019-05-21')
year <- year(ymd_year)
doy <- yday(ymd_year)

# 0% disturbance 
table <- data.table(year = year, doy = doy, agb_frac = 0, bgb_frac = 0, fol_frac = 0, stor_frac = 0)
xml0 <- file.path(WRITE_TO, 'harvest_0_1day.xml')
modify_xml(table, xml0)

# 45% disturbance 
table45 <- table
table45$stor_frac <- 0.45
xml45 <- file.path(WRITE_TO, 'harvest_45_1day.xml')
modify_xml(table45, xml45)

# 65% disturbance 
table65 <- table
table65$stor_frac <- 0.65
xml65 <- file.path(WRITE_TO, 'harvest_65_1day.xml')
modify_xml(table65, xml65)

# 85% disturbance 
table85 <- table
table85$stor_frac <- 0.85
xml85 <- file.path(WRITE_TO, 'harvest_85_1day.xml')
modify_xml(table85, xml85)


# 5. Create 1 day uniform ---------------------------------------------------------------------------
# Make 1 month of disturbances table 
ymd_year <- ymd('2019-05-21')
year <- year(ymd_year)
doy <- yday(ymd_year)

# 0% disturbance 
table <- data.table(year = year, doy = doy, agb_frac = 0, bgb_frac = 0, fol_frac = 0, stor_frac = 0)
xml0 <- file.path(WRITE_TO, 'harvest_0_1day_uniform.xml')
modify_xml(table, xml0)

# 45% disturbance 
table45 <- table
table45$agb_frac <- table45$stor_frac <- table45$bgb_frac <- table45$fol_frac <- 0.45
xml45 <- file.path(WRITE_TO, 'harvest_45_1day_uniform.xml')
modify_xml(table45, xml45)

# 65% disturbance 
table65 <- table
table65$agb_frac <- table65$stor_frac <- table65$bgb_frac <- table65$fol_frac <- 0.65
xml65 <- file.path(WRITE_TO, 'harvest_65_1day_uniform.xml')
modify_xml(table65, xml65)

# 85% disturbance 
table85 <- table
table85$agb_frac <- table85$stor_frac <- table85$bgb_frac <- table85$fol_frac <- 0.85
xml85 <- file.path(WRITE_TO, 'harvest_85_1day_unifrom.xml')
modify_xml(table85, xml85)


# 6. Create 1 day above ---------------------------------------------------------------------------
# Make 1 month of disturbances table 
ymd_year <- ymd('2019-05-21')
year <- year(ymd_year)
doy <- yday(ymd_year)

# 0% disturbance 
table <- data.table(year = year, doy = doy, agb_frac = 0, bgb_frac = 0, fol_frac = 0, stor_frac = 0)
xml0 <- file.path(WRITE_TO, 'harvest_0_1day_above.xml')
modify_xml(table, xml0)

# 45% disturbance 
table45 <- table
table45$agb_frac <- table45$stor_frac <- table45$fol_frac <- 0.45
xml45 <- file.path(WRITE_TO, 'harvest_45_1day_above.xml')
modify_xml(table45, xml45)

# 65% disturbance 
table65 <- table
table65$agb_frac <- table65$stor_frac <- table65$fol_frac <- 0.65
xml65 <- file.path(WRITE_TO, 'harvest_65_1day_above.xml')
modify_xml(table65, xml65)

# 85% disturbance 
table85 <- table
table85$agb_frac <- table85$stor_frac <- table85$fol_frac <- 0.85
xml85 <- file.path(WRITE_TO, 'harvest_85_1day_above.xml')
modify_xml(table85, xml85)

