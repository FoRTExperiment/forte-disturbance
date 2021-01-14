# Load libs

library(XML)
library(xml2)
library(data.table)

BASE_DIR <- here::here()
INPUT_DIR <- file.path(BASE_DIR, "A.inputs", "events")
OUT_DIR <- INPUT_DIR

out_file <- file.path(OUT_DIR, 'test.xml')


# Load the template xml file
template_xml <- xmlTreeParse(file.path(INPUT_DIR, "harvest_template.xml"), 
                             useInternal = TRUE)

template_xml <- read_xml(x = file.path(INPUT_DIR, "harvest_template.xml"))
xml_name(template_xml)


xml_name(template_xml)
xml_children(template_xml)
xml_text(template_xml)

as_list(template_xml)



# Save the pointer xml
saveXML(template_xml, file = out_file)
