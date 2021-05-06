# This minimal drake workflow demonstrates the file structure from
# https://books.ropensci.org/drake/projects.html#code-files.
# It is not the only way to organize R scripts for drake,
# but this pattern is helpful.

# Load your packages, e.g. library(drake).
source("C.analysis/0.packages.R")  

# Load our functions 
source("C.analysis/0.func_ED.R") 
source("C.analysis/0.func_metric.R")

# Load the drake plan 
source("C.analysis/plan.R")      

# Proces all of the outputs from ED rds files into csv files of the monthly/annual carbo fluxes for 
# each FoRTE treatment (0, 45, 65, 85)% with the appropriate units. The files should be saved in 
# ED-outputs/exp-constant-mon.csv and exp-constant-yr.csv 
make(exp_constant)

# Now calculate the mertic values. 
make(metric_values)