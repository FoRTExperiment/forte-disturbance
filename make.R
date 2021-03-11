# This minimal drake workflow demonstrates the file structure from
# https://books.ropensci.org/drake/projects.html#code-files.
# It is not the only way to organize R scripts for drake,
# but this pattern is helpful.

source("C.analysis/packages.R")  # Load your packages, e.g. library(drake).
source("C.analysis/ED_functions.R") # Define your custom code as a bunch of functions.
source("C.analysis/plan.R")      # Create your drake plan.


make(exp1_met_plan)
