# This minimal drake workflow demonstrates the file structure from
# https://books.ropensci.org/drake/projects.html#code-files.
# It is not the only way to organize R scripts for drake,
# but this pattern is helpful.

source("C.analysis/packages.R")  # Load your packages, e.g. library(drake).
source("C.analysis/functions.R") # Define your custom code as a bunch of functions.
source("C.analysis/plan.R")      # Create your drake plan.

# Call make() to run your work.
# Your targets will be stored in a hidden .drake/ cache, there has to be a better way to do thi 
# but fot now make all of the different drake plans separately. Also need to figure out the best 
# way to incorperate the markdown documents. 
lapply(plan, make)

