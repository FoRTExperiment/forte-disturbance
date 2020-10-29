## Extract the ED results from the h5 files 
## There is some extra post processing that will have to happen (the plan is 
## to use drake to make it reproduciable)

# 0. Set Up  ------------------------------------------------------------------------------------
# Define directories. 
BASE_DIR <- here::here()
OUT_DIR  <- file.path(BASE_DIR, 'ED-outputs')
dir.create(OUT_DIR)

# Load the required pacakges and functions. 
to_source <- file.path(BASE_DIR, "B.ed", "0B.post_processing_functions.R")
stopifnot(file.exists(to_source))
source(to_source)

# 1. Process output directories -----------------------------------------------------------------
# Start with experiment 1! And move them into the output direcotry 
files <- process_outputs(BASE_DIR = BASE_DIR, exp_dirs = "exp-1")

exp1_out <- file.path(OUT_DIR, 'exp-1')
dir.create(exp1_out)

system2('cp', args = c(paste0(files, collapse = ' '), exp1_out))


# 2. Move to local machine ----------------------------------------------------------------------
# As of right now there is no great way to do this, ideally some code would be added here 
# that could move the files to a local machine but as of right now there isn't really a great way to 
# do that, then the results should be saved on osf i think.. .
system2('ssh', args = 'constance')




