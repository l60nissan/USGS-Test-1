## -----------------------------------------------------------------------------
# Run Sparrow Helper workflow
## -----------------------------------------------------------------------------
print(paste0("Sparrow Helper :: START -- ", Sys.time()))

# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs

# Set inputs for workflow
parent_path <- sparrow_parent_path # parent path
output_path <- sparrow_output_path # output path

# source/run script to make table
source("./Scripts/sparrow_helper_table.R")

# source/run script to make plots
source("./Scripts/sparrow_helper_plots.R")

print(paste0("Sparrow Helper :: END -- ", Sys.time()))
