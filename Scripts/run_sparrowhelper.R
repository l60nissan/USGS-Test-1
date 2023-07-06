## -----------------------------------------------------------------------------
# Run Sparrow Helper workflow
#
# Run routine to summarize Cape Sable Seaside Sparrow (CSSS) Helper output
# Script outputs are: bar plots and CSVs
## -----------------------------------------------------------------------------
# No expected warnings
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Sparrow Helper :: START"))

# Install any necessary packages that are not yet installed
source("./Scripts/workflow_packages.R")

# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs

# Set inputs for workflow
parent_path <- sparrow_parent_path # parent path
output_path <- sparrow_output_path # output path

# source/run script to make table
source("./Scripts/sparrow_helper_table.R")

# source/run script to make plots
source("./Scripts/sparrow_helper_plots.R")

print(paste0("INFO [", Sys.time(), "] Sparrow Helper :: END"))
