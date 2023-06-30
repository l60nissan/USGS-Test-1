## -----------------------------------------------------------------------------
# Run Small Fish workflow

# Run routine to summarize Small Fish Density Model output
# Script outputs are: line graph, bar plots, maps, txt files,
#                      and RData of processed output
## -----------------------------------------------------------------------------
# Expected warnings:
# - "Warning message: attribute variables are assumed to be spatially
#   constant throughout all geometries"
## -----------------------------------------------------------------------------

print(paste0("INFO [", Sys.time(), "] Small Fish :: START"))

# Install any necessary packages that are not yet installed
source("./Scripts/workflow_packages.R")

# Source inputs/definitions
print(paste0("INFO [", Sys.time(), "] Loading Workflow inputs"))
source("./Scripts/workflow_inputs.R") # workflow inputs

# Set inputs for workflow
parent_path <- fish_parent_path # parent path
fish_path <- list.files(parent_path, pattern = "\\FISH_TIMESERIES_PSU.csv",
                          full.names = TRUE, recursive = TRUE)
output_path <- fish_output_path # output path

# source/run workflow
source("./Scripts/smallfish_workflow.R")

print(paste0("INFO [", Sys.time(), "] Small Fish :: END"))
