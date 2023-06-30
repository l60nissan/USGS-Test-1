## -----------------------------------------------------------------------------
# Run Marl Prairie workflow

# Run routine to summarize Marl Prairie Indicator output
# Script outputs are: maps, CSVs, and RData of processed output
## -----------------------------------------------------------------------------
# Expected warnings:
# - "Warning message: attribute variables are assumed to be spatially
#   constant throughout all geometries"
# - "st_centroid assumes attributes are constant over geometries of x"
## -----------------------------------------------------------------------------

print(paste0("INFO [", Sys.time(), "] Marl Prairie :: START"))

# Install any necessary packages that are not yet installed
source("./Scripts/workflow_packages.R")

# Source inputs/definitions
print(paste0("INFO [", Sys.time(), "] Loading Workflow inputs"))
source("./Scripts/workflow_inputs.R") # workflow inputs

# Set inputs for workflow
parent_path <- marl_parent_path # parent path
output_path <- marl_output_path # output path

# source/run workflow
source("./Scripts/marlprairie_workflow.R")

print(paste0("INFO [", Sys.time(), "] Marl Prairie :: END"))
