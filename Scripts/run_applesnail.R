## -----------------------------------------------------------------------------
# Run Apple Snail workflow
#
# Run routine to summarize EverSnail (Apple Snail Model) output
# Script outputs are: bar plots, maps, CSVs, and RData of processed output
## -----------------------------------------------------------------------------
# Expected warnings:
# - When working with NetCDFs in R, some warnings will be triggered
# regarding the CRS but these can be ignored. 
#
# - "Warning message: attribute variables are assumed to be spatially
#   constant throughout all geometries"
## -----------------------------------------------------------------------------

print(paste0("INFO [", Sys.time(), "] Apple Snail :: START"))

# Install any necessary packages that are not yet installed
source("./Scripts/workflow_packages.R")

# Source inputs/definitions
print(paste0("INFO [", Sys.time(), "] Loading Workflow inputs"))
source("./Scripts/workflow_inputs.R") # workflow inputs

print(paste0("INFO [", Sys.time(), "] Loading Species String Definitions"))
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- apsn_parent_path # parent path
output_path <- apsn_output_path # output path
sp_string <- apsn_string # species string
cropped <- apsn_cropped # cropped?

# source/run workflow
source("./Scripts/rest_run_workflow.R")

print(paste0("INFO [", Sys.time(), "] Apple Snail :: END"))
