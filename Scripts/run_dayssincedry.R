## -----------------------------------------------------------------------------
# Run Days Since Dry workflow
#
# Run routine to summarize DaysSinceDry output
# Script outputs are: bar plots, maps, CSVs, and RData of processed output
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Days Since Dry :: START"))

# Install any necessary packages that are not yet installed
source("./Scripts/workflow_packages.R")

# Source inputs/definitions
print(paste0("INFO [", Sys.time(), "] Loading Workflow inputs"))
source("./Scripts/workflow_inputs.R") # workflow inputs

print(paste0("INFO [", Sys.time(), "] Loading Species String Definitions"))
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- dsd_parent_path # parent path
output_path <- dsd_output_path # output path
sp_string <- dsd_string # species string
cropped <- dsd_cropped # cropped?

# source/run workflow
source("./Scripts/rest_run_workflow.R")

print(paste0("INFO [", Sys.time(), "] Days Since Dry :: END"))
