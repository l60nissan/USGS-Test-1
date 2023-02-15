## -----------------------------------------------------------------------------
# Run Days Since Dry workflow
## -----------------------------------------------------------------------------
# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- dsd_parent_path # parent path
output_path <- dsd_output_path # output path
sp_string <- dsd_string # species string
cropped <- dsd_cropped # cropped?

# source/run workflow
source("./Scripts/rest_run_workflow.R")

rm(list = ls()) # Clear environment
