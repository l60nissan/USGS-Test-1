## -----------------------------------------------------------------------------
# Run KiteNest workflow
## -----------------------------------------------------------------------------
# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- snki_parent_path # parent path
output_path <- snki_output_path # output path
sp_string <- snki_string # species string
cropped <- snki_cropped # cropped?

# source/run workflow
source("./Scripts/rest_run_workflow.R")

rm(list = ls()) # Clear environment
