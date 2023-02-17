## -----------------------------------------------------------------------------
# Run EverWaders workflow
## -----------------------------------------------------------------------------
print(paste0("EVERWADERS :: START -- ", Sys.time()))

# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- waders_parent_path # parent path
output_path <- waders_output_path # output path
sp_string <- waders_string # species string
cropped <- waders_cropped # cropped?

# source/run workflow
source("./Scripts/rest_run_workflow.R")

print(paste0("EVERWADERS :: END -- ", Sys.time()))
