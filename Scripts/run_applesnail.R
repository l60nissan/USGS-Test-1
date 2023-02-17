## -----------------------------------------------------------------------------
# Run Apple Snail workflow
## -----------------------------------------------------------------------------
print(paste0("APPLE SNAIL :: START -- ", Sys.time()))

# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- apsn_parent_path # parent path
output_path <- apsn_output_path # output path
sp_string <- apsn_string # species string
cropped <- apsn_cropped # cropped?

# source/run workflow
source("./Scripts/rest_run_workflow.R")

print(paste0("APPLE SNAIL :: END -- ", Sys.time()))
