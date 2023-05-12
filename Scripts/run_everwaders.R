## -----------------------------------------------------------------------------
# Run EverWaders workflow
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] EverWaders :: START"))

# Install any necessary packages that are not yet installed
source("./Scripts/workflow_packages.R")

# Source inputs/definitions
print(paste0("INFO [", Sys.time(), "] Loading Workflow inputs"))
source("./Scripts/workflow_inputs.R") # workflow inputs

print(paste0("INFO [", Sys.time(), "] Loading Species String Definitions"))
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- waders_parent_path # parent path
output_path <- waders_output_path # output path
sp_string <- waders_string # species string
cropped <- waders_cropped # cropped?

# source/run workflow
source("./Scripts/rest_run_workflow.R")

print(paste0("INFO [", Sys.time(), "] EverWaders :: END"))
