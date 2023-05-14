## -----------------------------------------------------------------------------
# Run Small Fish workflow
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Small Fish :: START"))

# Source inputs/definitions
print(paste0("INFO [", Sys.time(), "] Loading Workflow inputs"))
source("./Scripts/workflow_inputs.R") # workflow inputs

# Set inputs for workflow
parent_path <- fish_path # parent path
output_path <- fish_output_path # output path

# source/run workflow
source("./Scripts/smallfish_workflow.R")

print(paste0("INFO [", Sys.time(), "] Small Fish :: END"))
