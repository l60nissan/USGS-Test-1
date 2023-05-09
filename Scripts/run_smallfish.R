## -----------------------------------------------------------------------------
# Run Small Fish workflow
## -----------------------------------------------------------------------------
print(paste0("SMALL FISH :: START -- ", Sys.time()))

# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs

# Set inputs for workflow
parent_path <- fish_path # parent path
output_path <- fish_output_path # output path

# source/run workflow
source("./Scripts/smallfish_workflow.R")

print(paste0("SMALL FISH :: END -- ", Sys.time()))
