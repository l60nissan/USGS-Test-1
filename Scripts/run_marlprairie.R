## -----------------------------------------------------------------------------
# Run Marl Prairie workflow
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
