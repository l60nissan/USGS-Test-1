## -----------------------------------------------------------------------------
# Run Marl Prairie workflow
## -----------------------------------------------------------------------------
print(paste0("Marl Prairie :: START -- ", Sys.time()))

# Source inputs/definitions
source("./Scripts/workflow_inputs.R") # workflow inputs
source("./Scripts/species_string_definitions.R") # species strings

# Set inputs for workflow
parent_path <- marl_parent_path # parent path
output_path <- marl_output_path # output path

# source/run workflow
source("./Scripts/marlprairie_workflow.R")

print(paste0("Marl Prairie :: END -- ", Sys.time()))
