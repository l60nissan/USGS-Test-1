## ------------------------------------------------------------------------------
## Generate Tables for Sparrow Helper
## ------------------------------------------------------------------------------

# Load packages
library(tidyverse)

## ------------------------------------------------------------------------------
# Calculations for table
# 
# "Table: Difference in the number of days for which the daily mean depths
# are within the target depth range (-50.0 to -25.0 cm) from 1965 through 2005,
# where the difference is the number of days in the tentatively selected plan
# minus the number of days in the baseline."
## ------------------------------------------------------------------------------

# Vector of all scenario names - scenario names set in workflow_inputs.R
scenario_names <- c(alt_names, base_names)
scenario_names

# Make vector with "|" between each scenario 
all_scenario_names <- paste0(scenario_names, collapse = "|")
all_scenario_names

all_files <- list.files(path = parent_path,
                        pattern = "depth_average_\\(-50.0 to -25.0).csv",
                        recursive = TRUE, full.names = TRUE)
all_files

#Load all files and add scenario name
all_runs <- data.frame()
for (s in 1:length(all_files)) {
  name <- str_extract_all(all_files[s], all_scenario_names)[[1]]
  
  file <- read.csv(all_files[s])
  
  file$Scenario <- name

  all_runs <- bind_rows(all_runs, file)  
  }

# Make long - letters represent subpopulations
runs.long <- pivot_longer(all_runs, c("A", "B","C","D", "E","F"))

# Group and sum each subpopulation for each group
runs.summary <- runs.long %>%
  group_by(Scenario, name) %>%
  summarise(num.days = sum(value))

#Calculate difference
num_day_diff <- data.frame()
for (b in 1:length(base_names)) {
  b_name <- base_names[b]
  
  for (a in 1:length(alt_names)) {

    a_name <- alt_names[a]
    
    ab_df <- runs.summary[runs.summary$Scenario == a_name | runs.summary$Scenario == b_name,]
    
    ab_df <- pivot_wider(ab_df, id_cols = "name",
                         names_from = "Scenario", values_from = "num.days")
    
    ab_df$Scenario <- paste0(a_name, "-", b_name)
    
    ab_df$Difference <- ab_df[[a_name]] - ab_df[[b_name]]
    
    diff_days <- dplyr::select(ab_df, c(name, Scenario, Difference))

    num_day_diff <- bind_rows(num_day_diff, diff_days)
  }
}

#format wide for word document
num_day_diff <- pivot_wider(num_day_diff, id_cols = "Scenario",
                            names_from = "name", values_from = "Difference")

#export to document calculations
output_filename <- paste0(output_path, "SparrowHelper_table.csv")
output_filename
write.csv(num_day_diff, output_filename, row.names = F)





