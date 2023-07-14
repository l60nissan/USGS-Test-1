## -----------------------------------------------------------------------------
## Generate Tables for Sparrow Helper
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Generating Sparrow Helper Table"))
      
# Load packages
library(tidyverse)

# Message regarding inputs
message("INFO [", Sys.time(), "] USER INPUTS SET TO: \n*parent path: ", parent_path,
        "\n*output path: ", output_path,
        "\n*alternative scenarios: ", paste(alt_names, collapse = " "),
        "\n*basline scenarios: ", paste(base_names, collapse = " "))

## -----------------------------------------------------------------------------
# Calculations for table
# 
# Table: Difference in the number of days for which the daily mean depths
# are within the target depth range (-50.0 to -25.0 cm) from 1965 through 2005,
# where the difference is the number of days in the tentatively selected plan
# minus the number of days in the baseline
## -----------------------------------------------------------------------------

# Vector of all scenario names - scenario names set in workflow_inputs.R
scenario_names <- c(alt_names, base_names)
scenario_names

# Make vector with "|" between each scenario 
all_scenario_names <- paste0(scenario_names, collapse = "|")
all_scenario_names

# List all csv files that contain the target pattern
file_pattern_tb <- "depth_average_\\(-50.0 to -25.0).csv"
print(paste0("INFO [", Sys.time(), "] File pattern for tables: ",
             file_pattern_tb))
all_files <- list.files(path = parent_path,
                        pattern = file_pattern_tb,
                        recursive = TRUE, full.names = TRUE)
all_files

#Load all files and add scenario name
all_runs <- data.frame()
for (s in seq_along(all_files)) {
  
  # extract alternative or baseline scenario from each file
  name <- str_extract_all(all_files[s], all_scenario_names)[[1]]

  # Read the csv file
  file <- read.csv(all_files[s])
  
  # Create column that contatins scenario name
  file$Scenario <- name
  
  # Bind rows read csv file to previously read data to create one dataframe 
  # that conatains data from all files
  all_runs <- bind_rows(all_runs, file)  
  }

# Make long: letters represent subpopulations
runs.long <- pivot_longer(all_runs, c("A", "B", "C", "D", "E", "F"))

# Group and sum each subpopulation for each group
runs.summary <- runs.long %>%
  group_by(Scenario, name) %>%
  summarise(num.days = sum(value))

#Calculate difference
num_day_diff <- data.frame()
for (b in seq_along(base_names)) {
  
  # Pull baseline name
  b_name <- base_names[b]
  
  for (a in seq_along(alt_names)) {
    
    # Pull alternative line
    a_name <- alt_names[a]
    
    # Extract summary for target alternative (a) and baseline (b)
    ab_df <- runs.summary[runs.summary$Scenario == a_name | runs.summary$Scenario == b_name, ]
    
    # Pivot data to wide format
    ab_df <- pivot_wider(ab_df, id_cols = "name",
                         names_from = "Scenario", values_from = "num.days")
    
    # make scenario name alternative-baseline
    ab_df$Scenario <- paste0(a_name, "-", b_name)
    
    # Calculate differnce between alternative and baseline
    ab_df$Difference <- ab_df[[a_name]] - ab_df[[b_name]]
    
    # Select columns of interest
    diff_days <- dplyr::select(ab_df, c(name, Scenario, Difference))

    # Bind difference calculations together to one dataframe 
    num_day_diff <- bind_rows(num_day_diff, diff_days)
  }
}

#format wide for word document
num_day_diff <- pivot_wider(num_day_diff, id_cols = "Scenario",
                            names_from = "name", values_from = "Difference")

#export to document calculations
table_data_name <- "SparrowHelper_table.csv"
output_filename <- paste0(output_path, table_data_name)
output_filename
print(paste0("INFO [", Sys.time(), "] Saving Data to ", output_filename))
write.csv(num_day_diff, file = output_filename, row.names = FALSE)





