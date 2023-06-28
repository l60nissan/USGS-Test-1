## -----------------------------------------------------------------------------
# Workflow for Restoration Run Marl Prairie Output
## -----------------------------------------------------------------------------

print(paste0("INFO [", Sys.time(), "] Running Workflow"))

# Load packages
library(tidyverse)
library(sf)
library(raster)

# Source dependency scripts
print(paste0("INFO [", Sys.time(), "] Sourcing Dependency Scripts"))

# Functions to process data nd make bar plots
source("./Scripts/marlprairie_process_function.R")

# Function to generate maps
source("./Scripts/marlprairie_map_functions.R")

# Defined file paths for shapefiles
source("./Scripts/input_paths.R")

# Defined species strings - for consistency across coordinated scripts
source("./Scripts/species_string_definitions.R")

# Define strings to avoid hard coding
mp_file_pattern <- "\\MP_Scores.csv"

## -----------------------------------------------------------------------------
# Scenario Name strings
alt_names <- paste0(alt_names, collapse = "|")
alt_names

# Base Name strings
base_names <- paste0(base_names, collapse = "|")
base_names

# Alt and Base Names
base_names_join <- paste0(base_names, "|")
all_scenario_names <- paste0(base_names_join, alt_names, collapse = "|")
all_scenario_names

# All Files
all_files <- list.files(parent_path, pattern = "\\MP_Scores.csv",
                        full.names = TRUE, recursive = TRUE)
all_files

# Alt Files
alt_list <- grep(alt_names, value = TRUE, all_files)
alt_list

# Base Files
base_list <- grep(base_names, value = TRUE, all_files)
base_list

# Shapefile of MP scores
mp_shp_lst <- list.files(parent_path, pattern = "\\MP_Scores.shp",
                     full.names = TRUE, recursive = TRUE)
mp_shp <- mp_shp_lst[1]
mp_shp

## Process data for all baseline and alternate combinations
marl_process_list <- list()
acreage_diff_df <- data.frame()
  for (b in 1:seq_along(base_list)) {
    
    b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
    marl_process_list[[b]] <- list()
    names(marl_process_list)[[b]] <- b_name
    
    for (a in 1:seq_along(alt_list)) {
      a_name <- str_extract_all(alt_list[a], all_scenario_names)[[1]]
      print(paste0("Processing :: ALT_", a_name, " minus BASE_", b_name))
      marl_process <- MarlProcess(base_file = base_list[b],
                                  alt_file = alt_list[a],
                                  mp_file = mp_shp,
                                  subpop_file = subpop_path)
    
      marl_process_list[[b]][[a]] <- marl_process
      names(marl_process_list[[b]])[[a]] <- marl_process$diff_name
    
      # Add acreage df to large daeframe with all marl acreage
      print(paste0("Adding Acreage data :: ALT_", a_name, " minus BASE_", b_name))
      acreage_diff_df <- bind_rows(acreage_diff_df, marl_process$acreage_df)
    
      # Make Map
      print(paste0("Making Map :: ALT_", a_name, " minus BASE_", b_name))
      
      
      ind_fill <- "labs"
      dif_fill <- "labs"
      map_title <- marl_process$Map_title
      out_path <- output_path
      out_file <- paste0(out_path, "Marl_HSI_", marl_process$diff_name, ".tiff")
    
      MarlMap(df_ind = marl_process$ind_df,
             ind_fill = ind_fill,
             df_dif = marl_process$diff_df,
             dif_fill = dif_fill,
             scenario_col = names(marl_process$ind_df),
             spop_path = subpop_path,
             mpr_path = mpr_path,
             wcas_path = wcas_path,
             map_extent = marl_map_extent,
             map_title = marl_process$Map_title,
             output_file_name = out_file)
      }
  }

# Save acreage Table
write.csv(acreage_diff_df,
          file = paste0(output_path, "Acreage_", marl_process$sp_string, ".csv"),
          row.names = FALSE)

# Save Marl RDATA of processed files
save(marl_process_list, all_files,
     file = paste0(output_path, marl_process$sp_string,
                   "_processed_data.RData"))
