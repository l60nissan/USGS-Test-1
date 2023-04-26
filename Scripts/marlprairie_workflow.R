######################################
# Workflow for Restoration Run MArl Praire Output
#
# Caitlin Hackett chackett@usgs.gov
######################################

# Load packages
library(tidyverse)
library(sf)
library(raster)

# Source dependancy scripts
# Functions to process data nd make bar plots
source("./Scripts/marlprairie_process_function.R")

# Function to generate maps
source("./Scripts/marlprairie_map_function.R")

# Defined file paths for shapefiles
source("./Scripts/input_paths.R")

# Defined species strings - for consistency across coordinated scripts
source("./Scripts/species_string_definitions.R")

# Define strings to avoid hard coding
mp_file_pattern <- "\\MP_Scores.csv"

########################################################
### USER SET FACTORS ###

# Folder containing species output
#PARENT_PATH <- "./Data/LOSOM_Round1_2021_05/Model Output/MarlPrairie/MarlPrairie_Data/MarlPrairie_Data/"

# Folder to output CSV and figures - include "/" after path to get file names correct when saving 
#OUTPUT_PATH <- "./Output/LOSOM_Round1_2021_05/MarlPrairie/test/"

# Marl Shapefile Path
#mp_shp <- "./Data/LOSOM_Round1_2021_05/Model Output/MarlPrairie/MarlPrairie_Data/MarlPrairie_Data/depth_AA/MP_Scores.shp"

# Set Alternate scenario names
#alt_names <- c("Sim_0001")
#alt_names <- c("AA", "BB", "CC", "DD", "EE1", "EE2")
#alt_names <- c("WECB", "WFWO")
#alt_names

# Set Baseline scenario names
#base_names <- c("Sim_0003", "Sim_0002")
#base_names <- c("ECBr", "NA25")
#base_names <- c("ALTHR")
#base_names
#########################################################

# Scenario Name strings
alt_names <- paste0(alt_names, collapse = "|")
alt_names

# Base Name strings
base_names <- paste0(base_names, collapse = "|")
base_names

# Alt and Base Names
#if(length(base_names) == 1){base_names <- paste0(base_names, "|")}
base_names_join <- paste0(base_names, "|")
all_scenario_names <- paste0(base_names_join, alt_names, collapse = "|")
all_scenario_names

# All Files
all_files <- list.files(paren_path, pattern = mp_file_pattern,
                        full.names = TRUE, recursive = TRUE)
all_files

# Alt Files
alt_list <- grep(alt_names, value = TRUE, all_files)
alt_list

# Base Files
base_list <- grep(base_names, value = TRUE, all_files)
base_list


## Process data for all baseline and alternate combinations
marl_process_list <- list()
acreage_diff_df <- data.frame()
  for (b in 1:length(base_list)) {
    
    b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
    marl_process_list[[b]] <- list()
    names(marl_process_list)[[b]] <- b_name
    
    for (a in 1:length(alt_list)) {
      a_name <- str_extract_all(alt_list[a], all_scenario_names)[[1]]
      print(paste0("Processing :: ALT_", a_name, " minus BASE_", b_name))
      marl_process <- MarlProcess(base_list[b], alt_list[a], mp_shp)
    
      marl_process_list[[b]][[a]] <- marl_process
      names(marl_process_list[[b]])[[a]] <- marl_process$diff_name
    
      # Add acreage df to large daeframe with all marl acreage
      print(paste0("Adding Acreage data :: ALT_", a_name, " minus BASE_", b_name))
      acreage_diff_df <- bind_rows(acreage_diff_df, marl_process$acreage_df)
    
      # Make Map
      print(paste0("Making Map :: ALT_", a_name, " minus BASE_", b_name))
      
      
      ind_fill <- 'labs'
      dif_fill <- 'labs'
      map_title <- marl_process$Map_title
      out_path <- OUTPUT_PATH
      out_file <- paste0(out_path,"Marl_HSI_", marl_process$diff_name, ".tiff")
    
      MARL_MAP(DF_IND = marl_process$ind_df,
             IND_FILL = ind_fill,
             DF_DIF = marl_process$diff_df,
             DIF_FILL = dif_fill,
             SCENARIO_COL = names(marl_process$ind_df),
             SPOP_PATH = SUBPOP_PATH,
             MPR_PATH = MPR_PATH,
             WCAS_PATH = WCAS_PATH,
             MAP_EXTENT = MAP_EXTENT,
             MAP_TITLE = marl_process$Map_title,
             OUTPUT_FILE_NAME = out_file)
      }
  }

# Save acreage Table
write.csv(acreage_diff_df,
          file = paste0(OUTPUT_PATH,"Acreage_", marl_process$sp_string, ".csv"),
          row.names = FALSE)

# Save Marl RDATA of processed files
save(marl_process_list, all_files,
     file = paste0(OUTPUT_PATH, marl_process$sp_string,
                   "_processed_data.RData"))