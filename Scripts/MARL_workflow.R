######################################
# Workflow for Restoration Run MArl Praire Output
#
# Caitlin Hackett chackett@usgs.gov
######################################

source("../Figure Development/Scripts/Final/Second Test/MARL_process_function.R")
source("../Figure Development/Scripts/Final/Second Test/MARL_map_functions.R")


library(tidyverse)
library(sf)
library(raster)

########################################################
### USER SET FACTORS ###

# Folder containing species output
PARENT_PATH <- "./Data/LOSOM_Round1_2021_05/Model Output/MarlPrairie/MarlPrairie_Data/MarlPrairie_Data/"

# Folder to output CSV and figures - include "/" after path to get file names correct when saving 
OUTPUT_PATH <- "./Output/LOSOM_Round1_2021_05/MarlPrairie/test/"

# Marl Shapefile Path
mp_shp <- "./Data/LOSOM_Round1_2021_05/Model Output/MarlPrairie/MarlPrairie_Data/MarlPrairie_Data/depth_AA/MP_Scores.shp"

# Set Alternate scenario names
#alt_names <- c("Sim_0001")
alt_names <- c("AA", "BB", "CC", "DD", "EE1", "EE2")
#alt_names <- c("WECB", "WFWO")
alt_names

# Set Baseline scenario names
#base_names <- c("Sim_0003", "Sim_0002")
base_names <- c("ECBr", "NA25")
#base_names <- c("ALTHR")
base_names
#########################################################

# Scenario Name strings
ALT_NAMES <- paste0(alt_names, collapse = "|")
ALT_NAMES

# Base Name strings
BASE_NAMES <- paste0(base_names, collapse = "|")
BASE_NAMES

# Alt and Base Names
#if(length(BASE_NAMES) == 1){BASE_NAMES <- paste0(BASE_NAMES, "|")}
BASE_NAMES_join <- paste0(BASE_NAMES, "|")
all_scenario_names <- paste0(BASE_NAMES_join, ALT_NAMES, collapse = "|")
all_scenario_names

# All Files
ALL_FILES <- list.files(PARENT_PATH, pattern = "\\MP_Scores.csv", full.names = TRUE, recursive = TRUE)
ALL_FILES

# Alt Files
ALT_LIST <- grep(ALT_NAMES, value = TRUE, ALL_FILES)
ALT_LIST

# Base Files
BASE_LIST <- grep(BASE_NAMES, value = TRUE, ALL_FILES)
BASE_LIST


## Process data for all baseline and alternate combinations
marl_process_list <- list()
acreage_diff_df <- data.frame()
  for(b in 1:length(BASE_LIST)){
    
    b_name <- str_extract_all(BASE_LIST[b], all_scenario_names)[[1]]
    marl_process_list[[b]] <- list()
    names(marl_process_list)[[b]] <- b_name
    
    for(a in 1:length(ALT_LIST)){
      a_name <- str_extract_all(ALT_LIST[a], all_scenario_names)[[1]]
      print(paste0("Processing :: ALT_", a_name, " minus BASE_", b_name))
      marl_process <- MARL_PROCESS_OUTPUT(BASE_LIST[b], ALT_LIST[a], mp_shp)
    
      marl_process_list[[b]][[a]]<- marl_process
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
write.csv(acreage_diff_df, file = paste0(OUTPUT_PATH,"Acreage_", marl_process$sp_string, ".csv"), row.names = FALSE)

# Save Marl RDATA of processed files
save(marl_process_list, ALL_FILES, file = paste0(OUTPUT_PATH, marl_process$sp_string, "_processed_data.RData"))


######
# Examine output since difference maps look similar, values are different but falling within the same bin      
ecbr_diff <- lapply(marl_process_list$ECBr, function(x) x$diff_df)
ecbr_diff_values <- lapply(ecbr_diff, function(x) x$hsi_CombinedScore)
ecbr_diff_df <- bind_cols(ecbr_diff_values)
ecbr_diff_df <- bind_cols(ecbr_diff_df, marl_process_list$ECBr$`AA-ECBr`$diff_df$RSM)

na25_diff <- lapply(marl_process_list$NA25, function(x) x$diff_df)
na25_diff_values <- lapply(na25_diff, function(x) x$hsi_CombinedScore)
na25_diff_df <- bind_cols(na25_diff_values)
na25_diff_df <- bind_cols(na25_diff_df, marl_process_list$ECBr$`AA-ECBr`$diff_df$RSM)
