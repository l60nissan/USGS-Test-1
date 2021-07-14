################################################################
# Workflow for Netcdf output for restoration runs (LOSOM)
# Use for Alligator, Everwaders, Snail kite, and Applesnail
#
# Caitlin Hackett chackett@usgs.gov
###############################################################

#Load packages
library(tidyverse)
library(ncdf4)
library(raster)
library(sf)
library(cowplot)

source("./Scripts/LOSOM_process_functions.R")
source("./Scripts/LOSOM_map_functions.R")

# Species string options
gator_sp_string <- "Alligator"
waders_sp_string <- c("GBHE", "GLIB", "GREG", "LBHE", "ROSP", "WHIB", "WOST")
apsn_sp_string <- "Apple_Snail"
snki_sp_string <- "SnailKite"
dsd_sp_string <- "dsd"

########################################################
### USER SET FACTORS ###

# Folder containing species output
PARENT_PATH <- "../LOSOM/Data/LOSOM_Round1_2021_05/Model Output/DaysSinceDry/DaysSinceDrydown_COP_AOI/"

# Is Species output already cropped to AOI? (TRUE/FALSE)
cropped <- TRUE

# Folder to output CSV and figures - include "/" after path to get file names correct when saving 
OUTPUT_PATH <- "../LOSOM/Output/LOSOM_Round1_2021_05/DaysSinceDry/"

# Path to AOI
AOI_PATH <- "../../GIS_Library/COP_AOI_mask/COP_AOI_mask.shp"
#AOI_PATH <- "../../GIS_Library/WERPzones_EDEN_mask.shp/WERPzones_EDEN_mask.shp"

# set target species from species string options at top of script
sp_string <- dsd_sp_string

# Set Alternate scenario names
#alt_names <- c("Sim_0001")
#alt_names <- c("ALTHR")
#alt_names <- c("WECB", "WFWO")
alt_names <- c("AA", "BB", "CC", "DD", "EE1", "EE2")
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
#if(length(base_names) == 1){BASE_NAMES <- paste0(BASE_NAMES, "|")
#}else {BASE_NAMES <- BASE_NAMES}
BASE_NAMES_join <- paste0(BASE_NAMES, "|")
BASE_NAMES_join
all_scenario_names <- paste0(BASE_NAMES_join, ALT_NAMES, collapse = "|")
all_scenario_names

# Loop through species, process output, output acreage csv and map
# Loop through target species
process_list_all <- list() # list to store all output
#n <- 1
for(n in 1:length(sp_string)){ # This part is to accomodate multiple species output in the everwaders path

  FILES <- SP_SCENARIO_FILES(ALT_NAMES, BASE_NAMES, PARENT_PATH, sp_string[n])
  FILES
  ALT_LIST <- FILES$ALT_LIST
  ALT_LIST
  BASE_LIST <- FILES$BASE_LIST
  BASE_LIST

  ## Loop to process data and make maps, acreage table and barplot 
  # Process each baselinee and alt combination
    process_list <- list() # list to hold processed data
    percent_diff <- data.frame() # data frame to hold percent difference data for barplot
    acreage_diff_df <- data.frame() # data frame to hold acreage data 
    #b <- 1
    #i <- 1
    
    # Process baseline file b
    for(b in 1:length(BASE_LIST)){  
      
      # Create empty list for output for each baseline
      b_name <- str_extract_all(BASE_LIST[b], all_scenario_names)[[1]]
      process_list[[b]] <- list()
      names(process_list)[[b]] <- b_name
      
      # process alternate file i
      for(i in 1:length(ALT_LIST)){
      
      a_name <- str_extract_all(ALT_LIST[i], all_scenario_names)[[1]]
      print(paste0("Processing :: ALT_", a_name, " minus BASE_", b_name))
  
      map_dfs <- PROCESS_OUTPUT(BASE_LIST[b], ALT_LIST[i], AOI_PATH, all_scenario_names, CROPPED = cropped)
  
      process_list[[b]][[i]] <- map_dfs
      names(process_list[[b]])[[i]] <- paste0(map_dfs$alt_name, "-", map_dfs$base_name)
      
      print(paste0("Making Map :: ALT_", a_name, " minus BASE_", b_name))
      
      # Make Map
      ind_fill <- 'labs'
      dif_fill <- 'labs'
      map_title <- map_dfs$map_title
      out_path <- OUTPUT_PATH
      out_file <- paste0(out_path, gsub(" ", "_", map_title), "_", map_dfs$alt_name, "_", map_dfs$base_name, ".pdf")
  
      print(paste0("Making map for ", gsub(" ", "_", map_title), "_", map_dfs$alt_name, "_", map_dfs$base_name))
  
      name.labs <- map_dfs$name.labs
  
      RESTORATION_RUN_MAP(DF_IND = map_dfs$ind_df,
                      IND_FILL = ind_fill,
                      DF_DIF = map_dfs$diff_df,
                      DIF_FILL = dif_fill,
                      SCENARIO_COL = names(map_dfs$ind_df)[5],
                      YEAR_COL = names(map_dfs$ind_df)[3],
                      AOI_PATH = AOI_PATH,
                      MPR_PATH = MPR_PATH,
                      WCAS_PATH = WCAS_PATH,
                      FL_PATH = FL_PATH,
                      MAP_EXTENT = MAP_EXTENT,
                      MAP_TITLE = map_title,
                      OUTPUT_FILE_NAME = out_file)
  
      # Add data to Acreage dataframe
      print(paste0("Adding Acreage :: ALT_", a_name, " minus BASE_", b_name))
      acreage_diff_df <- bind_rows(acreage_diff_df, map_dfs$acreage_df)

      # add data t o percent diff dataframe - if percent diff was calculated during data processing, bind rows to one dataframe
      #if(grepl(gator_sp_string, FILES$ALL_FILES[1])){
      print(paste0("Adding Percent Difference :: ALT_", a_name, " minus BASE_", b_name))
      #if("Percent_diff_mean" %in% names(map_dfs)){
      if(!is.null(map_dfs$Percent_diff_mean)){
        percent_diff <- bind_rows(percent_diff, map_dfs$Percent_diff_mean)
      } 
      
      if(is.null(map_dfs$Percent_diff_mean)){
      percent_diff <- NULL
          }
       }
    }
  # Print list with all processed data  
  process_list_all[[n]] <- process_list
  names(process_list_all)[[n]] <- sp_string[n]
  
  # Export acreage Data
  print(paste0("Export Acreage Data CSV"))
  
  # Tidy scenario_year column to individual columns
  acreage_diff_df <- separate(acreage_diff_df, Scenario_year, into = c("Scenario", "Year"), sep = "_X")
  
  output_filename <- paste0(out_path, "Acreage_", gsub(" ", "_", map_title), ".csv")
  write.csv(acreage_diff_df, output_filename, row.names = FALSE)
  
  # Export dataframe with all percent differences
  # If percent diffrence data frame exists, create a bar plot
  print(paste0("Export Percent Difference Data CSV"))
  if(!is.null(percent_diff)){
  diff_output_filename <- paste0(out_path, "PercentDiff_", gsub(" ", "_", map_title), ".csv")
  write.csv(percent_diff, diff_output_filename, row.names = FALSE)
  
  # Make Percent diffrence bar plot
  x_var <- "Year"
  y_var <- "Percent_Difference"
  fill_var <- "Scenarios"
  title <- map_dfs$map_title
  #y_lab <- paste0("Percent Change in ", title, "\nfrom baseline to ", map_dfs$alt_name)
  x_lab <- "Year"
  min_limit <-plyr::round_any(min(percent_diff[y_var]), 5, f = floor) - 10
  max_limit <- plyr::round_any(max(percent_diff[y_var]), 5, f = ceiling) + 10
  

  for(a in 1:length(alt_names)){
    print(paste0("Making Differnce Bar Plot :: ", alt_names[a]))
    
    per_diff_alt <- percent_diff[grep(alt_names[a], percent_diff$Scenarios),]
    TEST <- PER_DIFF_PLOT(
      DF = per_diff_alt,
      X_VAR = x_var,
      Y_VAR = y_var,
      FILL_VAR = fill_var,
      TITLE = title,
      Y_LAB = paste0("Percent Change in ", title, "\nfrom Baseline to ", alt_names[a]),
      X_LAB = x_lab,
      MIN_LIMIT = min_limit,
      MAX_LIMIT = max_limit
    )
    diff_plot_filename <- paste0(out_path, "PercentDiff_BarPlot_", gsub(" ", "_", map_title),"_", alt_names[a], ".pdf")
    ggsave(diff_plot_filename, TEST, width=11, height=8.5, units="in", dpi=300)
  }}
  
}

# Save R object that has processed data stored
  if(grepl("EverWaders", FILES$ALL_FILES[1])){
  save_string <- "EverWaders"
  }else{
  save_string <- sp_string}

save(process_list_all, FILES, file = paste0(out_path, save_string, "_processed_data.RData"))
load('./Output/LOSOM_Round1_2021_05/Alligator/Alligator_Round1/Alligator_processed_data.RData')
