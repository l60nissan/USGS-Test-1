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
#waders_sp_string <- c("GBHE", "GLIB", "GREG", "LBHE", "ROSP", "WHIB", "WOST")
waders_sp_string <- c("GREG", "ROSP", "WHIB", "WOST")
apsn_sp_string <- "Apple_Snail"
snki_sp_string <- "SnailKite"
dsd_sp_string <- "dsd"

########################################################
### USER SET FACTORS ###

# Folder containing species output
PARENT_PATH <- "../LOSOM/Data/LOSOM_Round1_2021_05/Model Output/Apple_Snail/JEM_Apple_Snail_Model_Data/JEM_Apple_Snail_Model_Data/"

# Is Species output already cropped to AOI? (TRUE/FALSE)
cropped <- TRUE

# Folder to output CSV and figures - include "/" after path to get file names correct when saving 
OUTPUT_PATH <- "../LOSOM/Output/LOSOM_Round1_2021_05/Apple_Snail/"

# Path to AOI
AOI_PATH <- "../../GIS_Library/COP_AOI_mask/COP_AOI_mask.shp"
#AOI_PATH <- "../../GIS_Library/WERPzones_EDEN_mask.shp/WERPzones_EDEN_mask.shp"

# Filepaths FOR MAPs
  # Main Park Road
  MPR_PATH <- "../../GIS_Library/ENP_main_road/ENP_main_road.shp" # Main Park Road

  #Wcas Boundary
  WCAS_PATH <- "../../GIS_Library/EVERareas_UTM/EVERareas_UTM/EVERareas_UTM.shp" # WCA Boundaries

  # florida outline
  FL_PATH <- "../../GIS_Library/FL_outline_ESRI_2010/FL_outline_ESRI_2010/FL_outline_ESRI_2010.shp" # ESRI Fl boundary

# set target species from species string options at top of script
sp_string <- apsn_sp_string

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

##  END USER SET FACTORS ###########
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

  ## Extract species name
  species <- sp_string[n]
  
  ## List All files, Alternate scenario files, and Baseline files
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
      print(paste0("Processing :: ALT_", a_name, " minus BASE_", b_name, " -- ", Sys.time()))
      
      ####
      # Process data to build maps and calculate acreage
      ####
      map_dfs <- PROCESS_OUTPUT(BASE_LIST[b], ALT_LIST[i], AOI_PATH, all_scenario_names)#, CROPPED = cropped)
      
      ####
      # Add data to list of processed data
      ####
      process_list[[b]][[i]] <- map_dfs
      names(process_list[[b]])[[i]] <- paste0(map_dfs$alt_name, "-", map_dfs$base_name)
      
      ####
      # Make maps
      ####
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
      ####
      # Add data to Acreage dataframe
      ####
      print(paste0("Adding Acreage :: ALT_", a_name, " minus BASE_", b_name))
      acreage_diff_df <- bind_rows(acreage_diff_df, map_dfs$acreage_df)
      } #close i
    } #close b
      
    #########################################################################
    # Process Diff calculations for Alts and baselines
    # - This happens within n loop so it is performed for every species in the sp_string
    # - can use b for base and i for alt becuase they are originating from the same list of baselines and alt files
    # - if mask is applied, it masks the entire stack. Individual years were masked above for maps
    ##########################################################################
    
    # Mask FILES and Read as stack - For percent diff calcilations and barplots
    if(!cropped){
      ###
      # MASK BASELINES & CALC CELL STATS (landscape means)
      ###
      BASE_LIST_MASKED <- list()
      for(b in 1:length(BASE_LIST)){
        b_name <- str_extract_all(BASE_LIST[b], all_scenario_names)[[1]]
        
        print(paste0("Masking Base :: ", b_name))
        
        # Mask the nc stack
        masked <- MASK_NC_OUTPUT(BASE_LIST[b], AOI_PATH, all_scenario_names)
        
        print(paste0("Calculating Landscape Mean Base :: ", b_name, " -- ", Sys.time()))
        
        # Calculate mean of landscape for each day
        daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
        masked$daily_mean <- daily_mean_vals
        
        BASE_LIST_MASKED[[b]] <- masked
        names(BASE_LIST_MASKED)[[b]] <- masked$Scenario 
      } # closes b
      
      ##
      # MASK ALTERNATES & CALC CELL STATS (landscale means)
      ##
      ALT_LIST_MASKED <- list()
      for(i in 1:length(ALT_LIST)){
        a_name <- str_extract_all(ALT_LIST[i], all_scenario_names)[[1]]
        
        print(paste0("Masking Alt :: ", a_name))
        
        # Mask the nc stack
        masked <- MASK_NC_OUTPUT(ALT_LIST[i], AOI_PATH, all_scenario_names)
        
        print(paste0("Calculating Landscape Mean Alt :: ", a_name, " -- ", Sys.time()))
        
        # Calculate mean of landscape for each day
        daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
        masked$daily_mean <- daily_mean_vals
        
        ALT_LIST_MASKED[[i]] <- masked
        names(ALT_LIST_MASKED)[[i]] <- masked$Scenario 
      } # closes i 
    }else{
      ###
      # READ BASELINES & CALC CELL STATS
      ###
      BASE_LIST_MASKED <- list()
      for(b in 1:length(BASE_LIST)){
        b_name <- str_extract_all(BASE_LIST[b], all_scenario_names)[[1]]
        
        print(paste0("Stacking Base :: ", b_name,  " -- ", Sys.time()))
        
        # Read nc stack - no masking since already masked (cropped = TRUE)
        masked <- STACK_NC_OUTPUT(BASE_LIST[b], AOI_PATH, all_scenario_names)
        
        print(paste0("Calculating Landscape Mean Base :: ", b_name, " -- ", Sys.time()))
        
        # Calculate mean of landscape for each day
        daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
        masked$daily_mean <- daily_mean_vals
        
        BASE_LIST_MASKED[[b]] <- masked
        names(BASE_LIST_MASKED)[[b]] <- masked$Scenario
      } # closes b
      ###
      # READ ALTERNATES & CALC CELL STATS
      ###
      ALT_LIST_MASKED <- list()
      for(i in 1:length(ALT_LIST)){
        a_name <- str_extract_all(ALT_LIST[i], all_scenario_names)[[1]]
        
        print(paste0("Stacking Alt :: ", a_name))
        
        # Read nc stack - no masking since already masked (cropped = TRUE)
        masked <- STACK_NC_OUTPUT(ALT_LIST[i], AOI_PATH, all_scenario_names)
        
        print(paste0("Calculating Landscape Mean Alt :: ", a_name, " -- ", Sys.time()))
        
        # Calculate mean of landscape for each day
        daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
        masked$daily_mean <- daily_mean_vals
        
        ALT_LIST_MASKED[[i]] <- masked
        names(ALT_LIST_MASKED)[[i]] <- masked$Scenario 
      } # closes i 
    } # closes else
    
    ## Export first baseline and alt slices to double check masked for calculations
      jpeg(paste0(OUTPUT_PATH, species, "_base_mask_test.jpg"))
      plot(BASE_LIST_MASKED[[1]][[1]][[1]])
      dev.off()
      
      jpeg(paste0(OUTPUT_PATH, species, "_alt_mask_test.jpg"))
      plot(ALT_LIST_MASKED[[1]][[1]][[1]])
      dev.off()
    
    ####
    #Process data to calculate percent difference
    ###
    # Process baseline file b
    percent_diff <- data.frame() # list to hold processed data
    #percent_diff <- data.frame() # data frame to hold percent difference data for barplot
    
    #index <- 0
    for(b in 1:length(BASE_LIST_MASKED)){  
      
      # Get baseline name
      b_name <- BASE_LIST_MASKED[[b]]$Scenario
      
      # Get Baseline daily vals
      base_vals <- BASE_LIST_MASKED[[b]]$daily_mean

      # process alternate file i
      for(i in 1:length(ALT_LIST_MASKED)){
        
        # Get alternate name
        a_name <- ALT_LIST_MASKED[[i]]$Scenario
        
        # Get alternate daily vals
        alt_vals <- ALT_LIST_MASKED[[i]]$daily_mean
        
        # Set species string - needed for DIFF_CHANGE_FUNCTION since species output is daily or yearly
        if(species %in% waders_sp_string){
          pchange_sp_string <- "EverWaders"
        }else{
          pchange_sp_string <- sp_string
        } #close else for sp string
        
        # Calculate percent change 
        # Calculates percent change of landscale means
        # For daily output, calculates percent change of landscape means, and then averages the daily percent change values
        print(paste0("Calculating Percent Change :: ", a_name, "-", b_name, " -- ", Sys.time()))
        pchange <- DIFF_CHANGE_CALC(pchange_sp_string, BASE_LIST_MASKED[[b]]$nc_masked, alt_vals, a_name, base_vals, b_name)
        
        ####
        # add data to percent diff dataframe - if percent diff was calculated during data processing, bind rows to one dataframe
        ####
        #if(grepl(gator_sp_string, FILES$ALL_FILES[1])){
        print(paste0("Adding Percent Difference :: ALT_", a_name, " minus BASE_", b_name))
        #if("Percent_diff_mean" %in% names(map_dfs)){
        if(!is.null(pchange)){
          percent_diff <- bind_rows(percent_diff, pchange)
        } # close if (!is.null(pchange)) 
        
        if(is.null(pchange)){
          percent_diff <- NULL
        } # close if (is.null(change))
      } # Close i
    } # close b
  #} # closes n
    
  # Print list with all processed data  
  process_list_all[[n]] <- process_list
  names(process_list_all)[[n]] <- sp_string[n]
  
  # Export acreage Data
  print(paste0("Export Acreage Data CSV"))
  
  # Tidy scenario_year column to individual columns
  acreage_diff_df <- separate(acreage_diff_df, Scenario_year, into = c("Scenario", "Year"), sep = "_X")
  
  # Generate file name and export acreage table
  output_filename <- paste0(out_path, "Acreage_", gsub(" ", "_", map_title), ".csv")
  output_filename
  write.csv(acreage_diff_df, output_filename, row.names = FALSE)
  
  ####
  # Crop out forst 3 years for Apple snail
  ####
  if("Apple_Snail" %in% sp_string){
    minyear <- as.numeric(min(percent_diff$Year))
    startyr <- minyear+3
    exclude_yrs <- seq(minyear, startyr-1, 1)
    percent_diff <- percent_diff[percent_diff$Year != exclude_yrs,]
  }
  
  ####
  # Export Percent diff data, and make Bar plots
  ####
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
  
  # make bar plots - 1 plot for each alt against all baselines
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
  } # closes alt bar plots
  
  # make bar plots - 1 bar plot for each baselines that shows all alts
  for(l in 1:length(base_names)){
    print(paste0("Making Differnce Bar Plot :: ", base_names[l]))
    
    per_diff_base <- percent_diff[grep(base_names[l], percent_diff$Scenarios),]
    TEST <- PER_DIFF_PLOT_ALTS(
      DF = per_diff_base,
      X_VAR = x_var,
      Y_VAR = y_var,
      FILL_VAR = fill_var,
      TITLE = title,
      Y_LAB = paste0("Percent Change in ", title, "\nfrom Baseline ", base_names[l]),
      X_LAB = x_lab,
      MIN_LIMIT = min_limit,
      MAX_LIMIT = max_limit
    )
    
  diff_plot_filename <- paste0(OUTPUT_PATH, "/PercentDiff_BarPlot_", gsub(" ", "_", title),"_", base_names[l], ".png")
  ggsave(diff_plot_filename, TEST, width=15, height=8.5, units="in", dpi=300, scale = 1)
  } # closes base barplots
  } # closes bar plots if percent diff ! null
  } # closes n

# Save R object that has processed data stored
  if(grepl("EverWaders", FILES$ALL_FILES[1])){
  save_string <- "EverWaders"
  }else{
  save_string <- sp_string}

save(process_list_all, FILES, BASE_LIST_MASKED, ALT_LIST_MASKED, file = paste0(out_path, save_string, "_processed_data.RData"))
#load('../LOSOM/Output/LOSOM_Round1_2021_05/Apple_Snail/Apple_Snail_processed_data.RData')
