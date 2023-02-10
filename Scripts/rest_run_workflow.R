## -----------------------------------------------------------------------------
# Workflow for Netcdf output for restoration runs
# Use for Alligator, Everwaders, Snail kite, and Applesnail
#
# Caitlin Hackett chackett@usgs.gov
## -----------------------------------------------------------------------------

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

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
# START: USER SET FACTORS

# Folder containing species output
parent_path <- "../LOSOM/Data/LOSOM_Round3_2021_12/Model Output/Alligator/JEM_Alligator_Production_Probability_Model_Data/JEM_Alligator_Production_Probability_Model_Data/"
parent_path

# Folder to output CSV and figures - 
# include "/" after path to get file names correct when saving 
output_path <- "../data_release_develop/"
output_path

# set target species from species string options at top of script
sp_string <- gator_sp_string
sp_string

# Is Species output already cropped to AOI? (TRUE/FALSE)
cropped <- FALSE
cropped

# Set Alternate scenario names
alt_names <- c("PA22", "PA25")
alt_names

# Set Baseline scenario names
base_names <- c("ECB19", "NA22f", "NA25f")
base_names

message("USER INPUTS SET TO: \n*parent path: ", print(parent_path),
        "\n*output path: ", print(output_path),
        "\n*species: ", paste(sp_string, collapse = " "),
        "\n*croppped(T/F): ", print(cropped),
        "\n*alternative scenarios: ", paste(alt_names, collapse = " "),
        "\n*basline scenarios: ", paste(base_names, collapse = " "))

# END: USER SET FACTORS
## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------

#-----------------
# Set scenario names

# Scenario Name strings
alt_string <- paste0(alt_names, collapse = "|")
alt_string

# Base Name strings
base_string <- paste0(base_names, collapse = "|")
base_string

# Alt and Base Names
all_scenario_names <- paste0(paste0(base_string, "|"),
                             alt_string, collapse = "|")
all_scenario_names

#-----------------
# Loop through species, process output, output acreage csv and map
# Loop through target species
process_list_all <- list() # list to store all output
#n <- 1
for (n in 1:length(sp_string)) { # This part is to accomodate multiple 
                                 # species output in the everwaders path

  ## Extract species name
  species <- sp_string[n]
  
  ## List All files, Alternate scenario files, and Baseline files
  files <- sp_scenario_files(alt_string, base_string, parent_path, sp_string[n])
  files
  alt_list <- files$alt_list
  alt_list
  base_list <- files$base_list
  base_list
  
  #-----------------
  # Loop to process data and make maps, acreage table and barplot 
  
  # Process each baselinee and alt combination
  process_list <- list() # list to hold processed data
  percent_diff <- data.frame() # data frame to hold percent difference data for barplot
  acreage_diff_df <- data.frame() # data frame to hold acreage data 
    #b <- 1
    #i <- 1
    
  # Process baseline file b
  for (b in 1:length(base_list)) {  
      
    # Create empty list for output for each baseline
    b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
    process_list[[b]] <- list()
    names(process_list)[[b]] <- b_name
      
    # process alternate file i 
    for (i in 1:length(alt_list)) {
      
      a_name <- str_extract_all(alt_list[i], all_scenario_names)[[1]]
      print(paste0("Processing :: ALT_", a_name, " minus BASE_",
                   b_name, " -- ", Sys.time()))
      
      #-----------------
      # Process data to build maps and calculate acreage

      map_dfs <- ProcessOutput(base_list[b], alt_list[i],
                               aoi_path, all_scenario_names)#, CROPPED = cropped)
      
      #-----------------
      # Add data to list of processed data

      process_list[[b]][[i]] <- map_dfs
      names(process_list[[b]])[[i]] <- paste0(map_dfs$alt_name, "-",
                                              map_dfs$base_name)
      
      #-----------------
      # Make maps

      print(paste0("Making Map :: ALT_", a_name, " minus BASE_", b_name))
      
      # Make Map
      ind_fill <- sym('labs')
      dif_fill <- 'labs'
      map_title <- map_dfs$map_title
      out_path <- output_path
      out_file <- paste0(out_path, gsub(" ", "_", map_title), "_",
                         map_dfs$alt_name, "_", map_dfs$base_name, ".pdf")
      out_file
  
      print(paste0("Making map for ", gsub(" ", "_", map_title), "_",
                   map_dfs$alt_name, "_", map_dfs$base_name))
  
      name.labs <- map_dfs$name.labs
  
      RestorationRunMap(df_ind = map_dfs$ind_df,
                       ind_fill = ind_fill,
                       df_dif = map_dfs$diff_df,
                       dif_fill = dif_fill,
                       scenario_col = names(map_dfs$ind_df)[5],
                       year_col = names(map_dfs$ind_df)[3],
                       aoi_path = aoi_path,
                       mpr_path = mpr_path,
                       wcas_path = wcas_path,
                       fl_path = fl_path,
                       map_extent = map_extent,
                       map_title = map_title,
                       output_file_name = out_file)

      #-----------------
      # Add data to Acreage dataframe
      
      print(paste0("Adding Acreage :: ALT_", a_name, " minus BASE_", b_name))
      acreage_diff_df <- bind_rows(acreage_diff_df, map_dfs$acreage_df)
    } #close i
  } #close b
      
  #-----------------
  # Process Diff calculations for Alts and baselines
  # - Calculate for every species in the sp_string
  # - if mask is applied, it masks the entire stack.

  # Mask FILES and Read as stack - For percent diff calcilations and barplots
  if (!cropped) {
  
    #-----------------
    # MASK BASELINES & CALC CELL STATS (landscape means)
    base_list_masked <- list()
    for (b in 1:length(base_list)) {
      b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
        
      print(paste0("Masking Base :: ", b_name))
        
      # Mask the nc stack
      masked <- MaskNcOutput(base_list[b], aoi_path, all_scenario_names)
        
      print(paste0("Calculating Landscape Mean Base :: ",
                   b_name, " -- ", Sys.time()))
        
      # Calculate mean of landscape for each day
      daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
      masked$daily_mean <- daily_mean_vals
        
      base_list_masked[[b]] <- masked
      names(base_list_masked)[[b]] <- masked$Scenario 
    } # closes b
      
    #-----------------
    # MASK ALTERNATES & CALC CELL STATS (landscale means)
    alt_list_masked <- list()
    for (i in 1:length(alt_list)) {
      a_name <- str_extract_all(alt_list[i], all_scenario_names)[[1]]
        
      print(paste0("Masking Alt :: ", a_name))
        
      # Mask the nc stack
      masked <- MaskNcOutput(alt_list[i], aoi_path, all_scenario_names)
        
      print(paste0("Calculating Landscape Mean Alt :: ",
                   a_name, " -- ", Sys.time()))
        
      # Calculate mean of landscape for each day
      daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
      masked$daily_mean <- daily_mean_vals
        
      alt_list_masked[[i]] <- masked
      names(alt_list_masked)[[i]] <- masked$Scenario 
    } # closes i 
  } else {
     
    #-----------------
    # READ BASELINES & CALC CELL STATS
    base_list_masked <- list()
    for (b in 1:length(base_list)) {
      b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
        
      print(paste0("Stacking Base :: ", b_name,  " -- ", Sys.time()))
        
      # Read nc stack - no masking since already masked (cropped = TRUE)
      masked <- StackNcOutput(base_list[b], aoi_path, all_scenario_names)
        
      print(paste0("Calculating Landscape Mean Base :: ",
                   b_name, " -- ", Sys.time()))
        
      # Calculate mean of landscape for each day
      daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
      masked$daily_mean <- daily_mean_vals
        
      base_list_masked[[b]] <- masked
      names(base_list_masked)[[b]] <- masked$Scenario
    } # closes b
    
    #-----------------
    # READ ALTERNATES & CALC CELL STATS
    alt_list_masked <- list()
    for (i in 1:length(alt_list)) {
      a_name <- str_extract_all(alt_list[i], all_scenario_names)[[1]]
        
      print(paste0("Stacking Alt :: ", a_name))
        
      # Read nc stack - no masking since already masked (cropped = TRUE)
      masked <- StackNcOutput(alt_list[i], aoi_path, all_scenario_names)
        
      print(paste0("Calculating Landscape Mean Alt :: ",
                   a_name, " -- ", Sys.time()))
        
      # Calculate mean of landscape for each day
      daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
      masked$daily_mean <- daily_mean_vals
        
      alt_list_masked[[i]] <- masked
      names(alt_list_masked)[[i]] <- masked$Scenario 
    } # closes i 
  } # closes else
    
  #-----------------
  ## Export first baseline and alt slices to double check masked for calculations
  jpeg(paste0(output_path, species, "_base_mask_test.jpg"))
  plot(base_list_masked[[1]][[1]][[1]])
  dev.off()
      
  jpeg(paste0(output_path, species, "_alt_mask_test.jpg"))
  plot(alt_list_masked[[1]][[1]][[1]])
  dev.off()
    
  #-----------------
  #Process data to calculate percent difference
  
  # Process baseline file b
  percent_diff <- data.frame() # list to hold processed data

  #index <- 0
  for (b in 1:length(base_list_masked)) {  
    
    # Get baseline name
    b_name <- base_list_masked[[b]]$Scenario
      
    # Get Baseline daily vals
    base_vals <- base_list_masked[[b]]$daily_mean

    # process alternate file i
    for (i in 1:length(alt_list_masked)) {
        
      # Get alternate name
      a_name <- alt_list_masked[[i]]$Scenario
        
      # Get alternate daily vals
      alt_vals <- alt_list_masked[[i]]$daily_mean
        
      # Set species string - needed for DIFF_CHANGE_FUNCTION
      # since species output is daily or yearly
      if (species %in% waders_sp_string) {
        pchange_sp_string <- "EverWaders"
      } else {
        pchange_sp_string <- sp_string
      } #close else for sp string
      
      #-----------------
      # Calculate percent change 
      # Calculates percent change of landscale means
      # For daily output, calculates percent change of landscape means,
      # and then averages the daily percent change values
      print(paste0("Calculating Percent Change :: ",
                   a_name, "-", b_name, " -- ", Sys.time()))
      pchange <- DIFF_CHANGE_CALC(pchange_sp_string,
                                  base_list_masked[[b]]$nc_masked,
                                  alt_vals, a_name, base_vals, b_name)
        
      #-----------------
      # add data to percent diff dataframe - 
      # if percent diff was calculated during data processing,
      # bind rows to one dataframe
      print(paste0("Adding Percent Difference :: ALT_",
                   a_name, " minus BASE_", b_name))
      if (!is.null(pchange)) {
        percent_diff <- bind_rows(percent_diff, pchange)
      } # close if (!is.null(pchange)) 
        
      if(is.null(pchange)){
        percent_diff <- NULL
      } # close if (is.null(change))
    } # Close i
  } # close b

  # Print list with all processed data  
  process_list_all[[n]] <- process_list
  names(process_list_all)[[n]] <- sp_string[n]
  
  #-----------------
  # Export acreage Data
  print(paste0("Export Acreage Data CSV"))
  
  # Tidy scenario_year column to individual columns
  acreage_diff_df <- separate(acreage_diff_df, Scenario_year,
                              into = c("Scenario", "Year"), sep = "_X")
  
  # Generate file name and export acreage table
  output_filename <- paste0(out_path, "Acreage_",
                            gsub(" ", "_", map_title), ".csv")
  output_filename
  write.csv(acreage_diff_df, output_filename, row.names = FALSE)
  
  # Crop out first 3 years for Apple snail
  if ("Apple_Snail" %in% sp_string) {
    minyear <- as.numeric(min(percent_diff$Year))
    startyr <- minyear + 3
    exclude_yrs <- seq(minyear, startyr - 1, 1)
    percent_diff <- percent_diff[percent_diff$Year != exclude_yrs,]
  }
  
  #-----------------
  # Export Percent diff data, and make Bar plots

  # Export dataframe with all percent differences
  # If percent diffrence data frame exists, create a bar plot
  print(paste0("Export Percent Difference Data CSV"))
  if (!is.null(percent_diff)) {
    diff_output_filename <- paste0(out_path, "PercentDiff_",
                                   gsub(" ", "_", map_title), ".csv")
    write.csv(percent_diff, diff_output_filename, row.names = FALSE)
  
    # Make Percent diffrence bar plot
    x_var <- "Year"
    y_var <- "Percent_Difference"
    fill_var <- "Scenarios"
    title <- map_dfs$map_title
    x_lab <- "Year"
    min_limit <- plyr::round_any(min(percent_diff[y_var]), 5, f = floor) - 10
    max_limit <- plyr::round_any(max(percent_diff[y_var]), 5, f = ceiling) + 10
  
    # make bar plots - 1 plot for each alt against all baselines
    for (a in 1:length(alt_names)) {
      print(paste0("Making Differnce Bar Plot :: ", alt_names[a]))
    
      per_diff_alt <- percent_diff[grep(alt_names[a], percent_diff$Scenarios),]
      diff_plot <- PerDiffPlot(
        df = per_diff_alt,
        x_var = x_var,
        y_var = y_var,
        fill_var = fill_var,
        title = title,
        y_lab = paste0("Percent Change in ", title, "\nfrom Baseline to ", alt_names[a]),
        x_lab = x_lab,
        min_limit = min_limit,
        max_limit = max_limit
      )
      diff_plot_filename <- paste0(out_path, "PercentDiff_BarPlot_",
                                   gsub(" ", "_", map_title),"_",
                                   alt_names[a], ".png")
      ggsave(diff_plot_filename, diff_plot,
             width = 15, height = 8.5, units = "in", dpi = 300, scale = 1)
    } # closes alt bar plots
  
    # make bar plots - 1 bar plot for each baselines that shows all alts
    for (l in 1:length(base_names)) {
      print(paste0("Making Differnce Bar Plot :: ", base_names[l]))
    
      per_diff_base <- percent_diff[grep(base_names[l], percent_diff$Scenarios),]
      diff_plot_a <- PerDiffPlotAlt(
        df = per_diff_base,
        x_var = x_var,
        y_var = y_var,
        fill_var = fill_var,
        title = title,
        y_lab = paste0("Percent Change in ", title, "\nfrom Baseline ", base_names[l]),
        x_lab = x_lab,
        min_limit = min_limit,
        max_limit = max_limit
      )
    
      diff_plot_filename <- paste0(output_path, "/PercentDiff_BarPlot_",
                                   gsub(" ", "_", title),"_",
                                   base_names[l], ".png")
      ggsave(diff_plot_filename, diff_plot_a,
             width = 15, height = 8.5, units = "in", dpi = 300, scale = 1)
    } # closes base barplots
  } # closes bar plots if percent diff ! null
} # closes n

# Save R object that has processed data stored
  if (grepl("EverWaders", files$all_files[1])) {
  save_string <- "EverWaders"
  } else {
  save_string <- sp_string}

save(process_list_all, files, base_list_masked, alt_list_masked,
     file = paste0(out_path, save_string, "_processed_data.RData"))
