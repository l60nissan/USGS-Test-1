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

# Source dependancy scripts
# Functions to process data nd make bar plots
source("./Scripts/rest_run_process_functions.R")

# Function to generate maps
source("./Scripts/rest_run_map_functions.R")

# Defined file paths for shapefiles
source("./Scripts/input_paths.R")

# Defined species strings - for consistency across coordinated scripts
source("./Scripts/species_string_definitions.R")

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
# START: USER SET FACTORS

# Folder containing species output
parent_path <- "../LOSOM/Data/LOSOM_Round3_2021_12/Model Output/Alligator/JEM_Alligator_Production_Probability_Model_Data/"

# Folder to output CSV and figures - 
# include "/" after path to get file names correct when saving 
output_path <- "../data_release_develop/"
output_path

# set target species from species string options at top of script
sp_string <- gator_string
sp_string

# Is Species output already cropped to AOI? (TRUE/FALSE)
cropped <- TRUE
cropped

# Set Alternate scenario names
alt_names <- c("PA22", "PA25")
alt_names

# Set Baseline scenario names
base_names <- c("ECB19", "NA22F", "NA25F")
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
# Define strings to avoid hard coding thorughout script
collapse_str <- "|"
sep_str <- "_"
labs_str <- "labs"
base_mask_str <- "_base_mask_test.jpg"
alt_mask_str <- "_alt_mask_test.jpg"
scenario_str <- "Scenario"
year_str <- "Year"
scyr_sep_str <- "_X"
acreage_str <- "Acreage_"
csv_str <- ".csv"
perdiff_name_str <- "PercentDiff_"
perdiff_var_str <- "Percent_Difference"
scenarios_str <- "Scenarios"
round_accuracy <- 5
min_max_adjust <- 10
bar_title_str1 <- "Percent Change in "
bar_title_str2 <- "\nfrom Baseline to "
bar_title_str3 <- "\nfrom Baseline "
bar_out_str <- "/PercentDiff_BarPlot_"
png_str <- ".png"
bar_width <- 15
bar_height <- 8.5
bar_units <- "in"
bar_dpi <- 300
bar_scale <- 1
rdata_str <- "_processed_data.RData"
apsn_yr_trim <- 3

#-----------------
# Set scenario names

# Scenario Name strings
alt_string <- paste0(alt_names, collapse = collapse_str)
alt_string

# Base Name strings
base_string <- paste0(base_names, collapse = collapse_str)
base_string

# Alt and Base Names
all_scenario_names <- paste0(paste0(base_string, collapse_str),
                             alt_string, collapse = collapse_str)
all_scenario_names

#-----------------
# If Everwaders, set sp_string to list of species to process
if (sp_string == waders_string) {
  sp_string <- waders_sp_process
}

#-----------------
# Loop through species, process output, output acreage csv and map
# Loop through target species
process_list_all <- list() # list to store all output
#n <- 1
for (n in 1:length(sp_string)) { # This is to accomodate multiple 
                                 # species output in the everwaders path

  ## Extract species name
  species <- sp_string[n]
  
  ## List All files, Alternate scenario files, and Baseline files
  files <- SpScenarioFiles(alt_string, base_string, parent_path, sp_string[n])
  files
  alt_list <- files$alt_list
  alt_list
  base_list <- files$base_list
  base_list
  
  #-----------------
  # Loop to process data and make maps, acreage table and barplot 
  
  # Process each baselinee and alt combination
  # list to hold processed data
  process_list <- list() 
  # data frame to hold percent difference data for barplot
  percent_diff <- data.frame() 
  # data frame to hold acreage data
  acreage_diff_df <- data.frame() 
    #b <- 1
    #i <- 1
    
  # Process baseline file b
  for (b in 1:length(base_list)) {  
      
    # CGet baseline scenario name
    b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
    # Create empty list for output for each baseline
    process_list[[b]] <- list()
    names(process_list)[[b]] <- b_name
      
    # process each alternate file i with baseline b
    for (i in 1:length(alt_list)) {
      
      # Get alternate scenario name
      a_name <- str_extract_all(alt_list[i], all_scenario_names)[[1]]
      print(paste0("Processing :: ALT_", a_name, " minus BASE_",
                   b_name, " -- ", Sys.time()))
      
      #-----------------
      # Process data needed to build maps and calculate acreage

      map_dfs <- ProcessOutput(base_list[b], alt_list[i],
                               aoi_path, all_scenario_names)
      
      #-----------------
      # Add data to list of processed data

      process_list[[b]][[i]] <- map_dfs
      names(process_list[[b]])[[i]] <- paste0(map_dfs$alt_name, "-",
                                              map_dfs$base_name)
      
      #-----------------
      # Make maps

      print(paste0("Making Map :: ALT_", a_name, " minus BASE_", b_name))
      
      # Define inputs for function that makes maps (RestorationRunMap())
      ind_fill <- sym(labs_str)
      dif_fill <- sym(labs_str)
      map_title <- map_dfs$map_title
      out_path <- output_path
      out_file <- paste0(out_path, gsub(" ", sep_str, map_title), sep_str,
                         map_dfs$alt_name, sep_str, map_dfs$base_name, ".pdf")
      out_file
  
      print(paste0("Making map for ", gsub(" ", sep_str, map_title), sep_str,
                   map_dfs$alt_name, sep_str, map_dfs$base_name))
  
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
  # Process Diff calculations for alternate scenarios and baseline scenarios
  # - Calculate for every species in the sp_string within loop n
  # - if mask is applied, it masks the entire stack.

  # Mask files and read as stack
  # - For percent difference calculations and barplots
  
  # If input nc is not cropped, apply aoi mask before calculations
  if (!cropped) {
  
    #-----------------
    # Mask baseline scenarios and calculate cell stats (landcape means)
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
        
      # add masked cell stats to list
      base_list_masked[[b]] <- masked
      names(base_list_masked)[[b]] <- masked$Scenario 
    } # closes b
      
    #-----------------
    # Mask alternate scenarios and calculate cell stats (landscape means)
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
        
      # add masked cell stats to list
      alt_list_masked[[i]] <- masked
      names(alt_list_masked)[[i]] <- masked$Scenario 
    } # closes i 
  
  } else {
  # If input nc is cropped calculate cell stats without applying aoi mask
  
    #-----------------
    # Read baseline scenarios and calculate cell stats
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
        
      # add cell stats to list
      base_list_masked[[b]] <- masked
      names(base_list_masked)[[b]] <- masked$Scenario
    } # closes b
    
    #-----------------
    # Read alternate scenarios and calculate cell stats
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
        
      # add cell stats to list
      alt_list_masked[[i]] <- masked
      names(alt_list_masked)[[i]] <- masked$Scenario 
    } # closes i 
  } # closes else
    
  #-----------------
  ## Export first baseline and alternate scenarios slices to double check
  # that data is masked for calculations
  jpeg(paste0(output_path, species, base_mask_str))
  plot(base_list_masked[[1]][[1]][[1]])
  dev.off()
      
  jpeg(paste0(output_path, species, alt_mask_str))
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
      
    # Get baseline daily vals
    base_vals <- base_list_masked[[b]]$daily_mean

    # process alternate file i
    for (i in 1:length(alt_list_masked)) {
        
      # Get alternate name
      a_name <- alt_list_masked[[i]]$Scenario
        
      # Get alternate daily vals
      alt_vals <- alt_list_masked[[i]]$daily_mean
        
      # Set species string - needed for DiffChangeCalc()
      # since species output is daily or yearly
      if (species %in% waders_sp_process) {
        pchange_sp_string <- waders_string
      } else {
        pchange_sp_string <- sp_string
      } #close else for sp string
      
      #-----------------
      # Calculate percent change 
      # Calculate percent change of landscale means
      # For daily output, calculate percent change of landscape means,
      # and then averages the daily percent change values for each year
      print(paste0("Calculating Percent Change :: ",
                   a_name, "-", b_name, " -- ", Sys.time()))
      pchange <- DiffChangeCalc(pchange_sp_string,
                                  base_list_masked[[b]]$nc_masked,
                                  alt_vals, a_name, base_vals, b_name)
        
      #-----------------
      # Add data to percent diff dataframe - 
      # if percent diff was calculated during data processing,
      # bind rows to one dataframe
      print(paste0("Adding Percent Difference :: ALT_",
                   a_name, " minus BASE_", b_name))
      if (!is.null(pchange)) {
        percent_diff <- bind_rows(percent_diff, pchange)
      } # close if (!is.null(pchange)) 
        
      if (is.null(pchange)) { # no percent change calculated for dsd
        percent_diff <- NULL
      } # close if (is.null(pchange))
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
                              into = c(scenario_str, year_str),
                              sep = scyr_sep_str)
  
  # Generate file name and export acreage table
  output_filename <- paste0(out_path, acreage_str,
                            gsub(" ", sep_str, map_title), csv_str)
  output_filename
  write.csv(acreage_diff_df, output_filename, row.names = FALSE)
  
  # Crop out first 3 years for Apple snail due to ramp up of model
  if (apsn_string %in% sp_string) {
    minyear <- as.numeric(min(percent_diff$Year))
    startyr <- minyear + apsn_yr_trim
    exclude_yrs <- seq(minyear, startyr - 1, 1)
    percent_diff <- percent_diff[percent_diff$Year != exclude_yrs,]
  }
  
  #-----------------
  # Export percent difference data, and make bar plots

  # Export dataframe with all percent differences
  # If percent diffrence data frame exists, create a bar plot
  print(paste0("Export Percent Difference Data CSV"))
  if (!is.null(percent_diff)) {
    diff_output_filename <- paste0(out_path, perdiff_name_str,
                                   gsub(" ", sep_str, map_title), csv_str)
    write.csv(percent_diff, diff_output_filename, row.names = FALSE)
  
    # Make Percent diffrence bar plot
    # Define inputs for function PerDiffPlot()
    x_var <- year_str
    y_var <- perdiff_var_str
    fill_var <- scenarios_str
    title <- map_dfs$map_title
    x_lab <- year_str
    min_limit <- plyr::round_any(min(percent_diff[y_var]), round_accuracy,
                                 f = floor) - min_max_adjust
    max_limit <- plyr::round_any(max(percent_diff[y_var]), round_accuracy,
                                 f = ceiling) + min_max_adjust
  
    # make bar plots: 1 plot for each alternate shown against all baselines
    for (a in 1:length(alt_names)) {
      print(paste0("Making Differnce Bar Plot :: ", alt_names[a]))
    
      per_diff_alt <- percent_diff[grep(alt_names[a], percent_diff$Scenarios),]
      diff_plot <- PerDiffPlot(
        df = per_diff_alt,
        x_var = x_var,
        y_var = y_var,
        fill_var = fill_var,
        title = title,
        y_lab = paste0(bar_title_str1, title, bar_title_str2, alt_names[a]),
        x_lab = x_lab,
        min_limit = min_limit,
        max_limit = max_limit
      )
      diff_plot_filename <- paste0(out_path, bar_out_str,
                                   gsub(" ", sep_str, map_title),sep_str,
                                   alt_names[a], png_str)
      ggsave(diff_plot_filename, diff_plot,
             width = bar_width, height = bar_height, units = bar_units,
             dpi = bar_dpi, scale = bar_scale)
    } # closes alt bar plots
  
    # Make bar plots: 1 bar plot for each baseline shown against all alternates
    for (l in 1:length(base_names)) {
      print(paste0("Making Differnce Bar Plot :: ", base_names[l]))
    
      per_diff_base <- percent_diff[grep(base_names[l], percent_diff$Scenarios),]
      diff_plot_a <- PerDiffPlotAlt(
        df = per_diff_base,
        x_var = x_var,
        y_var = y_var,
        fill_var = fill_var,
        title = title,
        y_lab = paste0(bar_title_str1, title, bar_title_str3, base_names[l]),
        x_lab = x_lab,
        min_limit = min_limit,
        max_limit = max_limit
      )
    
      diff_plot_filename <- paste0(output_path, bar_out_str,
                                   gsub(" ", sep_str, title),sep_str,
                                   base_names[l], png_str)
      ggsave(diff_plot_filename, diff_plot_a,
             width = bar_width, height = bar_height, units = bar_units,
             dpi = bar_dpi, scale = bar_scale)
    } # closes base barplots
  } # closes bar plots if percent diff is not null
} # closes n

# Save R object that has processed data stored
  if (grepl(waders_string, files$all_files[1])) {
  save_string <- waders_string
  } else {
  save_string <- sp_string}

save(process_list_all, files, base_list_masked, alt_list_masked,
     file = paste0(out_path, save_string, rdata_str))
