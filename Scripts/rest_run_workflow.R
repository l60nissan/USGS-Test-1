## -----------------------------------------------------------------------------
# Workflow for Netcdf output for restoration runs
# Use for Alligator, Everwaders, Snail kite, and Applesnail
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Running Workflow"))


#Load packages
library(tidyverse)
library(ncdf4)
library(raster)
library(sf)
library(cowplot)

# Source dependency scripts
print(paste0("INFO [", Sys.time(), "] Sourcing Dependency Scripts"))
# Functions to process data nd make bar plots
print(paste0("INFO [", Sys.time(), "] Loading Process Functions"))
source("./Scripts/rest_run_process_functions.R")

# Function to generate maps
print(paste0("INFO [", Sys.time(), "] Loading Map Functions"))
source("./Scripts/rest_run_map_functions.R")

# Defined file paths for shapefiles
print(paste0("INFO [", Sys.time(), "] Loading Shapefile Input Paths"))
source("./Scripts/shapefile_paths.R")

# Defined species strings - for consistency across coordinated scripts
print(paste0("INFO [", Sys.time(), "] Loading Species String Definitions"))
source("./Scripts/species_string_definitions.R")

# Message regarding inputs
message("INFO [", Sys.time(), "] USER INPUTS SET TO: \n*parent path: ", parent_path,
        "\n*output path: ", output_path,
        "\n*species: ", paste(sp_string, collapse = " "),
        "\n*croppped(T/F): ", cropped,
        "\n*alternative scenarios: ", paste(alt_names, collapse = " "),
        "\n*basline scenarios: ", paste(base_names, collapse = " "))

#-----------------
# Define strings to avoid hard coding throughout script
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

for (n in seq_along(sp_string)) { # This is to accommodate multiple 
                                 # species output in the everwaders path

  ## Extract species name
  species <- sp_string[n]
  
  print(paste0("INFO [", Sys.time(), "] Running Workflow for: ", species))

  ## List All files, Alternate scenario files, and Baseline files
  print(paste0("INFO [", Sys.time(), "] Pulling Files"))
  files <- SpScenarioFiles(alt_string, base_string, parent_path, sp_string[n])
  print(paste0("INFO [", Sys.time(), "] All Files: "))
  print(files)
  
  alt_list <- files$alt_list

  base_list <- files$base_list

  #-----------------
  # Loop to process data and make maps, acreage table and bar plot 
  
  # Process each baseline and alt combination
  # list to hold processed data
  process_list <- list() 
  # data frame to hold percent difference data for bar plot
  percent_diff <- data.frame() 
  # data frame to hold acreage data
  acreage_diff_df <- data.frame() 

  # Process baseline file b
  for (b in seq_along(base_list)) {  
      
    # Get baseline scenario name
    b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
    # Create empty list for output for each baseline
    process_list[[b]] <- list()
    names(process_list)[[b]] <- b_name
      
    # process each alternate file i with baseline b
    for (i in seq_along(alt_list)) {
      
      # Get alternate scenario name
      a_name <- str_extract_all(alt_list[i], all_scenario_names)[[1]]
      print(paste0("INFO [", Sys.time(), "] Processing :: ALT_", a_name,
                   " minus BASE_", b_name))
      
      #print(paste0("Processing :: ALT_", a_name, " minus BASE_",
      #             b_name, " -- ", Sys.time()))
      
      #-----------------
      # Process data needed to build maps and calculate acreage
      print(paste0("INFO [", Sys.time(), "] Processing map data and calculating acreage"))
      map_dfs <- ProcessOutput(base_list[b], alt_list[i],
                               aoi_path, all_scenario_names)
      print(paste0("INFO [", Sys.time(), "] Files for processing"))
      print(paste0("Baseline file: ", base_list[b]))
      print(paste0("Alternative file: ", alt_list[i]))
      
      #-----------------
      # Add data to list of processed data
      
      process_list[[b]][[i]] <- map_dfs
      names(process_list[[b]])[[i]] <- paste0(map_dfs$alt_name, "-",
                                              map_dfs$base_name)
      
      #-----------------
      # Make maps
      
      print(paste0("INFO [", Sys.time(), "] Making Map :: ALT_", a_name,
                   " minus BASE_", b_name))
    
      # Define inputs for function that makes maps (RestorationRunMap())
      ind_fill <- sym(labs_str)
      dif_fill <- sym(labs_str)
      map_title <- map_dfs$map_title
      out_path <- output_path
      out_file <- paste0(out_path, gsub(" ", sep_str, map_title), sep_str,
                         map_dfs$alt_name, sep_str, map_dfs$base_name, ".pdf")
      print(paste0("INFO [", Sys.time(), "] Map output file: "))
      print(out_file)
      
      
      print(paste0("INFO [", Sys.time(), "] Making map for ",
                   gsub(" ", sep_str, map_title), sep_str,
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
                       landscape = landscape,
                       map_title = map_title,
                       output_file_name = out_file)

      #-----------------
      # Add data to Acreage data frame
      
      print(paste0("INFO [", Sys.time(), "] Adding Acreage :: ALT_",
                   a_name, " minus BASE_", b_name))
      acreage_diff_df <- bind_rows(acreage_diff_df, map_dfs$acreage_df)
    } #close i
  } #close b
      
  #-----------------
  # Process Diff calculations for alternate scenarios and baseline scenarios
  # - Calculate for every species in the sp_string within loop n
  # - if mask is applied, it masks the entire stack

  # Stack NetCDF percent difference calculations and bar plots
  # Mask is applied if cropped = FALSE
  
  #-----------------
  # Stack/mask baseline scenarios and calculate cell stats (landscape means)
  base_list_masked <- list()
  for (b in seq_along(base_list)) {
    b_name <- str_extract_all(base_list[b], all_scenario_names)[[1]]
      
    print(paste0("INFO [", Sys.time(), "] Stacking Base :: ", b_name))

    # Stack the NetCDf - mask applied within StackNcOutput()
    masked <- StackNcOutput(nc_file = base_list[b],
                            masked = cropped,
                            aoi = aoi_path,
                            all_scenario_names = all_scenario_names)

    print(paste0("INFO [", Sys.time(), "] Calculating Landscape Mean Base :: ",
                   b_name, " -- ", Sys.time()))
      
    # Calculate mean of landscape for each day
    daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
    masked$daily_mean <- daily_mean_vals
        
    # add masked cell stats to list
    base_list_masked[[b]] <- masked
    names(base_list_masked)[[b]] <- masked$Scenario 
  } # closes b
      
  #-----------------
  # stack/mask alternate scenarios and calculate cell stats (landscape means)
  alt_list_masked <- list()
  for (i in seq_along(alt_list)) {
    a_name <- str_extract_all(alt_list[i], all_scenario_names)[[1]]
        
    print(paste0("INFO [", Sys.time(), "] Stacking Alt :: ", a_name))
        
    # Stack the NetCDf - mask applied within StackNcOutput()
    masked <- StackNcOutput(alt_list[i],
                            masked = cropped,
                            aoi = aoi_path,
                            all_scenario_names = all_scenario_names)
        
    print(paste0("INFO [", Sys.time(), "] Calculating Landscape Mean Alt :: ",
                   a_name, " -- ", Sys.time()))
        
    # Calculate mean of landscape for each day
    daily_mean_vals <- cellStats(masked$nc_masked, stat = mean, na.rm = TRUE)
    masked$daily_mean <- daily_mean_vals
      
    # Add masked cell stats to list
    alt_list_masked[[i]] <- masked
    names(alt_list_masked)[[i]] <- masked$Scenario 
  } # closes i 
  
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

  for (b in seq_along(base_list_masked)) {  
    
    # Get baseline name
    b_name <- base_list_masked[[b]]$Scenario
      
    # Get baseline daily vals
    base_vals <- base_list_masked[[b]]$daily_mean

    # process alternate file i
    for (i in seq_along(alt_list_masked)) {
        
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
      # Calculate percent change of landscape means
      # For daily output, calculate percent change of landscape means,
      # and then averages the daily percent change values for each year
      print(paste0("INFO [", Sys.time(), "] Calculating Percent Change :: ",
                   a_name, "-", b_name, " -- ", Sys.time()))
      pchange <- DiffChangeCalc(pchange_sp_string,
                                  base_list_masked[[b]]$nc_masked,
                                  alt_vals, a_name, base_vals, b_name)
        
      #-----------------
      # Add data to percent diff data frame - 
      # if percent diff was calculated during data processing,
      # bind rows to one data frame
      print(paste0("INFO [", Sys.time(), "] Adding Percent Difference :: ALT_",
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
  print(paste0("INFO [", Sys.time(), "] Export Acreage Data CSV"))
  
  # Tidy scenario_year column to individual columns
  acreage_diff_df <- separate(acreage_diff_df, Scenario_year,
                              into = c(scenario_str, year_str),
                              sep = scyr_sep_str)
  
  # Generate file name and export acreage table
  output_filename <- paste0(out_path, acreage_str,
                            gsub(" ", sep_str, map_title), csv_str)
  print(paste0("INFO [", Sys.time(), "] Writing acreage table to: ", output_filename))
  write.csv(acreage_diff_df, output_filename, row.names = FALSE)
  
  # Crop out first 3 years for Apple snail due to ramp up of model
  if (apsn_string %in% sp_string) {
    minyear <- as.numeric(min(percent_diff$Year))
    startyr <- minyear + apsn_yr_trim
    exclude_yrs <- seq(minyear, startyr - 1, 1)
    percent_diff <- subset(percent_diff, !(Year %in% exclude_yrs))
  }
  
  #-----------------
  # Export percent difference data, and make bar plots

  # Export data frame with all percent differences
  # If percent difference data frame exists, create a bar plot
  print(paste0("INFO [", Sys.time(), "] Export Percent Difference Data CSV"))
  if (!is.null(percent_diff)) {
    diff_output_filename <- paste0(out_path, perdiff_name_str,
                                   gsub(" ", sep_str, map_title), csv_str)
    print(paste0("INFO [", Sys.time(), "] Percent Difference Data CSV filename: ",
                 diff_output_filename))
    write.csv(percent_diff, diff_output_filename, row.names = FALSE)
    
    # Set levels of percent_diff to match alt_names and base_names order that
    # is set in workflow_inputs.R
    # Create vector of levels
    diff_levels <- apply(expand.grid(alt_names, base_names),
                                        1, paste, collapse = "-")
    # Set factor levels
    percent_diff$Scenarios <- factor(percent_diff$Scenarios,
                                     levels = c(diff_levels))

    # Make Percent difference bar plot
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
    for (a in seq_along(alt_names)) {
      print(paste0("INFO [", Sys.time(), "] Making Differnce Bar Plot :: ",
                   alt_names[a]))
    
      per_diff_alt <- percent_diff[grep(alt_names[a], percent_diff$Scenarios), ]
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
                                   gsub(" ", sep_str, map_title), sep_str,
                                   alt_names[a], png_str)
      ggsave(diff_plot_filename, diff_plot,
             width = bar_width, height = bar_height, units = bar_units,
             dpi = bar_dpi, scale = bar_scale)
    } # closes alt bar plots
  
    # Make bar plots: 1 bar plot for each baseline shown against all alternates
    for (l in seq_along(base_names)) {
      print(paste0("INFO [", Sys.time(), "] Making Differnce Bar Plot :: ",
                   base_names[l]))
    
      per_diff_base <- percent_diff[grep(base_names[l], percent_diff$Scenarios), ]
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
                                   gsub(" ", sep_str, title), sep_str,
                                   base_names[l], png_str)
      ggsave(diff_plot_filename, diff_plot_a,
             width = bar_width, height = bar_height, units = bar_units,
             dpi = bar_dpi, scale = bar_scale)
    } # closes base bar plots
  } # closes bar plots if percent diff is not null
} # closes n

# Save R object that has processed data stored
  if (grepl(waders_string, files$all_files[1])) {
  save_string <- waders_string
  } else {
  save_string <- sp_string
  }

r_data_file <- paste0(out_path, save_string, rdata_str)
print(paste0("INFO [", Sys.time(), "] Saving RData to: ", r_data_file))
save(process_list_all, files, base_list_masked, alt_list_masked,
     file = r_data_file)
