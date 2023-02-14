## -----------------------------------------------------------------------------
# Functions used to process netcdf output from restoration runs
#
# Caitlin Hackett chakett@usgs.gov
## -----------------------------------------------------------------------------


## -----------------------------------------------------------------------------
# Pull file names for base and alternate scenarios for target species
# Returns a list containing strings of all files, alt files, and base files
SpScenarioFiles <- function(alt_names, # names of alternate scenarios
                            base_names, # names of base scenarios
                            folder_path, # path to folder with output
                            species_string ){ # species string to process
                             
  # All Files
  all_files <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
  all_files
  
  # Alt Files
  alt_names <- paste0(alt_names, collapse = "|")
  alt_list <- intersect(grep(alt_names, value = TRUE, all_files),
                        grep(species_string, value = TRUE, all_files))
  alt_list
  
  # Base Files
  base_names <- paste0(base_names, collapse = "|")
  base_list <- intersect(grep(base_names, value = TRUE, all_files),
                         grep(species_string, value = TRUE, all_files))
  base_list
  
  # Alt and Base Names
  if (length(base_names) == 1) {base_names <- paste0(base_names, "|")}
  all_scenario_names <- paste0(base_names, alt_names, collapse = "|")
  all_scenario_names
  
  file_name_list <- list("all_files" = all_files, "alt_list" = alt_list,
                         "base_list" = base_list, "species" = species_string)
  return(file_name_list)
}

## -----------------------------------------------------------------------------
# Make rasters dataframe for plotting
RasterToDf <- function(raster_name, scenario_name){
  ras_df  <- as.data.frame(rasterToPoints(raster_name, xy = TRUE))
  long_df <- pivot_longer(ras_df, cols = c(3:ncol(ras_df)))
  long_df$Scenario <- scenario_name
  return(long_df)
}

## -----------------------------------------------------------------------------
# Process output for maps and acreage tables
ProcessOutput <- function(
    base_file,     # Baseline netcdf to process
    alt_file,      # Alternate netcdf to process
    aoi_file,      # Area of interest - Shapefile
    base_alt_names ) { # All base and alternate names. format with "|" between
                       # names, example ("BASE1|BASE2|ALT1|ALT2")

  # ----
  # Define Strings for each species for Function
  # ----
  source("./Scripts/species_string_definitions.R")
  source("./Scripts/process_definitions.R")
  
  #----
  ## Set variables based on species 
  #----
  
  # If Species is Alligator
  if (grepl(gator_string, base_file)) {
    
    ind_cuts <- gator_ind_cuts
    ind_labs <- gator_ind_labels
    
    diff_cuts <- gator_diff_cuts
    diff_labs <- gator_diff_labels
    
    target_var <- gator_varname
    target_years <- gator_years
    target_years_labels <- gator_year_labels
    
    output_title <- gator_title
    daily_output <- FALSE
  }
  
  # If Species is snail kite
  if (grepl(snki_string, base_file)) {
    
    ind_cuts <- snki_ind_cuts
    ind_labs <- snki_ind_labels
    
    diff_cuts <- snki_diff_cuts
    diff_labs <- snki_diff_labels
    
    target_var <- snki_varname
    target_years <- snki_years
    target_years_labels <- snki_year_labels
    
    output_title <- snki_title
    daily_output <- TRUE
  }
  
  # If Species is DaysSinceDryDown
  if (grepl(dsd_string, base_file)) {
    
    ind_cuts <- dsd_ind_cuts
    ind_labs <- dsd_ind_labels
    
    diff_cuts <- dsd_diff_cuts
    diff_labs <- dsd_diff_labels
    
    target_var <- dsd_varname
    target_years <- dsd_years
    target_years_labels <- dsd_year_labels
    
    output_title <- dsd_title
    daily_output <- TRUE
  }
  
  # If species is Applesnail
  if (grepl(apsn_string, base_file)) {
    ind_cuts <- apsn_ind_cuts
    ind_labs <- apsn_ind_labels
    
    diff_cuts <- apsn_diff_cuts
    diff_labs <- apsn_diff_labels
    
    target_var <- apsn_varname
    target_years <- apsn_years
    target_years_labels <- apsn_year_labels
    
    output_title <- apsn_title
    daily_output <- TRUE
  }
  
  # If Species is Everwaders
  if (grepl(waders_string, base_file)) {
    wadervar <- base_file
    wadervar <- str_extract(wadervar, paste0(waders_sp_abr, collapse = "|"))
    sp_waders_varname <- paste0(wadervar, waders_varname)
    
    ind_cuts <- waders_ind_cuts
    ind_labs <- waders_ind_labels
    
    diff_cuts <- waders_diff_cuts
    diff_labs <- waders_diff_labels
    
    target_var <- sp_waders_varname
    target_years <- waders_years
    target_years_labels <- waders_year_labels
    
    full_sp_name <-  grep(wadervar, waders_sp_abr, value = TRUE)
    output_title <- paste0(names(full_sp_name), sub("_", " ", waders_varname))
    daily_output <- FALSE
  }
  
  # ----
  # Get layer/Band names from Netcdf
  # ----
  nc <- nc_open(base_file)
  
  # Get dates for bands
  time_att <- nc$dim$t$units
  time_length <- as.numeric(nc$dim$t$len)
  time_split <- strsplit(time_att, split = " ")
  time_split <- strsplit(time_split[[1]][3], split = "T")
  start_date <- as.Date(time_split[[1]][1])
  start_year <- as.numeric(format(start_date, format = "%Y"))
  
  if (daily_output) {
    end_year <- start_date + (time_length - 1)
    
    band_years <- seq(start_date, end_year, 1)
    band_years <- paste0("X", band_years)
    #BAND_YEARS
  } else {
    # subtract 1 to account for starting year in length
    end_year <- start_year + (time_length - 1) 
    
    # Sequence years for band names
    band_years <- seq(start_year, end_year, 1)
    band_years <- paste0("X", band_years)
    #BAND_YEARS
  }
  
  nc_close(nc) # Close netcdf
  
  # ----
  # Process for raw difference 
  # ----
  
  # Load files
  base_stack <- stack(base_file, varname = target_var)
  alt_stack <- stack(alt_file, varname = target_var)
  
  # Add names to bands
  names(base_stack) <- band_years
  names(alt_stack) <- band_years
  
  # Extract the dates of interest
  base_target <- subset(base_stack, target_years)
  alt_target <- subset(alt_stack, target_years)
  
  # Mask the subsets to AOI
  aoi <- shapefile(aoi_file)
  
  base_mask <- mask(base_target, aoi)
  alt_mask <- mask(alt_target, aoi)
  
  # Do raster math to get difference
  alt_base <- alt_mask - base_mask
  names(alt_base) <- target_years # in case the raster math drops layer names
  
  #Extract alt and base name from files
  base_name <- str_extract_all(base_file, base_alt_names)[[1]]
  base_name
  alt_name <- str_extract_all(alt_file, base_alt_names)[[1]]
  alt_name
  diff_name <- paste0(alt_name, "-", base_name)
  diff_name
  
  # ----
  # Make histogram of differences for acreage tables
  # ----
  
  acreage_list <- list() 
  for (n in 1:nlayers(alt_base)) {
    diff_hist <- hist(alt_base[[n]], breaks = diff_cuts,
                      right = FALSE, plot = FALSE)
    
    if (grepl(paste0(gator_string, "|", apsn_string, "|", waders_string, "|",
                     snki_string, "|", dsd_string), base_file)) {
      # Convert number of cells to number of acres
      # Each cell is 400m x 400 m; 4046.86 sq meters = 1 acres
      diff_acres <- diff_hist$counts * 400 * 400 / 4046.86
    }
    
    #if(grepl("MARL", base_file)){
      # for marl prairie **marl prairie uses different mesh!
     # diff_acres <- diff_hist$counts * 478.95 * 478.95 / 4046.86
    #}
    
    # Build data frame for export to csv
    diff_bins <- paste0("[", as.character(diff_cuts)[1:(length(diff_cuts) - 1)],
                        " - ", as.character(diff_cuts)[2:length(diff_cuts)], ")")
    acreage_diff_df <- data.frame(`Difference from baseline` = rev(diff_bins),
                          `Number of acres` = rev(diff_acres),
                          check.names = FALSE)
    acreage_list[[n]] <- acreage_diff_df
    names(acreage_list)[[n]] <- paste0(diff_name,"_", names(alt_base)[n])
  }
  acreage_df <- bind_rows(acreage_list, .id = "Scenario_year")
  
  # ----
  # Make data frames for plotting individual score and difference maps
  # ----
  
  # Make raster df and include Scenario name - for maps
  base_df <- RasterToDf(raster_name = base_mask, scenario_name = base_name)
  alt_df <- RasterToDf(raster_name = alt_mask, scenario_name = alt_name)
  diff_df <- RasterToDf(raster_name = alt_base, scenario_name = diff_name)
  
  # Combine individual score dataframes for base and alt
  ind_df <- bind_rows(base_df, alt_df)
  
  #Remove NA cells
  ind_df <- filter(ind_df, !is.na(value) | !is.nan(value))
  diff_df <- filter(diff_df, !is.na(value) | !is.nan(value))
  
  # Add breaks and labels to the dataframes - for plotting maps
  ind_df$breaks <- cut(ind_df$value, ind_cuts,
                       right = FALSE, include.lowest = TRUE)
  ind_df$labs   <- cut(ind_df$value, ind_cuts,
                       ind_labs, right = FALSE, include.lowest = TRUE)
  
  diff_df$breaks <- cut(diff_df$value, diff_cuts,
                        right = FALSE, include.lowest = TRUE)
  diff_df$labs <- cut(diff_df$value, diff_cuts,
                      diff_labs, right = FALSE, include.lowest = TRUE)
  
  # set levels so all get plotted
  ind_df$labs <- factor(ind_df$labs, levels = ind_labs)
  diff_df$labs <- factor(diff_df$labs, levels = diff_labs)
  
  ind_df$name <- factor(ind_df$name, levels = target_years)
  diff_df$name <- factor(diff_df$name, levels = target_years)
  
  ind_df$Scenario <- factor(ind_df$Scenario,
                            levels = c(base_name, alt_name, diff_name))
  diff_df$Scenario <- factor(diff_df$Scenario,
                            levels = c(base_name, alt_name, diff_name))
  
  #Name the Target Years to display on figure facet labels
  name.labs <- target_years_labels
  names(name.labs) <- target_years
  
  df_list <- list("ind_df" = ind_df, "diff_df" = diff_df,
                  "alt_name" = alt_name, "base_name" = base_name, 
                  "name.labs" = name.labs, "map_title" = output_title,
                  "acreage_df" = acreage_df)
  return(df_list)
}

## -----------------------------------------------------------------------------
# Make percent difference bar plot alternate shown against all baselines
PerDiffPlot <- function(df, # dataframe to plot
                        x_var, # string of x variable
                        y_var, # string of y variable
                        fill_var, # string of fill variable
                        title, # string of plot title
                        y_lab, # string of y label
                        x_lab, # string of x label
                        min_limit, # value of minimum value for y scale
                        max_limit){ # values of maximum value for y scale
  diff_plot <- ggplot(data = df, aes(x = !!sym(x_var), y = !!sym(y_var), 
                                    fill = !!sym(fill_var))) + 
    geom_bar(stat = "identity", position = "dodge",
             width = 0.7,colour = "black") + 
    labs(y = y_lab, x = x_lab, title = title,
         fill = "Percent Change \nfrom Baseline:") +
    scale_y_continuous(limits = c(min_limit, max_limit)) +
    theme(axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 20),
          axis.text.x = element_text(size = 12, angle = 70, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15), 
          legend.title.align = 0.5,
          # Set margins for plot - wider on right side to make space fore legend
          plot.margin = margin(1, .5, 1, .5, unit = "in")) 
  
  return(diff_plot)
}

## -----------------------------------------------------------------------------
# Make percent difference bar plot each baseline shown against all alternates
PerDiffPlotAlt <- function(df, # dataframe to plot
                          x_var, # string of x variable
                          y_var, # string of y variable
                          fill_var, # string of fill variable
                          title, # string of plot title
                          y_lab, # string of y label
                          x_lab, # string of x label
                          min_limit, # value of minimum value for y scale
                          max_limit){ # values of maximum value for y scale
  bar_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  diff_plot <- ggplot(data = df, aes(x = !!sym(x_var), y = !!sym(y_var),
                                     fill = !!sym(fill_var))) + 
    geom_bar(stat = "identity", position = "dodge",
             width = 0.7, colour = "black") + 
    scale_fill_manual(values = bar_pal ) +
    labs(y = y_lab, x = x_lab, title = title,
         fill = "Percent Change \nfrom Baseline:") +
    scale_y_continuous(limits = c(min_limit, max_limit)) +
    theme(axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 20),
          axis.text.x = element_text(size = 12, angle = 70, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15), 
          legend.title.align = 0.5,
          # Set margins for plot - wider on right side to make space fore legend
          plot.margin = margin(1, .5, 1, .5, unit = "in")) 
  
  return(diff_plot)
}

## -----------------------------------------------------------------------------
# Calculate Percent change
# cell stats alredy calculated to landscape means

DiffChangeCalc <- function(
  species_string, # species string -- match sp_string set in workflow 
  nc_stack,       # one nc stack to extract layer names
  alt_vals,       # landscape means for alternate scenario
  alt_scen,       # name of alternate scenario
  base_vals,      # landscape means for baseline scenario
  base_scen){     # name of baseline scenario
  
  source("./scripts/species_string_definitions.R")
  
  #Get diff name
  diff_name <- paste0(alt_scen, "-", base_scen)
  
  # Get species string
  species_string <- species_string
  
  if (grepl(paste0(gator_string, "|", waders_string), species_string)) {
    
    #Calculate percent diffrence of the landscape mean
    per_diff <- as.data.frame(((alt_vals - base_vals)/base_vals)*100)
      
    # Make data frame for bar chart
    rownames(per_diff) <- sub("X", "", rownames(per_diff))
    colnames(per_diff) <- "Percent_Difference"
    per_diff$Scenarios <- diff_name
    per_diff$Year <- row.names(per_diff)
    per_diff_mean <- per_diff
  } 
  
  if (grepl(paste0(apsn_string, "|", snki_string), species_string)) {
    
    # Calculate daily percent change across the landscape between alt and baseline
    
    #percent difference of the daily landscape meand
    per_diff <- as.data.frame(((alt_vals - base_vals)/base_vals)*100)

    # calculate average of daily percent change for each year
    #Get indices for years
    indicies <- format(as.Date(names(nc_stack),
                               format = "X%Y.%m.%d"), format = "%Y")
    indicies <- indicies
    names(per_diff) <- "Percent_Difference"
    per_diff$Year <- indicies
    
    #Mean across year
    per_diff_mean <- per_diff %>%
      group_by(Year) %>%
      summarise("Percent_Difference" = mean(Percent_Difference))
    
    per_diff_mean$Scenarios <- diff_name
  } 
  
  # Set dataframe as NULL for dsd. If not, will get error that object
  # does not exist when writing out to list at end of function
  if (grepl(dsd_string, species_string)) {
    per_diff_mean <- NULL
  }
  
  return(per_diff_mean)
}


## -----------------------------------------------------------------------------
# Mask NetCDF and return a list with:
# nc_masked: input NetCDF masked to defined "aoi"
# Scenario: Scenario name for input/masked NetCDF
# Varible: NetCDF variable for input/masked NetCDF

MaskNcOutput <- function(nc_file, # full path to NetCDF file to mask
                         aoi, # shapefile path of mask
                         all_scenario_names){ # character string of scenario
                                              # names separated by '|' 
  
  #Extract NAME FROM FILE
  scenario_name <- str_extract_all(nc_file, all_scenario_names)[[1]]
  
  # Load shapefile
  aoi_mask <- shapefile(aoi)
  
  # ----
  # Define Strings for each species for Function
  # ----
  source("./Scripts/species_string_definitions.R")
  
  ###
  # Set read in varname
  ###
  
  if (grepl(gator_string, nc_file)) {
    nc_varname <- gator_varname
    daily_output <- FALSE
  }
  
  if (grepl(snki_string, nc_file)) {
    nc_varname <- snki_varname
    daily_output <- TRUE
  }
  
  if (grepl(dsd_string, nc_file)) {
    nc_varname <- dsd_varname
    daily_output <- TRUE
  }
  
  if (grepl(apsn_string, nc_file)) {
    nc_varname <- apsn_varname
    daily_output <- TRUE
  }
  
  if (grepl(waders_string, nc_file)) {
    nc_varname <- waders_varname
    species <- gsub(".*EverWaders_|\\.nc.*", "", nc_file)
    nc_varname <- paste0(species, waders_varname)
    daily_output <- FALSE
  }
  
  # ----
  # Get layer/Band names from Netcdf
  # ----
  nc <- nc_open(nc_file)
  
  # Get dates for bands
  time_att <- nc$dim$t$units
  time_length <- as.numeric(nc$dim$t$len)
  time_split <- strsplit(time_att, split = " ")
  time_split <- strsplit(time_split[[1]][3], split = "T")
  start_date <- as.Date(time_split[[1]][1])
  start_year <- as.numeric(format(start_date, format = "%Y"))
  
  if (daily_output) {
    end_year <- start_date + (time_length - 1)
    
    band_years <- seq(start_date, end_year, 1)
    band_years <- paste0("X", band_years)
  } else {
    # subtract 1 to account for starting year in length
    end_year <- start_year + (time_length - 1) 
    
    # Sequence years for band names
    band_years <- seq(start_year, end_year, 1)
    band_years <- paste0("X", band_years)
  }
  
  nc_close(nc) # Clsoe netcdf
  
  # ----
  # Process for raw difference 
  # ----
  
  # Load files
  nc_stack <- stack(nc_file, varname = nc_varname)

  # Add names to bands
  names(nc_stack) <- band_years
  
  # Apply Mask
  nc_masked <- mask(nc_stack, aoi_mask)
  
  nc_masked_list <- list("nc_masked" = nc_masked,
                         "Scenario" = scenario_name,
                         "Variable" = nc_varname)
  return(nc_masked_list)
}

## -----------------------------------------------------------------------------
# Mask NetCDF and return a list with:
# nc_masked: input NetCDF that is already masked to defined "aoi"
# Scenario: Scenario name for input/masked NetCDF
# Varible: NetCDF variable for input/masked NetCDF

StackNcOutput <- function(nc_file, # full path to NetCDF file to mask
                          aoi, # shapefile path of mask
                          all_scenario_names){ # character string of scenario
                                               # names separated by '|' 
  
  #Extract NAME FROM FILE
  scenario_name <- str_extract_all(nc_file, all_scenario_names)[[1]]
  
  # ----
  # Define Strings for each species for Function
  # ----
  source("./Scripts/species_string_definitions.R")
  
  ###
  # Set read in varname
  ###
  
  if (grepl(gator_string, nc_file)) {
    nc_varname <- gator_varname
    daily_output <- FALSE
  }
  
  if (grepl(snki_string, nc_file)) {
    nc_varname <- snki_varname
    daily_output <- TRUE
  }
  
  if (grepl(dsd_string, nc_file)) {
    nc_varname <- dsd_varname
    daily_output <- TRUE
  }
  
  if (grepl(apsn_string, nc_file)) {
    nc_varname <- apsn_varname
    daily_output <- TRUE
  }
  
  if (grepl(waders_string, nc_file)) {
    nc_varname <- waders_varname
    species <- gsub(".*EverWaders_|\\.nc.*", "", nc_file)
    nc_varname <- paste0(species, waders_varname)
    daily_output <- FALSE
  }
  
  # ----
  # Get layer/Band names from Netcdf
  # ----
  nc <- nc_open(nc_file)
  
  # Get dates for bands
  time_att <- nc$dim$t$units
  time_length <- as.numeric(nc$dim$t$len)
  time_split <- strsplit(time_att, split = " ")
  time_split <- strsplit(time_split[[1]][3], split = "T")
  start_date <- as.Date(time_split[[1]][1])
  start_year <- as.numeric(format(start_date, format = "%Y"))
  
  if (daily_output) {
    end_year <- start_date + (time_length - 1)
    
    band_years <- seq(start_date, end_year, 1)
    band_years <- paste0("X", band_years)
  } else {
    # subtract 1 to account for starting year in length
    end_year <- start_year + (time_length - 1) 
    # Sequence years for band names
    band_years <- seq(start_year, end_year, 1)
    band_years <- paste0("X", band_years)
  }
  
  nc_close(nc) # Clsoe netcdf
  
  # ----
  # Process for raw difference 
  # ----
  
  # Load files
  nc_stack <- stack(nc_file, varname = nc_varname)
  
  # Add names to bands
  names(nc_stack) <- band_years
  
  nc_masked_list <- list("nc_masked" = nc_stack,
                         "Scenario" = scenario_name,
                         "Variable" = nc_varname)
  return(nc_masked_list)
}


