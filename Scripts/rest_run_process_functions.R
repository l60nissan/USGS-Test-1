## -----------------------------------------------------------------------------
# Functions used to process netcdf output from restoration runs
#
# Caitlin Hackett chakett@usgs.gov
## -----------------------------------------------------------------------------
############################################################
#TO DO:
 # * PROCESS_OUTPUT: Update snki kite map_title
 # * PROCESS_OUTPUT: Update set variables for snki
 # * check line 334 - does this match how snki should be processed for daily output?
############################################################

#Pull file names for base and alt scenarios for target species
# Returns a list containing strings of all files, alt files, and base files
sp_scenario_files <- function(alt_names, # names of alternate scenarios
                              base_names, # names of base scenarios
                              folder_path, # path to folder with output
                              species_string # species string to process
                              ){ # species string to process 
  
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

# Make rasters dataframe for plotting
raster_to_df <- function(raster_name, scenario_name){
  ras_df  <- as.data.frame(rasterToPoints(raster_name, xy = TRUE))
  long_df <- pivot_longer(ras_df, cols = c(3:ncol(ras_df)))
  long_df$Scenario <- scenario_name
  return(long_df)
}

# Function to process output - These are helpful when trouble shooting
#BASE_FILE <- BASE_LIST[1]
#ALT_FILE <- ALT_LIST[1]
#AOI_FILE <- AOI_PATH
#BASE_ALT_NAMES <- all_scenario_names
#CROPPED = cropped

ProcessOutput <- function(base_file,     # Baseline netcdf to process
                          alt_file,      # Alternate netcdf to process
                          aoi_file,      # Area of interest - Shapefile
                          base_alt_names ) { # All base and alternate names. format with "|" between names, example ("BASE1|BASE2|ALT1|ALT2")
                          #CROPPED        # TRUE/FALSE, Are base_file and alt_file already cropped to AOI
                          
  # ----
  # Define Strings for each species for Function
  # ----
  
  # Alligator
  gator_string <- "Alligator"
  gator_varname <- "Habitat_Suitability"
  #gator_varname <- "Breeding_Potential"
  gator_years <- paste0("X", c("1978", "1989", "1995"))
  gator_year_labels <- c("Average Year (1978)",
                         "Dry Year (1989)",
                         "Wet Year (1995)")
  
  gator_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
  gator_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30",
                        "0.31 - 0.40", "0.41 - 0.50", "0.51 - 0.60",
                        "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90",
                        "0.91 - 1.00")
  
  gator_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
  gator_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550",
                         "-0.101 to -0.325", "0.099 to -0.100", "0.100 to 0.324",
                         "0.325 to 0.549", "0.550 to 0.774", "0.775 to 1") 
  gator_title <- "Alligator Habitat Suitability Index"
  
  
  # Snail Kite
  snki_string <- "snki"
  snki_varname <- "SNKI_HSI"
  #snki_years <- paste0("X", c("1995.04.20", "2004.04.20"))
  snki_years <- paste0("X", c("1989.04.20" , "1995.04.20"))
  snki_year_labels <- c("Dry Year (April 20, 1989)", "Wet Year (April 20, 1995)")
  
  snki_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
  snki_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30",
                       "0.31 - 0.40", "0.41 - 0.50", "0.51 - 0.60",
                       "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90", "0.91 - 1.00")
  
  snki_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
  snki_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550",
                        "-0.101 to -0.325", "0.099 to -0.100", "0.100 to 0.324",
                        "0.325 to 0.549", "0.550 to 0.774", "0.775 to 1") 
  snki_title <- "Snail Kite Nesting Relative Selection"
  
  
  # Days Since Dry
  dsd_string <- "dsd"
  dsd_varname <- "dsd"
  dsd_years <- paste0("X", c("1978.06.01", "1989.06.01", "1995.06.01"))
  dsd_year_labels <- c("Average Year (June 1, 1978)", "Dry Year (June 1, 1989)",
                       "Wet Year (June 1, 1995)")
  
  dsd_ind_cuts <- c(0.0, 110,  219,  329,  438,  548,  657,  767,  876,  986, Inf)
  dsd_ind_labels <- c("0 - 109", "110 - 218", "219 - 328", "329 - 437",
                      "438 - 547", "548 - 656", "657 - 766", "767 - 875",
                      "876 - 985", "986 - 1,095+")
  
  dsd_diff_cuts <- seq(from = -1095, to = 1095, length.out = 10)
  dsd_diff_cuts <- c(-Inf, -852, -608, -365, -122, 122, 365, 608, 852, Inf)
  dsd_diff_labels <- c("-853 to -1,095+", "-609 to -852", "-366 to -608",
                       "-123 to -365", "121 to -122", "122 to 364", "365 to 607",
                       "608 to 851", "852 to 1,095+") 
  dsd_title <- "Days Since Drydown"
  
  
  # Apple Snail
  apsn_string <- "Apple_Snail"
  apsn_varname <- "snailPopulationAdults"
  #apsn_years <- paste0("X", c("1995.04.20", "2004.04.20"))
  apsn_years <- paste0("X", c("1989.04.20" , "1995.04.20"))
  apsn_year_labels <- c("Dry Year (April 20, 1989)", "Wet Year (April 20, 1995)")
  
  apsn_ind_cuts <- c(-Inf, 1, 15001, 30001, 45001, 60001, 75001, 90001,
                     105001, 120001, Inf)
  apsn_ind_labels <- c("0", "1 - 15,000", "15,001 - 30,000", "30,001 - 45,000",
                       "45,001 - 60,000", "60,001 - 75,000", "75,001 - 90,000",
                       "90,001 - 105,000", "105,001 - 120,000",
                       "120,001 - 140,000+")
  
  apsn_diff_cuts <- c(-140000, -107500, -75000, -42500, -10000, 10001, 42501,
                      75001, 107501, 140000)
  apsn_diff_labels <- c("-107,501 to -140,000", "-75,001 to -107,500",
                        "-42,501 to 75,000", "-10,001 to -42,500",
                        "10,000 to -10,000", "10,001 to 42,500",
                        "42,501 to 75,000", "75,001 to 107,500",
                        "107,501 to 140,000")
  apsn_title <- "Adult Apple Snail Population"
  
  
  # EverWaders
  waders_string <- "EverWaders"
  waders_varname <- "_Occupancy"
  waders_years <- paste0("X", c("1978", "1989", "1995"))
  waders_year_labels <- c("Average Year (1978)", "Dry Year (1989)",
                          "Wet Year (1995)")

  waders_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
  waders_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30",
                         "0.31 - 0.40", "0.41 - 0.50", "0.51 - 0.60",
                         "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90",
                         "0.91 - 1.00")
  
  waders_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
  waders_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550",
                          "-0.101 to -0.325", ".099 to -0.100", "0.100 to 0.324",
                          "0.325 to 0.549", ".550 to .774", "0.775 to 1") 
  
  waders_sp_abr <- c("GBHE", "GLIB", "GREG", "LBHE", "ROSP", "WHIB", "WOST")
  waders_sp_name <- c("Great Blue Heron", "Glossy Ibis", "Great Egret",
                      "Little Blue Heron", "Roseate Spoonbill",
                      "White Ibis", "Wood Stork")
  names(waders_sp_abr) <- waders_sp_name
  
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
    wadervar <- gsub(".*EverWaders_|\\.nc.*", "", wadervar)
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
    end_year <- start_year + (time_length - 1) # subtract 1 to account for starting year in length
    
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
  AOI <- shapefile(aoi_file)
  
  base_mask <- mask(base_target, AOI)
  alt_mask <- mask(alt_target, AOI)
  
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
  base_df <- raster_to_df(raster_name = base_mask, scenario_name = base_name)
  alt_df <- raster_to_df(raster_name = alt_mask, scenario_name = alt_name)
  diff_df <- raster_to_df(raster_name = alt_base, scenario_name = diff_name)
  
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

# Make percent diff Bar plot
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

##############################
# Calculate Percent change
# cell stats alredy calculated to landscape means
##############################
DIFF_CHANGE_CALC <- function(species_string, #species string -- match sp_string set at beginign of workflow script
                             NC_STACK,       # One nc stack to extract layer names
                             ALT_VALS,       # landscape means for alternate scenario
                             ALT_SCEN,       # name of alternate scenario
                             BASE_VALS,      # landscape means for baseline scenario
                             BASE_SCEN){     # name of baseline scenario
  
  #Get diff name
  diff_name <- paste0(ALT_SCEN, "-", BASE_SCEN)
  
  # Get species string
  species_string <- species_string
  
  if(grepl(paste0("Alligator", "|", "EverWaders"), species_string)){
    
    #Calculate percent diffrence of the landscape mean
    per_diff <- as.data.frame(((ALT_VALS - BASE_VALS)/BASE_VALS)*100)
      
    #Make data frame for bar chart
    rownames(per_diff) <- sub("X", "", rownames(per_diff))
    colnames(per_diff) <- "Percent_Difference"
    per_diff$Scenarios <- diff_name
    per_diff$Year <- row.names(per_diff)
    per_diff_mean <- per_diff
  } 
  
  if(grepl(paste0("Apple_Snail", "|", "SnailKite"), species_string)){
    
    # Calculate daily percent change across the landscape between alt and baseline
    
    #percent difference of the daily landscape meand
    per_diff <- as.data.frame(((ALT_VALS - BASE_VALS)/BASE_VALS)*100)

    # calculate average of daily percent change for each year
    #Get indices for years
    indicies <- format(as.Date(names(NC_STACK), format="X%Y.%m.%d"), format="%Y")
    indicies <- indicies
    names(per_diff) <- "Percent_Difference"
    per_diff$Year <- indicies
    
    #Mean across year
    per_diff_mean <- per_diff%>%
      group_by(Year)%>%
      summarise("Percent_Difference" = mean(Percent_Difference))
    
    per_diff_mean$Scenarios <- diff_name
  } 
  
  # Set dataframe as NULL for dsd. If not, will get error that object does not exist when writing out to list at end of function
  if(grepl("dsd", species_string)){
    per_diff_mean <- NULL
  }
  
  return(per_diff_mean)
}


##############################
# FUNCTION TO MASK NETCDF
##############################

MASK_NC_OUTPUT <- function(NC_FILE, AOI, ALL_SCENARIO_NAMES){
  
  #Extract NAME FROM FILE
  scenario_name <- str_extract_all(NC_FILE, ALL_SCENARIO_NAMES)[[1]]
  
  # Load shapefile
  AOI_mask <- shapefile(AOI)
  
  # ----
  # Define Strings for each species for Function
  # ----

  # Alligator
  gator_string <- "Alligator"
  gator_varname <- "Habitat_Suitability"

  # Snail Kite
  snki_string <- "snki"
  snki_varname <- "SNKI_HSI"

  # Days Since Dry
  dsd_string <- "dsd"
  dsd_varname <- "dsd"

  # Apple Snail
  apsn_string <- "Apple_Snail"
  apsn_varname <- "snailPopulationAdults"

  # EverWaders
  waders_string <- "EverWaders"
  waders_varname <- "_Occupancy"
  
  ###
  # Set read in varname
  ###
  
  if(grepl(gator_string, NC_FILE)){
    nc_varname <- gator_varname
    daily_output <- FALSE
  }
  
  if(grepl(snki_string, NC_FILE)){
    nc_varname <- snki_varname
    daily_output <- TRUE
  }
  
  if(grepl(dsd_string, NC_FILE)){
    nc_varname <- dsd_varname
    daily_output <- TRUE
  }
  
  if(grepl(apsn_string, NC_FILE)){
    nc_varname <- apsn_varname
    daily_output <- TRUE
  }
  
  if(grepl(waders_string, NC_FILE)){
    nc_varname <- waders_varname
    species <- gsub(".*EverWaders_|\\.nc.*", "", NC_FILE)
    nc_varname <- paste0(species, waders_varname)
    daily_output <- FALSE
  }
  
    # ----
  # Get layer/Band names from Netcdf
  # ----
  nc <- nc_open(NC_FILE)
  
  # Get dates for bands
  time_att <- nc$dim$t$units
  time_length <- as.numeric(nc$dim$t$len)
  time_split <- strsplit(time_att, split = " ")
  time_split <- strsplit(time_split[[1]][3], split = "T")
  start_date <- as.Date(time_split[[1]][1])
  start_year <- as.numeric(format(start_date, format = "%Y"))
  
  if(daily_output){
    end_year <- start_date + (time_length - 1)
    
    band_years <- seq(start_date, end_year, 1)
    band_years <- paste0("X", band_years)
    #BAND_YEARS
  } else {
    end_year <- start_year + (time_length - 1) # subtract 1 to account for starting year in length
    
    # Sequence years for band names
    band_years <- seq(start_year, end_year, 1)
    band_years <- paste0("X", band_years)
    #BAND_YEARS
  }
  
  nc_close(nc) # Clsoe netcdf
  
  # ----
  # Process for raw difference 
  # ----
  
  # Load files
  nc_stack <- stack(NC_FILE, varname = nc_varname)

  # Add names to bands
  names(nc_stack) <- band_years
  
  #nc_stack <- nc_stack[[1:2]] # subset used when testing function to increase speed
  
  # Apply Mask
  nc_masked <- mask(nc_stack, AOI_mask)
  
  nc_masked_list <- list("nc_masked" = nc_masked, "Scenario" = scenario_name, "Variable" = nc_varname)
  return(nc_masked_list)
}

##############################
# Function to stack nc output if
# already masked
##############################

STACK_NC_OUTPUT <- function(NC_FILE, AOI, ALL_SCENARIO_NAMES){
  
  #Extract NAME FROM FILE
  scenario_name <- str_extract_all(NC_FILE, ALL_SCENARIO_NAMES)[[1]]
  
  # ----
  # Define Strings for each species for Function
  # ----
  
  # Alligator
  gator_string <- "Alligator"
  gator_varname <- "Habitat_Suitability"
  
  # Snail Kite
  snki_string <- "snki"
  snki_varname <- "SNKI_HSI"
  
  # Days Since Dry
  dsd_string <- "dsd"
  dsd_varname <- "dsd"
  
  # Apple Snail
  apsn_string <- "Apple_Snail"
  apsn_varname <- "snailPopulationAdults"
  
  # EverWaders
  waders_string <- "EverWaders"
  waders_varname <- "_Occupancy"
  
  ###
  # Set read in varname
  ###
  
  if(grepl(gator_string, NC_FILE)){
    nc_varname <- gator_varname
    daily_output <- FALSE
  }
  
  if(grepl(snki_string, NC_FILE)){
    nc_varname <- snki_varname
    daily_output <- TRUE
  }
  
  if(grepl(dsd_string, NC_FILE)){
    nc_varname <- dsd_varname
    daily_output <- TRUE
  }
  
  if(grepl(apsn_string, NC_FILE)){
    nc_varname <- apsn_varname
    daily_output <- TRUE
  }
  
  if(grepl(waders_string, NC_FILE)){
    nc_varname <- waders_varname
    species <- gsub(".*EverWaders_|\\.nc.*", "", NC_FILE)
    nc_varname <- paste0(species, waders_varname)
    daily_output <- FALSE
  }
  
  # ----
  # Get layer/Band names from Netcdf
  # ----
  nc <- nc_open(NC_FILE)
  
  # Get dates for bands
  time_att <- nc$dim$t$units
  time_length <- as.numeric(nc$dim$t$len)
  time_split <- strsplit(time_att, split = " ")
  time_split <- strsplit(time_split[[1]][3], split = "T")
  start_date <- as.Date(time_split[[1]][1])
  start_year <- as.numeric(format(start_date, format = "%Y"))
  
  if(daily_output){
    end_year <- start_date + (time_length - 1)
    
    band_years <- seq(start_date, end_year, 1)
    band_years <- paste0("X", band_years)
    #BAND_YEARS
  } else {
    end_year <- start_year + (time_length - 1) # subtract 1 to account for starting year in length
    
    # Sequence years for band names
    band_years <- seq(start_year, end_year, 1)
    band_years <- paste0("X", band_years)
    #BAND_YEARS
  }
  
  nc_close(nc) # Clsoe netcdf
  
  # ----
  # Process for raw difference 
  # ----
  
  # Load files
  nc_stack <- stack(NC_FILE, varname = nc_varname)
  
  # Add names to bands
  names(nc_stack) <- band_years
  
  nc_masked_list <- list("nc_masked" = nc_stack, "Scenario" = scenario_name, "Variable" = nc_varname)
  return(nc_masked_list)
}


