############################################################
# Functions used to process netcdf output from restoration runs
#
# Caitlin Hackett chakett@usgs.gov
############################################################
############################################################
#TO DO:
 # * PROCESS_OUTPUT: Update snki kite map_title
 # * PROCESS_OUTPUT: Update set variables for snki
 # * check line 334 - does this match how snki should be processed for daily output?
############################################################

#Pull file names for base and alt scenarios for target species
# Returns a list containing strings of all files, alt files, and base files
SP_SCENARIO_FILES <- function(alt_names, base_names, folder_path, species_string){
  
  # All Files
  ALL_FILES <- list.files(folder_path, full.names = TRUE, recursive = TRUE)
  ALL_FILES
  
  # Alt Files
  alt_names <- paste0(alt_names, collapse = "|")
  ALT_LIST <- intersect(grep(alt_names, value = TRUE, ALL_FILES), grep(species_string, value = TRUE, ALL_FILES))
  ALT_LIST
  
  # Base Files
  base_names <- paste0(base_names, collapse = "|")
  BASE_LIST <- intersect(grep(base_names, value = TRUE, ALL_FILES), grep(species_string, value = TRUE, ALL_FILES))
  BASE_LIST
  
  # Alt and Base Names
  if(length(base_names) == 1){base_names <- paste0(BASE_NAMES, "|")}
  all_scenario_names <- paste0(base_names, ALT_NAMES, collapse = "|")
  all_scenario_names
  
  file_name_list <- list("ALL_FILES" = ALL_FILES, "ALT_LIST" = ALT_LIST, "BASE_LIST" = BASE_LIST, "SPECIES" = species_string)
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

PROCESS_OUTPUT <- function(BASE_FILE,     # Baseline netcdf to process
                           ALT_FILE,      # Alternate netcdf to process
                           AOI_FILE,      # Area of interest - Shapefile
                           BASE_ALT_NAMES,# All base and alternate names. format with "|" between names, example ("BASE1|BASE2|ALT1|ALT2")
                           CROPPED        # TRUE/FALSE, Are BASE_FILE and ALT_FILE already cropped to AOI
                           ){
  
  # ----
  # Define Strings for each species for Function
  # ----
  
  # Alligator
  gator_string <- "Alligator"
  gator_varname <- "Habitat_Suitability"
  #gator_varname <- "Breeding_Potential"
  gator_years <- paste0("X", c("1978", "1989", "1995"))
  gator_year_labels <- c("Average Year (1978)", "Dry Year (1989)", "Wet Year (1995)")
  
  gator_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
  gator_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30", "0.31 - 0.40", "0.41 - 0.50",
                        "0.51 - 0.60", "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90", "0.91 - 1.00")
  
  gator_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
  gator_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550", "-0.101 to -0.325",
                         "0.099 to -0.100", "0.100 to 0.324", "0.325 to 0.549", "0.550 to 0.774", "0.775 to 1") 
  gator_title <- "Alligator Habitat Suitability Index"
  
  
  # Snail Kite
  snki_string <- "snki"
  snki_varname <- "SNKI HSI"
  snki_years <- paste0("X", c("1995.04.20", "2004.04.20"))
  snki_year_labels <- c("Wet Year (April 20, 1995)", "Dry Year (April 20, 2004)")
  
  snki_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
  snki_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30", "0.31 - 0.40", "0.41 - 0.50",
                        "0.51 - 0.60", "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90", "0.91 - 1.00")
  
  snki_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
  snki_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550", "-0.101 to -0.325",
                         "0.099 to -0.100", "0.100 to 0.324", "0.325 to 0.549", "0.550 to 0.774", "0.775 to 1") 
  snki_title <- "Snail Kite Habitat Suitability Index"
  
  
  # Days Since Dry
  dsd_string <- "dsd"
  dsd_varname <- "dsd"
  dsd_years <- paste0("X", c("1978.06.01", "1989.06.01", "1995.06.01"))
  dsd_year_labels <- c("Average Year (Jun 1, 1978)", "Dry Year (June 1, 1989)", "Wet Year (Jun 1, 1995)")
  
  dsd_ind_cuts <- c(0.0, 110,  219,  329,  438,  548,  657,  767,  876,  986, Inf)
  dsd_ind_labels <- c("0 - 109", "110 - 218", "219 - 328", "329 - 437", "438 - 547", "548 - 656", "657 - 766",
                      "767 - 875", "876 - 985", "986 - 1,095+")
  
  dsd_diff_cuts <- seq(from = -1095, to = 1095, length.out = 10)
  dsd_diff_cuts <- c(-Inf, -852, -608, -365, -122, 122, 365, 608, 852, Inf)
  dsd_diff_labels <- c("-853 to -1,095+", "-609 to -852", "-366 to -608", "-123 to -365", "121 to -122", "122 to 364", "365 to 607",
                       "608 to 851", "852 to 1,095+") 
  dsd_title <- "Days Since Drydown"
  
  
  # Apple Snail
  apsn_string <- "Apple_Snail"
  apsn_varname <- "snailPopulationAdults"
  apsn_years <- paste0("X", c("1995.04.20", "2004.04.20"))
  apsn_year_labels <- c("Wet Year (April 20, 1995)", "Dry Year (April 20, 2004)")
  
  apsn_ind_cuts <- c(-Inf, 1, 15001, 30001, 45001, 60001, 75001, 90001, 105001, 120001, Inf)
  apsn_ind_labels <- c("0", "1 - 15,000", "15,001 - 30,000", "30,001 - 45,000", "45,001 - 60,000", "60,001 - 75,000",
                       "75,001 - 90,000", "90,001 - 105,000", "105,001 - 120,000", "120,001 - 140,000+")
  
  apsn_diff_cuts <- c(-140000, -107500, -75000, -42500, -10000, 10001, 42501, 75001, 107501, 140000)
  apsn_diff_labels <- c("-107,501 to -140,000", "-75,001 to -107,500", "-42,501 to 75,000", "-10,001 to -42,500", "10,000 to -10,000",
                        "10,001 to 42,500", "42,501 to 75,000", "75,001 to 107,500", "107,501 to 140,000")
  apsn_title <- "Adult Apple Snail Population"
  
  
  # EverWaders
  waders_string <- "EverWaders"
  waders_varname <- "_Occupancy"
  waders_years <- paste0("X", c("1978", "1989", "1995"))
  waders_year_labels <- c("Average Year (1978)", "Dry Year (1989)", "Wet Year (1995)")
  #waders_years <- paste0("X", c("2021")) - used when testing with everforecast output
  #waders_year_labels <- c("TESTING")
  
  waders_ind_cuts <- seq(from = 0.0, to = 1, by = 0.10)
  waders_ind_labels <- c("0.00 - 0.10", "0.11 - 0.20", "0.21 - 0.30", "0.31 - 0.40", "0.41 - 0.50",
                         "0.51 - 0.60", "0.61 - 0.70", "0.71 - 0.80", "0.81 - 0.90", "0.91 - 1.00")
  
  waders_diff_cuts <- c(-1, -.775, -.55, -.325, -.10, .10, .325, .55, .775, 1)
  waders_diff_labels <- c("-0.776 to -1", "-0.551 to -0.775", "-0.326 to -0.550", "-0.101 To -0.325",
                          "-0.100 to .099", "0.100 to 0.324", "0.325 to 0.549", ".550 to .774", "0.775 to 1") 
  
  waders_sp_abr <- c("GBHE", "GLIB", "GREG", "LBHE", "ROSP", "WHIB", "WOST")
  waders_sp_name <- c("Great Blue Heron", "Glossy Ibis", "Great Egret", "Little Blue Heron", "Roseate Spoonbill", "White Ibis", "Wood Stork")
  names(waders_sp_abr) <- waders_sp_name
  
  #----
  ## Set variables based on species 
  #----
  
  # If Species is Alligator
  if(grepl(gator_string, BASE_FILE)){
    
    ind_cuts <- gator_ind_cuts
    ind_labs <- gator_ind_labels
    
    diff_cuts <- gator_diff_cuts
    diff_labs <- gator_diff_labels
    
    TARGET_VAR <- gator_varname
    TARGET_YEARS <- gator_years
    TARGET_YEARS_LABELS <- gator_year_labels
    
    output_title <- gator_title
    DAILY_OUTPUT <- FALSE
  }
  
  # If Species is snail kite
  if(grepl(snki_string, BASE_FILE)){
    
    ind_cuts <- snki_ind_cuts
    ind_labs <- snki_ind_labels
    
    diff_cuts <- snki_diff_cuts
    diff_labs <- snki_diff_labels
    
    TARGET_VAR <- snki_varname
    TARGET_YEARS <- snki_years
    TARGET_YEARS_LABELS <- snki_year_labels
    
    output_title <- snki_title
    DAILY_OUTPUT <- TRUE
  }
  
  # If Species is DaysSinceDryDown
  if(grepl(dsd_string, BASE_FILE)){
    
    ind_cuts <- dsd_ind_cuts
    ind_labs <- dsd_ind_labels
    
    diff_cuts <- dsd_diff_cuts
    diff_labs <- dsd_diff_labels
    
    TARGET_VAR <- dsd_varname
    TARGET_YEARS <- dsd_years
    TARGET_YEARS_LABELS <- dsd_year_labels
    
    output_title <- dsd_title
    DAILY_OUTPUT <- TRUE
  }
  
  # If species is Applesnail
  if(grepl(apsn_string, BASE_FILE)){
    ind_cuts <- apsn_ind_cuts
    ind_labs <- apsn_ind_labels
    
    diff_cuts <- apsn_diff_cuts
    diff_labs <- apsn_diff_labels
    
    TARGET_VAR <- apsn_varname
    TARGET_YEARS <- apsn_years
    TARGET_YEARS_LABELS <- apsn_year_labels
    
    output_title <- apsn_title
    DAILY_OUTPUT <- TRUE
  }
  
  # If Species is Everwaders
  if(grepl(waders_string, BASE_FILE)) {
    wadervar <- BASE_FILE
    wadervar <- gsub(".*EverWaders_|\\.nc.*", "", wadervar)
    sp_waders_varname <- paste0(wadervar, waders_varname)
    
    ind_cuts <- waders_ind_cuts
    ind_labs <- waders_ind_labels
    
    diff_cuts <- waders_diff_cuts
    diff_labs <- waders_diff_labels
    
    TARGET_VAR <- sp_waders_varname
    TARGET_YEARS <- waders_years
    TARGET_YEARS_LABELS <- waders_year_labels
    
    full_sp_name <-  grep(wadervar, waders_sp_abr, value = TRUE)
    output_title <- paste0(names(full_sp_name), sub("_", " ", waders_varname))
    DAILY_OUTPUT <- FALSE
  }
  
  # ----
  # Get layer/Band names from Netcdf
  # ----
  nc <- nc_open(BASE_FILE)
  
  # Get dates for bands
  time_att <- nc$dim$t$units
  time_length <- as.numeric(nc$dim$t$len)
  time_split <- strsplit(time_att, split = " ")
  time_split <- strsplit(time_split[[1]][3], split = "T")
  start_date <- as.Date(time_split[[1]][1])
  start_year <- as.numeric(format(start_date, format = "%Y"))
  
  if(DAILY_OUTPUT){
    end_year <- start_date + (time_length - 1)
    
    BAND_YEARS <- seq(start_date, end_year, 1)
    BAND_YEARS <- paste0("X", BAND_YEARS)
    #BAND_YEARS
  } else {
    end_year <- start_year + (time_length - 1) # subtract 1 to account for starting year in length
    
    # Sequence years for band names
    BAND_YEARS <- seq(start_year, end_year, 1)
    BAND_YEARS <- paste0("X", BAND_YEARS)
    #BAND_YEARS
  }
  
  nc_close(nc) # Clsoe netcdf
  
  # ----
  # Process for raw difference 
  # ----
  
  # Load files
  base_stack <- stack(BASE_FILE, varname = TARGET_VAR)
  alt_stack <- stack(ALT_FILE, varname = TARGET_VAR)
  
  # Add names to bands
  names(base_stack) <- BAND_YEARS
  names(alt_stack) <- BAND_YEARS
  
  # Extract the dates of interest
  base_target <- subset(base_stack, TARGET_YEARS)
  alt_target <- subset(alt_stack, TARGET_YEARS)
  
  # Mask the subsets to AOI
  AOI <- shapefile(AOI_FILE)
  
  base_mask <- mask(base_target, AOI)
  alt_mask <- mask(alt_target, AOI)
  
  # Do raster math to get difference
  alt_base <- alt_mask-base_mask
  names(alt_base) <- TARGET_YEARS # in case the raster math drops layer names
  
  #Extract alt and base name from files
  base_name <- str_extract_all(BASE_FILE, BASE_ALT_NAMES)[[1]]
  base_name
  alt_name <- str_extract_all(ALT_FILE, BASE_ALT_NAMES)[[1]]
  alt_name
  diff_name <- paste0(alt_name, "-", base_name)
  diff_name
  
  # ----
  # Make histogram of differences for acreage tables
  # ----
  
  acreage_list <- list() 
  for(n in 1:nlayers(alt_base)){
    diff_hist <- hist(alt_base[[n]], breaks = diff_cuts, right = FALSE, plot = FALSE)
    
    if(grepl(paste0(gator_string, "|", apsn_string, "|", waders_string, "|", snki_string, "|", dsd_string), BASE_FILE)){
      # Convert number of cells to number of acres
      # Each cell is 400m x 400 m; 4046.86 sq meters = 1 acres
      diff_acres <- diff_hist$counts * 400 * 400 / 4046.86
    }
    
    #if(grepl("MARL", BASE_FILE)){
      # for marl prairie **marl prairie uses different mesh!
     # diff_acres <- diff_hist$counts * 478.95 * 478.95 / 4046.86
    #}
    
    # Build data frame for export to csv
    diff_bins <- paste0("[", as.character(diff_cuts)[1:(length(diff_cuts) - 1)], " - ", as.character(diff_cuts)[2:length(diff_cuts)], ")")
    acreage_diff_df <- data.frame(`Percent difference from baseline` = rev(diff_bins),
                          `Number of acres` = rev(diff_acres),
                          check.names = FALSE)
    acreage_list[[n]] <- acreage_diff_df
    names(acreage_list)[[n]] <- paste0(diff_name,"_", names(alt_base)[n])
  }
  acreage_df <- bind_rows(acreage_list, .id = "Scenario_year")
  
  # ----
  # Calculate percent diff for barplot
  # ----
  
    if(grepl(paste0(gator_string, "|", waders_string), BASE_FILE)){
      
      if(CROPPED == FALSE){
      # Crop to AOI if not already masked  
      alt_all <- mask(alt_stack, AOI)
      base_all <- mask(base_stack, AOI)
      }
      
      if(CROPPED == TRUE){
      alt_all <- alt_stack
      base_all <- base_stack
      }
      
    # Mean of Every time step since yearly output  
    alt_mean <- cellStats(alt_all, stat = mean, na.rm = TRUE)
    base_mean <- cellStats(base_all, stat = mean, na.rm = TRUE)
    
    #Calculate percent diffrence of the means
    per_diff_mean <- ((alt_mean - base_mean)/base_mean)*100
    
    #Make data frame for bar chart
    per_diff_mean <- data.frame(per_diff_mean)
    rownames(per_diff_mean) <- sub("X", "", rownames(per_diff_mean))
    colnames(per_diff_mean) <- "Percent_Difference"
    per_diff_mean$Scenarios <- diff_name
    per_diff_mean$Year <- row.names(per_diff_mean)
  } 
  
  if(grepl(paste0(apsn_string, "|", snki_string), BASE_FILE)){
    
    if(CROPPED == FALSE){
      #Mask the full raster stack - this will take ~ 15 minutes for each file
      alt_all <- mask(alt_stack, AOI)
      base_all <- mask(base_stack, AOI)
    }
    
    if(CROPPED == TRUE){
      alt_all <- alt_stack
      base_all <- base_stack
    }
    
    #Get indices for years
    base.indices <- format(as.Date(names(base_all), format="X%Y.%m.%d"), format="%Y")
    alt.indices <- format(as.Date(names(alt_all), format="X%Y.%m.%d"), format="%Y")
    #Mean across year
    base_popyr <- stackApply(base_all, base.indices, fun=mean)
    alt_popyr <- stackApply(alt_all, alt.indices, fun=mean)
    #Mean across landscape
    base_mean <- cellStats(base_popyr, mean)
    alt_mean <- cellStats(alt_popyr, mean)
    # Calculate percent diff of means
    per_diff_mean <- ((alt_mean - base_mean)/base_mean)*100
    
    #Make data frame for bar chart
    per_diff_mean <- data.frame(per_diff_mean)
    colnames(per_diff_mean) <- "Percent_Difference"
    years <- as.data.frame(unique(base.indices))
    names(years) <- "Year"
    per_diff_mean <- cbind(years, per_diff_mean)
    per_diff_mean$Scenarios <- diff_name
  } 
  
  # Set dataframe as NULL for dsd. If not, will get error that object does not exist when writing out to list at end of function
  if(grepl(dsd_string, BASE_FILE)){
    per_diff_mean <- NULL
    }
    
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
  ind_df$breaks <- cut(ind_df$value, ind_cuts, right = FALSE, include.lowest = TRUE)
  ind_df$labs   <- cut(ind_df$value, ind_cuts, ind_labs, right = FALSE, include.lowest = TRUE)
  
  diff_df$breaks <- cut(diff_df$value, diff_cuts, right = FALSE, include.lowest = TRUE)
  diff_df$labs <- cut(diff_df$value, diff_cuts, diff_labs, right = FALSE, include.lowest = TRUE)
  
  # set levels so all get plotted
  ind_df$labs <- factor(ind_df$labs, levels = ind_labs)
  diff_df$labs <- factor(diff_df$labs, levels = diff_labs)
  
  ind_df$name <- factor(ind_df$name, levels = TARGET_YEARS)
  diff_df$name <- factor(diff_df$name, levels = TARGET_YEARS)
  
  ind_df$Scenario <- factor(ind_df$Scenario, levels = c(base_name, alt_name, diff_name))
  diff_df$Scenario <- factor(diff_df$Scenario, levels = c(base_name, alt_name, diff_name))
  
  #Name the Target Years to display on figure facet labels
  name.labs <- TARGET_YEARS_LABELS
  names(name.labs) <- TARGET_YEARS
  
  df_list <- list("ind_df" = ind_df, "diff_df" = diff_df, "alt_name" = alt_name, "base_name" = base_name, 
                  "name.labs" = name.labs, "map_title" = output_title, "acreage_df" = acreage_df, "Percent_diff_mean" = per_diff_mean)
  return(df_list)
}

# Make percent diff Bar plot
PER_DIFF_PLOT <- function(DF, X_VAR, Y_VAR, FILL_VAR, TITLE, Y_LAB, X_LAB, MIN_LIMIT, MAX_LIMIT){
  DIFF_PLOT <- ggplot(data = DF, aes_string(x=X_VAR, y=Y_VAR, fill = FILL_VAR))+ 
    geom_bar(stat="identity", position="dodge", width=0.7, colour="black") + 
    labs(y = Y_LAB, x = X_LAB, title = TITLE, fill = "Percent Change \nfrom Baseline:")+
    scale_y_continuous(limits=c(MIN_LIMIT,MAX_LIMIT)) +
    theme(axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          plot.title=element_text(size=20),
          axis.text.x=element_text(size=12, angle=70, hjust=1), 
          axis.text.y=element_text(size=12), 
          legend.text=element_text(size=12),
          legend.title=element_text(size=15), 
          legend.title.align = 0.5,
          plot.margin = margin(1,.5,1,.5, unit = "in")) # Set margins for plot - wider on right side to make space fore legend
  
  return(DIFF_PLOT)
}

PER_DIFF_PLOT_ALTS <- function(DF, X_VAR, Y_VAR, FILL_VAR, TITLE, Y_LAB, X_LAB, MIN_LIMIT, MAX_LIMIT){
  bar_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  DIFF_PLOT <- ggplot(data = DF, aes_string(x=X_VAR, y=Y_VAR, fill = FILL_VAR))+ 
    geom_bar(stat="identity", position = "dodge", width=0.7, colour="black") + 
    scale_fill_manual(values = bar_pal )+
    labs(y = Y_LAB, x = X_LAB, title = TITLE, fill = "Percent Change \nfrom Baseline:")+
    scale_y_continuous(limits=c(MIN_LIMIT,MAX_LIMIT)) +
    theme(axis.title.x=element_text(size=15),
          axis.title.y=element_text(size=15),
          plot.title=element_text(size=20),
          axis.text.x=element_text(size=12, angle=70, hjust=1), 
          axis.text.y=element_text(size=12), 
          legend.text=element_text(size=12),
          legend.title=element_text(size=15), 
          legend.title.align = 0.5,
          plot.margin = margin(1,.5,1,.5, unit = "in")) # Set margins for plot - wider on right side to make space fore legend
  
  return(DIFF_PLOT)
}
