#############################################
# Function to process MArl Prairie Output
#
# Caitlin Hackett chackett@usgs.gov
#############################################

library(tidyverse)
library(sf)
library(raster)

MarlProcess <- function(base_file,
                        alt_file,
                        mp_file,
                        subpop_file){
  
  ## Define Strings for Function ----
  rsm_string <- "RSM"
  hsi_string <- "hsi_CombinedScore"
  base_string <- "BASE"
  alt_string <- "ALT"
  
  
  # Marl
  marl_string <- "Marl"
  
  marl_ind_cuts <- seq(from = 0, to = 100, by = 10)
  marl_ind_labels <- c("0 - 10%", "11 - 20%", "21 - 30%",
                       "31 - 40%", "41 - 50%", "51 - 60%",
                       "61 - 70%", "71 - 80%", "81 - 90%", "91 - 100%")
  
  marl_diff_cuts <- c(-100, -78, -55, -33, -10, 11, 34, 56, 79, 100)
  marl_diff_labels <- c("-79 to -100", "-56 to -78", "-34 to -55",
                        "-11 to -33", "10 to -10", "11 to 33",
                        "34 to 55", "56 to 78", "79 to 100") 
  marl_title <- "Marl Prairie Score: Percent to Target"
  
  # Get alt, base, and difference names
  base_name <- str_extract_all(base_file, all_scenario_names)[[1]]
  base_name
  alt_name <- str_extract_all(alt_file, all_scenario_names)[[1]]
  alt_name
  diff_name <- paste0(alt_name, "-", base_name)
  diff_name
  
  # Read CSV
  base_csv <- read.csv(base_file, header = TRUE, stringsAsFactors = FALSE)
  base_csv <- base_csv[, c(rsm_string, hsi_string)]
  names(base_csv) <- c(rsm_string, base_string)
  
  alt_csv <- read.csv(alt_file, header = TRUE, stringsAsFactors = FALSE)
  alt_csv <- alt_csv[, c(rsm_string, hsi_string)]
  names(alt_csv) <- c(rsm_string, alt_string)
  
  # Calculate differences and make data frame of differences for plotting
  alt_base <- full_join(base_csv, alt_csv, by = rsm_string)
  alt_base$RSM <- as.numeric(alt_base$RSM)
  alt_base[diff_name] <- alt_base$ALT - alt_base$BASE
  names(alt_base)[names(alt_base) == alt_string] <- paste0(alt_name)
  names(alt_base)[names(alt_base) == base_string] <- paste0(base_name)
  alt_base_long <- pivot_longer(alt_base, cols = c(2:4),
                                names_to = "Scenario",
                                values_to = hsi_string)
  
  # Load shapefile
  mp <- st_read(mp_file) %>%
    st_transform(crs = 26917)
  mp$RSM <- as.numeric(mp$ID)
  mp <- mp[, !names(mp) %in% c("ID", "hsi_MaxDry", "hsi_Hydrop", 
                               "hsi_WDepth", "hsi_DDepth",
                               "hsi_CScore", "RSM_ID")]
  
  # Merge shapefile and diff_data to save out if needed
  mp_diff <- merge(mp, alt_base, by = rsm_string)
  #st_write(mp_diff, "./Output/SHAPEFILE.shp")
  
  # Merge shapefile and long data for plotting -
  # pivot_longer does not work on sf object so must pivot prior to merging
  mp_diff_plot <- merge(mp, alt_base_long)
  
  # set factor levels for plotting
  mp_diff_plot$Scenario <- factor(mp_diff_plot$Scenario,
                                  levels = c(base_name, alt_name, diff_name))
  
  # split df to make easier plotting
  ind_df <- filter(mp_diff_plot, Scenario != diff_name)
  diff_df <- filter(mp_diff_plot, Scenario == diff_name)
  
  # Add breaks and lebels to the dataframes
  ind_df$breaks <- cut(ind_df$hsi_CombinedScore, marl_ind_cuts,
                       right = TRUE, include.lowest = TRUE)
  ind_df$labs   <- cut(ind_df$hsi_CombinedScore, marl_ind_cuts,
                       marl_ind_labels, right = TRUE, include.lowest = TRUE)
  
  diff_df$breaks <- cut(diff_df$hsi_CombinedScore, marl_diff_cuts,
                        right = FALSE, include.lowest = TRUE)
  diff_df$labs <- cut(diff_df$hsi_CombinedScore, marl_diff_cuts,
                      marl_diff_labels, right = FALSE, include.lowest = TRUE)
  
  
  # Make histogram of differences for acreage tables
  sub_pop <- st_read(subpop_file) %>%
    st_transform(crs = 26917)
  
  ext <- extent(diff_df)
  r <- raster(ext, res = 478.95) 
  crs(r) <-  crs(diff_df)
  
  diff_sp <- diff_df[, names(diff_df) %in% c(hsi_string)]
  
  rr <- rasterize(diff_sp, r, field = hsi_string, na.rm = TRUE)
  
  diff_mask <- mask(rr, sub_pop)
  diff_mask <- crop(diff_mask, sub_pop)
  
  diff_hist <- hist(diff_mask, breaks = marl_diff_cuts,
                    right = FALSE, plot = FALSE)
  diff_acres <- diff_hist$counts * 478.95 * 478.95 / 4046.86
  
  # Build data frame for export to csv
  diff_bins <- paste0("[",
                      as.character(marl_diff_cuts)[1:(length(marl_diff_cuts) - 1)],
                      " - ",
                      as.character(marl_diff_cuts)[2:length(marl_diff_cuts)],
                      ")")
  acreage_diff_df <- data.frame(`Difference` = rev(diff_bins),
                                `Number of acres` = rev(diff_acres),
                                check.names = FALSE)
  acreage_diff_df$Scenario <- diff_name
  
  return(list("ind_df" = ind_df, "diff_df" = diff_df, "full_df" = mp_diff,
              "diff_name" = diff_name, "alt_name" = alt_name,
              "base_name" = base_name, "acreage_df" = acreage_diff_df,
              "Map_title" = marl_title, "sp_string" = marl_string))
}      
