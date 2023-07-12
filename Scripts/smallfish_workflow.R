# ------------------------------------------------------------------------------
# This script uses the output from the JEM Small Fish Density model (SA)
# and performs the following:
# 1.) remove PSUs not within the AOI mask
# 2.) Calculate cumulative fish density, and generates line plot
# 3.) calculate daily percent difference for every PSU
# 4.) Summarize daily percent difference to:
#    A) YEAR - for Bar plot
#    B) YEAR & PSU for map
# 5.) Generate percent difference bar plot
#     (using means of daily percent difference)
# 6.) Generate maps (using means of daily percent difference
#     for difference map, and mean TOTFISH for individual score maps)
# ------------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Running Workflow"))

# Load packages
library(tidyverse)
library(sf)
library(raster)

# Source dependency scripts
print(paste0("INFO [", Sys.time(), "] Sourcing Dependency Scripts"))

print(paste0("INFO [", Sys.time(), "] Sourcing map functions"))
source("./Scripts/smallfish_map_functions.R")

print(paste0("INFO [", Sys.time(), "] Sourcing barplot functions"))
source("./Scripts/smallfish_barplot_functions.R")

print(paste0("INFO [", Sys.time(), "] Sourcing process definitions"))
source("./Scripts/process_definitions.R")

# Defined file paths for shapefiles
print(paste0("INFO [", Sys.time(), "] Sourcing shapefile input paths"))
source("./Scripts/shapefile_paths.R")

## -----------------------------------------------------------------------------
## 1. SUBSET FISH PSU TO AOI
## -----------------------------------------------------------------------------
# Read in CSV with all fish data - large file so may take a few minutes
print(paste0("INFO [", Sys.time(), "] Reading fish csv. This takes several minutes."))
fish_all <- read.csv(fish_path, header = TRUE) 
# read in AOI shapefile
aoi <- st_read(dsn = aoi_path) 

# Get Unique PSUs and make spatial
psu_coords_all <- unique(dplyr::select(fish_all,
                                       c("PSU", "EASTING", "NORTHING")))
coordinates(psu_coords_all) <- ~ EASTING + NORTHING
psu_coords_all <- st_as_sf(psu_coords_all)
psu_coords_all <- psu_coords_all %>% st_set_crs(st_crs(aoi))

# Subset PSU locations to those within the AOI
print(paste0("INFO [", Sys.time(), "] Subsetting PSUs to AOI"))
psu_coords_aoi <- psu_coords_all[aoi, ]

# filter fish to only psus within the AOI
psu_aoi_names <- psu_coords_aoi$PSU # names of PSUS within AOI
fish <- fish_all[fish_all$PSU %in% psu_aoi_names, ] 
head(fish)

# Add column for year so annual mean can be calcualted for each PSU
fish$DATE <- as.Date(fish$DATE)
fish$YEAR <- format(fish$DATE, format = "%Y")
head(fish)

## -----------------------------------------------------------------------------
## 2. CUMULATIVE DENSITY
##    Calculate cumulative fish density over the period of record
## -----------------------------------------------------------------------------
# Calculate mean daily TOT_FISH_CUM  -- combined PSUs
# Name scenarios and columns to sumamrise base on alternative and base names
scenario_names <- c(alt_names, base_names)
scenario_cols <- paste0("depth_", scenario_names, "_TOTFISH_CUM")
scenario_cols

# Group data by date, and calculate daily mean for combined PSUs using colummns
# defined in the scenario_cols vector
print(paste0("INFO [", Sys.time(), "] Calculating mean daily TOT_FISH_CUM"))
cum_fish_by_date <- fish %>%
  group_by(DATE) %>%
  summarise_at(vars(all_of(scenario_cols)),
               list(mean = mean))
head(cum_fish_by_date)
names(cum_fish_by_date) <- c("DATE", scenario_names)

# Pivot longer for plotting
cum_fish_by_date <- pivot_longer(cum_fish_by_date,
                                 cols = c(2:ncol(cum_fish_by_date)),
                                 names_to = "Scenario",
                                 values_to = "totfish_cum")
head(cum_fish_by_date)

# Set variable orders
cum_fish_by_date$Scenario <- factor(cum_fish_by_date$Scenario,
                                    levels = c(alt_names, base_names))
cum_fish_by_date$DATE <- as.Date(cum_fish_by_date$DATE)

# Set breaks for dates that will be displayed on x-axis of plot
# create vector of all years that have output
fishyears <- unique(fish$YEAR)
fishyears

# Create breaks for every other year
yearbreaks <- seq(fishyears[1], fishyears[length(fishyears)], 5)
yearbreaks

# Add another 5 year interval so x axis will plot properly
yearbreaks <- c(yearbreaks, (last(yearbreaks) + 5))
yearbreaks

# Make the break intervals dates
dbreaks <- as.Date(paste0(yearbreaks, "-01-01"))
dbreaks

# Set palette
# palette source:" https://clauswilke.com/dataviz/color-pitfalls.html

# Number of colors needed
ncolor <- length(scenario_cols)
ncolor

full_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
              "#D55E00", "#CC79A7", "#000000")
line_pal <- full_pal[1:ncolor]

# Set min and max dates for x axis
min_date <- min(cum_fish_by_date$DATE)
max_date <- max(cum_fish_by_date$DATE)

#-----------------
# Make plot
print(paste0("INFO [", Sys.time(), "] Making fish line graph"))
cum_fish_plot <- ggplot(cum_fish_by_date) + 
  
  # Set the line geometry
  geom_line(aes(x = DATE, y = totfish_cum, color = Scenario,
                linetype = Scenario), linewidth = 1) +

  # Set line colors
  scale_color_manual(name = "Scenario", values = line_pal,
                     labels = c(scenario_names)) +
  scale_x_date(breaks = dbreaks,
               limits = c(min_date, max_date),
               labels = yearbreaks
               ) +
  
  # Set x and y axis labels and title
  ylab(label = expression("Average Daily Cumulative Density (fish/m"^2*")")) +
  xlab(label = "Date") +
  ggtitle("Cumulative Small Fish Density") +

  # Set theme elements
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.title = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 70, hjust = 1),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        plot.margin = margin(1, 1, 1, 1, "cm"))
cum_fish_plot

#save plot
ggsave(paste0(output_path, "/fish_cumulative.pdf"), width = 11,
       height = 8.5, units = "in", dpi = 300)

# Save data
write.table(cum_fish_by_date,
            file = paste0(output_path, "/fish_cumulative.txt"), sep = ",")


## -----------------------------------------------------------------------------
## 3. Daily percent difference
## -----------------------------------------------------------------------------

# Subset to "TOTFISH" columns to calculate daily percent differnces
fish_daily <- dplyr::select(fish, c(DATE, YEAR, PSU,
                                     grep("TOTFISH$", names(fish))))

# Calculate daily percent difference for each PSU (all years)
# - takes a few minutes to process
print(paste0("INFO [", Sys.time(), "] Calculating daily percent difference fore each PSU (all years)"))
print("This may take several minutes")
per_diff_daily <- data.frame()

for (b in 1:seq_along(base_names)) {
  base_col <- grep(base_names[b], names(fish_daily), value = TRUE)
  for (a in 1:seq_along(alt_names)) {
    alt_col <- grep(alt_names[a], names(fish_daily), value = TRUE)
    
    b_name <- str_extract_all(base_col, base_names[b])[[1]]
    a_name <- str_extract_all(alt_col, alt_names[a])[[1]]
    diff_name <- paste0(a_name, "-", b_name)
    
    print(paste0("Processing Daily Percent Difference :: ", diff_name))
    
    alt_base <- fish_daily[, c(alt_col, base_col)]
    per_diff <- ((alt_base[alt_col] - alt_base[base_col]) / alt_base[base_col]) * 100
    names(per_diff) <- "percent_diff"
    per_diff <- cbind(fish_daily[, c("DATE", "YEAR", "PSU")], per_diff)
    per_diff$Scenario <- diff_name
    per_diff_daily <- rbind(per_diff_daily, per_diff)
  }
}

## -----------------------------------------------------------------------------
## 4. Summarize daily percent difference 
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Summarizing daily percent differences"))

#-----------------
# Summarize to YEAR - BAR PLOT

# calculate average daily percent difference for all days and PSUs (Barplot)
# for each sceanrio
daily_diff_bar <- per_diff_daily %>%
  group_by(YEAR, Scenario) %>%
  summarise(mean_perdiff = mean(percent_diff))
# write percent differnce table to text
write.table(daily_diff_bar,
            file = paste0(output_path, "/fish_annual_mean_pchange.txt"),
            sep = ",")

#-----------------
# Summarise to YEAR & PSU - MAPS

# calculate average daily percent difference for each PSU for each year
# Caclulate mean of daily percent change - to year, PSU, Scenario
daily_diff_map <- per_diff_daily %>%
  group_by(YEAR, PSU, Scenario) %>%
  summarise(mean_daily_diff = mean(percent_diff))

## -----------------------------------------------------------------------------
## 5. Make Bar plots
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Making Barplots"))

# Make Percent difference bar plot
x_var <- "YEAR"
y_var <- "mean_perdiff"
fill_var <- "Scenario"
title <- "Total Fish Density"
x_lab <- "Year"
min_limit <- plyr::round_any(min(daily_diff_bar[y_var]), 5, f = floor)
max_limit <- plyr::round_any(max(daily_diff_bar[y_var]), 5, f = ceiling)

# Set levels of daily_diff_bar to match alt_names and base_names order that
# is set in workflow_inputs.R
# Create vector of levels
diff_levels <- apply(expand.grid(alt_names, base_names),
                     1, paste, collapse = "-")
# Set factor levels
daily_diff_bar$Scenario <- factor(daily_diff_bar$Scenario,
                                 levels = c(diff_levels))

# Make bar plot for alt vs both baselines
for (a in 1:seq_along(alt_names)) {
  print(paste0("Making Differnce Bar Plot :: ", alt_names[a]))
  per_diff_alt <- daily_diff_bar[grep(alt_names[a], daily_diff_bar$Scenario), ]
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
  diff_plot_filename <- paste0(output_path, "/PercentDiff_BarPlot_",
                               gsub(" ", "_", title), "_", alt_names[a], ".pdf")
  ggsave(diff_plot_filename, diff_plot, width = 11,
         height = 8.5, units = "in", dpi = 300)
}

# Make bar plot for all alts against each baseline
for (b in 1:seq_along(base_names)) {
  print(paste0("Making Differnce Bar Plot :: ", base_names[b]))
  per_diff_alt <- daily_diff_bar[grep(base_names[b], daily_diff_bar$Scenario), ]
  diff_plot_alt <- PerDiffPlotAlts(
    df = per_diff_alt,
    x_var = x_var,
    y_var = y_var,
    fill_var = fill_var,
    title = title,
    y_lab = paste0("Percent Change in ", title, "\nfrom Baseline ", base_names[b]),
    x_lab = x_lab,
    min_limit = min_limit,
    max_limit = max_limit
  )
  diff_plot_filename <- paste0(output_path, "/PercentDiff_BarPlot_",
                               gsub(" ", "_", title),"_", base_names[b], ".png")
  ggsave(diff_plot_filename, diff_plot_alt, width = 15,
         height = 8.5, units = "in", dpi = 300, scale = 1)
} 

## -----------------------------------------------------------------------------
## 6. Make Maps
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Making Maps"))

# Make vector of desired column names
scenario_cols_psu <- paste0("depth_", scenario_names, "_TOTFISH")
scenario_cols_psu

# Calculate mean TOTFISH by year for each PSU - for individual score plotting on map
fish_psu_year <- fish %>%
  group_by(YEAR, PSU) %>%
  summarise_at(vars(all_of(scenario_cols_psu)),
               list(mean = mean))
head(fish_psu_year)
names(fish_psu_year) <- c("YEAR", "PSU", scenario_names)
head(fish_psu_year)

# needs to pivot long for plotting
ind_fish_plot <- pivot_longer(fish_psu_year, cols = c(3:ncol(fish_psu_year)),
                              values_to = "annual_mean", names_to = "Scenario")

# add PSU coordinates to individual and diff data
PSU_coords <- unique(dplyr::select(fish, c("PSU", "EASTING", "NORTHING")))
ind_fish_plot <- left_join(ind_fish_plot, PSU_coords, by = "PSU")      
diff_fish_plot <- left_join(daily_diff_map, PSU_coords, by = "PSU")

# Subset to correct plotting years 
ind_fish_plot <- filter(ind_fish_plot, YEAR %in% fish_years)
diff_fish_plot <- filter(diff_fish_plot, YEAR %in% fish_years)

# Round value prior to binning - binning drops values if not rounded first
ind_fish_plot$annual_mean_round <- round(ind_fish_plot$annual_mean, 2)
diff_fish_plot$mean_daily_diff_round <- round(diff_fish_plot$mean_daily_diff, 2)

# Add breaks and labels to the dataframes - for plotting maps
# Break and labels for individual fish scores
ind_fish_plot$breaks <- cut(ind_fish_plot$annual_mean_round,
                            fish_ind_cuts, right = TRUE, include.lowest = TRUE)
ind_fish_plot$labs   <- cut(ind_fish_plot$annual_mean_round,
                            fish_ind_cuts, fish_ind_labels, right = TRUE,
                            include.lowest = TRUE)
# order the bins so bin can correspond to point size on map
ind_fish_plot$labs <- factor(ind_fish_plot$labs, levels = fish_ind_labels,
                             ordered = TRUE) 

# Break and labels for difference in fish scores
diff_fish_plot$breaks <- cut(diff_fish_plot$mean_daily_diff_round,
                             fish_diff_cuts, right = FALSE,
                             include.lowest = TRUE)
diff_fish_plot$labs <- cut(diff_fish_plot$mean_daily_diff_round,
                           fish_diff_cuts, fish_diff_labels, right = FALSE,
                           include.lowest = TRUE)

# Set levels so all get plotted
ind_fish_plot$labs <- factor(ind_fish_plot$labs, levels = fish_ind_labels)
diff_fish_plot$labs <- factor(diff_fish_plot$labs, levels = fish_diff_labels)

# years
ind_fish_plot$YEAR <- factor(ind_fish_plot$YEAR, levels = fish_years)
diff_fish_plot$YEAR <- factor(diff_fish_plot$YEAR, levels = fish_years)

#Name the Target Years to display on figure facet labels
name.labs <- fish_year_labels
names(name.labs) <- fish_years
name.labs

# Make Maps
map_data_list <- list()
index <- 0
for (b in 1:seq_along(base_names)) {
  base_scenario <- base_names[b] # get base name
  for (a in 1:seq_along(alt_names)) {
    alt_scenario <- alt_names[a] # get alt name
    
    diff_scenario <- paste0(alt_scenario, "-", base_scenario) # make diff name
    
    print(paste0("Pulling Data to Map :: ", diff_scenario))
    
    # Subset for plotting
    ind_plot <- filter(ind_fish_plot,
                       (Scenario == alt_scenario | Scenario == base_scenario)) 
    diff_plot <- filter(diff_fish_plot, (Scenario == diff_scenario))
    
    # Set levels
    ind_plot$Scenario <- factor(ind_plot$Scenario,
                                levels = c(base_scenario,
                                           alt_scenario, diff_scenario))
    diff_plot$Scenario <- factor(diff_plot$Scenario, 
                                 levels = c(base_scenario,
                                           alt_scenario, diff_scenario))
    
    # Make map
    print(paste0("Making Map :: ", diff_scenario))
    
    FishMap(ind_fill = "labs",
             dif_fill = "labs",
             scenario_col = "Scenario",
             year_col = "YEAR",
             aoi_path = aoi_path,
             mpr_path = mpr_path,
             wcas_path = wcas_path,
             landscape = landscape,
             map_title = "Mean Total Fish Density",
             df_ind = ind_plot,
             df_dif = diff_plot,
             output_file_name = paste0(output_path, "/fish_map_",
                                       alt_scenario, "_", base_scenario, ".pdf"))
    
    #Save data used to plot map to list to reproduce if needed
    plot_list <- list("alt_scenario" = alt_scenario,
                      "base_scenario" = base_scenario,
                      "diff_scenario" = diff_scenario,
                      "ind_plot" = ind_plot,
                      "diff_plot" = diff_plot)
    index <- index + 1
    map_data_list[[index]] <- plot_list
    names(map_data_list)[[index]] <- diff_scenario
  }
}

print(paste0("INFO [", Sys.time(), "] Saving processed fish data"))
save(map_data_list, file = paste0(output_path, "/fish_processed_data.RData"))
