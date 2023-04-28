###############################################################################################################
#This script uses the output from the JEM Small Fish Density model (SA) and performs the following:
# 1.) removes PSUs not within the AOI mask
# 2.) Calculates cumulative fish density, and generates line plot
# 3.) calculate daily percent differnce for every PSU
# 4.) Summarize dily percent differnce to A) YEAR - for Bar plot and B)YEAR & PSU for map
# 5.) Generate percent differnce bar plot (using means of daily percent differnce)
# 6.) Generate maps (using means of daily percent difference - for difference map, and mean TOTFISH for individual score maps)
#
# Caitlin Hackett chackett@usgs.gov
#############################################################################################################

#Load packages
library(tidyverse)
library(sf)
library(raster)
# raster might cause issues with select() in dplyr so be sure to usee dplyr::select() if using the function

source("../restoration_runs/Scripts/smalfish_map_functions.R")

#############################
# 1. SUBSET FISH PSU TO AOI 
#############################
fish_all <- read.csv(fish_path, header = TRUE) # Read in CSV with all fish data
AOI <- st_read(dsn = AOI_PATH) # read in AOI shapefile

# Get Unique PSUs and make spatial
PSU_coords_all <- unique(dplyr::select(fish_all, c("PSU", "EASTING", "NORTHING")))
coordinates(PSU_coords_all) <- ~ EASTING + NORTHING
PSU_coords_all <- st_as_sf(PSU_coords_all)
PSU_coords_all <- PSU_coords_all %>% st_set_crs(st_crs(AOI))

# Subset PSU locations to those within the AOI
PSU_coords_AOI <- PSU_coords_all[AOI,]

# filter fish to only psus within the AOI
PSU_AOI_names <- PSU_coords_AOI$PSU # names of PSUS within AOI
fish <- fish_all[fish_all$PSU %in% PSU_AOI_names,] 
head(fish)

# Add column for year so annual mean can be calcualted for each PSU
fish$DATE <- as.Date(fish$DATE)
fish$YEAR <- format(fish$DATE, format="%Y")
head(fish)

############################
# LOSOM Round 3 ONLY
# - Rename NA22F and NA25F with NA22f and NA25f
############################
colnames(fish) <- sub("NA22F", "NA22f", colnames(fish))
colnames(fish) <- sub("NA25F", "NA25f", colnames(fish))
head(fish)

############################
# 2. CUMULATIVE DENSITY
############################
# Calculate cumulative fish density over the period of record

# Calculate mean daily TOT_FISH_CUM  -- combined PSUs
# I tried to factor the string but was haaving trouble getting dplyr to use the strings in the pipe - this is a point
# of future improvement
cum_fish_by_date <- fish %>%
  group_by(DATE)%>%
  summarise(PA22 = mean(depth_PA22_TOTFISH_CUM),
            PA25 = mean(depth_PA25_TOTFISH_CUM),
            ECB19 = mean(depth_ECB19_TOTFISH_CUM),
            NA22f = mean(depth_NA22f_TOTFISH_CUM),
            NA25f = mean(depth_NA25f_TOTFISH_CUM))
head(cum_fish_by_date)

# Pivot longer for plotting
cum_fish_by_date <- pivot_longer(cum_fish_by_date, cols = c(2:ncol(cum_fish_by_date)), names_to = "Scenario", values_to = "totfish_cum")
head(cum_fish_by_date)

# Set variable orders
cum_fish_by_date$Scenario <- factor(cum_fish_by_date$Scenario, levels = c(alt_names, base_names))
cum_fish_by_date$DATE <- as.Date(cum_fish_by_date$DATE)

# Add breaks so x-axis has date not just year
dbreaks <- as.Date(c("1965-01-01","1970-01-01", "1975-01-01","1980-01-01","1985-01-01","1990-01-01","1995-01-01","2000-01-01","2005-01-01",
                     "2010-01-01", "2016-12-31"))

# Set palette
# palette source:" https://clauswilke.com/dataviz/color-pitfalls.html
#line_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
line_pal <- c("#E69F00", "#000000", "#009E73", "#0072B2", "#F0E442")

## For LOSOM round 3 only!
# - Make column that marks linetype befcause PA22 and NA22f do not show up on
# graph because values are very close to PA25 and NA25f
cum_fish_by_date$line_type <- 0
cum_fish_by_date[cum_fish_by_date$Scenario == "PA22", 4] <- 1
cum_fish_by_date[cum_fish_by_date$Scenario == "NA22f", 4] <- 1
cum_fish_by_date$line_type <- as.factor(cum_fish_by_date$line_type)


# Make the plot
#cum_fish_plot <- ggplot(cum_fish_by_date) + 
#  geom_line(aes(x=DATE, y=totfish_cum, colour=Scenario, linetype =  size = 1), stat="identity")+ 
#  #geom_line(data = filter(cum_fish_by_date, Scenario != "PA22" & Scenario != "NA22F"), aes(x=DATE, y=totfish_cum, colour=Scenario), stat="identity", size = 0.5)+ 
#  scale_color_manual(values = line_pal)+
#  #scale_linetype_manual(values = c(1,2,1,2,1))+
#  #scale_color_viridis_d()+
#  ylab(label="Average Daily Cumulative Density (fish/m2)") + xlab(label="Date") +
#  ggtitle("Cumulative Small Fish Density") +
#  scale_x_date(breaks=dbreaks, limits=as.Date(c("1965-01-01", "2016-12-31"))) +
#  theme(axis.title.x=element_text(size=15), axis.title.y=element_text(size=15), plot.title=element_text(size=20),
#        axis.text.x=element_text(size=12, angle=70, hjust=1), axis.text.y=element_text(size=12), legend.text=element_text(size=12),
#        legend.title=element_text(size=15), plot.margin = margin(1,1,1,1, "cm"))
#cum_fish_plot

cum_fish_plot <- ggplot(cum_fish_by_date, aes(x=DATE, y=totfish_cum))+ 
  geom_line(aes(colour = Scenario:line_type, size = Scenario:line_type))+ 
  #geom_line(data = filter(cum_fish_by_date, Scenario != "PA22" & Scenario != "NA22F"), aes(x=DATE, y=totfish_cum, colour=Scenario), stat="identity", size = 0.5)+ 
  scale_color_manual(name = "Scenario", values = line_pal, labels = c("PA22", "PA25", "ECB19", "NA22f", "NA25f"))+
  scale_size_manual(name = "Scenario", values = c(1.5,0.5, 0.5, 1.5, 0.5), labels = c("PA22", "PA25", "ECB19", "NA22f", "NA25f"))+
  #scale_color_viridis_d()+
  ylab(label="Average Daily Cumulative Density (fish/m2)") + xlab(label="Date") +
  ggtitle("Cumulative Small Fish Density") +
  scale_x_date(breaks=dbreaks, limits=as.Date(c("1965-01-01", "2016-12-31"))) +
  theme(axis.title.x=element_text(size=15), axis.title.y=element_text(size=15), plot.title=element_text(size=20),
        axis.text.x=element_text(size=12, angle=70, hjust=1), axis.text.y=element_text(size=12), legend.text=element_text(size=12),
        legend.title=element_text(size=15), plot.margin = margin(1,1,1,1, "cm"))
cum_fish_plot

#save plot
ggsave(paste0(output_path, "/fish_cumulative.pdf"), width=11, height=8.5, units="in", dpi=300)

# Save data
cum_fish_by_date <- cum_fish_by_date[, -4]
write.table(cum_fish_by_date, file=paste0(output_path, "/fish_cumulative.txt"), sep=",")

##########################################
# 3. Daily percent difference
##########################################

# Subset to "TOTFISH" columns to calculate daily percent differnces
fish_daily <- dplyr::select(fish ,c(DATE, YEAR, PSU, grep("TOTFISH$", names(fish))))

# Calculate daily percent difference for each PSU (all years) _ kind of slow might be a better way with lapply?
per_diff_daily <- data.frame()
for(b in 1:length(base_names)){
  base_col <- grep(base_names[b], names(fish_daily), value = TRUE)
  for(a in 1:length(alt_names)){
    alt_col <- grep(alt_names[a], names(fish_daily), value = TRUE)
    
    b_name <- str_extract_all(base_col, base_names[b])[[1]]
    a_name <- str_extract_all(alt_col, alt_names[a])[[1]]
    diff_name <- paste0(a_name, "-", b_name)
    
    print(paste0("Processing Daily Percent Difference :: ", diff_name))
    
    alt_base <- fish_daily[,c(alt_col, base_col)]
    per_diff <- ((alt_base[alt_col]-alt_base[base_col])/alt_base[base_col])* 100
    names(per_diff) <- "percent_diff"
    per_diff <- cbind(fish_daily[,c("DATE", "YEAR", "PSU")], per_diff)
    per_diff$Scenario <- diff_name
    per_diff_daily <- rbind(per_diff_daily, per_diff)
    }}

###########################################
# 4. Summarize daily percent difference 
###########################################

####
# Summarize to YEAR - BAR PLOT
###
# calculate average daily percent difference for all days and PSUs (Barplot) for each sceanrio
daily_diff_bar <- per_diff_daily%>%
  group_by(YEAR, Scenario)%>%
  summarise(mean_perdiff = mean(percent_diff))
# write percent differnce table to text
write.table(daily_diff_bar, file=paste0(output_path, "/fish_annual_mean_pchange.txt"), sep=",")

####
# Summarise to YEAR & PSU - MAPS
####
# calculate average daily percent difference for each PSU for each year
# Caclulate mean of daily percent change - to year, PSU, Scenario
daily_diff_map <- per_diff_daily%>%
  group_by(YEAR, PSU, Scenario)%>%
  summarise(mean_daily_diff = mean(percent_diff))

###########################################
# 5. Make Bar plots
###########################################

## FUNCTION FOR BAR PLOTS
# Make percent diff Bar plot - 1 alt and baselines
PER_DIFF_PLOT <- function(DF, X_VAR, Y_VAR, FILL_VAR, TITLE, Y_LAB, X_LAB, MIN_LIMIT, MAX_LIMIT){
  DIFF_PLOT <- ggplot(data = DF, aes_string(x=X_VAR, y=Y_VAR, fill = FILL_VAR))+ 
    geom_bar(stat="identity", position="dodge", width=0.7, colour="black") + 
    labs(y = Y_LAB, x = X_LAB, title = TITLE, fill = "percent change \nfrom baseline:")+
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

#plot for all alts with 1 baseline
PER_DIFF_PLOT_ALTS <- function(DF, X_VAR, Y_VAR, FILL_VAR, TITLE, Y_LAB, X_LAB, MIN_LIMIT, MAX_LIMIT){
  bar_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00")
  DIFF_PLOT <- ggplot(data = DF, aes_string(x=X_VAR, y=Y_VAR, fill = FILL_VAR))+ 
    geom_bar(stat="identity", position = "dodge", width=0.7, colour="black") + 
    scale_fill_manual(values = bar_pal )+
    labs(y = Y_LAB, x = X_LAB, title = TITLE, fill = "percent change \nfrom baseline:")+
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
#######

# Make Percent diffrence bar plot
x_var <- "YEAR"
y_var <- "mean_perdiff"
fill_var <- "Scenario"
title <- "Total Fish Density"
#y_lab <- paste0("Percent Change in ", title, "\nfrom baseline to ", map_dfs$alt_name)
x_lab <- "Year"
min_limit <-plyr::round_any(min(daily_diff_bar[y_var]), 5, f = floor)
max_limit <- plyr::round_any(max(daily_diff_bar[y_var]), 5, f = ceiling)

# Make bar plot for alt vs both baselines
#a <- 1
for(a in 1:length(alt_names)){
  print(paste0("Making Differnce Bar Plot :: ", alt_names[a]))
  per_diff_alt <- daily_diff_bar[grep(alt_names[a], daily_diff_bar$Scenario),]
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
  diff_plot_filename <- paste0(output_path, "/PercentDiff_BarPlot_", gsub(" ", "_", title),"_", alt_names[a], ".pdf")
  ggsave(diff_plot_filename, TEST, width=11, height=8.5, units="in", dpi=300)
}

# Make bar plot for all alts against each baseline
#a <- 1
for(b in 1:length(base_names)){
  print(paste0("Making Differnce Bar Plot :: ", base_names[b]))
  per_diff_alt <- daily_diff_bar[grep(base_names[b], daily_diff_bar$Scenario),]
  TEST <- PER_DIFF_PLOT_ALTS(
    DF = per_diff_alt,
    X_VAR = x_var,
    Y_VAR = y_var,
    FILL_VAR = fill_var,
    TITLE = title,
    Y_LAB = paste0("Percent Change in ", title, "\nfrom Baseline ", base_names[b]),
    X_LAB = x_lab,
    MIN_LIMIT = min_limit,
    MAX_LIMIT = max_limit
  )
  diff_plot_filename <- paste0(output_path, "/PercentDiff_BarPlot_", gsub(" ", "_", title),"_", base_names[b], ".png")
  ggsave(diff_plot_filename, TEST, width=15, height=8.5, units="in", dpi=300, scale = 1)
}

#########################################
# 6. Make Maps
#########################################
TARGET_YEARS <- c(1978, 1989, 1995)
TARGET_YEAR_LABELS <- c("1978 - Average Year", "1989 - Dry Year", "1995 - Wet Year")

# Calculates mean TOTFISH by year for each PSU - for individual score plotting on map
fish_psu_year <- fish%>%
  group_by(YEAR, PSU)%>%
  summarise(PA22 = mean(depth_PA22_TOTFISH),
            PA25 = mean(depth_PA25_TOTFISH),
            ECB19 = mean(depth_ECB19_TOTFISH),
            NA22f = mean(depth_NA22f_TOTFISH),
            NA25f = mean(depth_NA25f_TOTFISH))
head(fish_psu_year)

# needs to pivot long for plotting
ind_fish_plot <- pivot_longer(fish_psu_year, cols = c(3:ncol(fish_psu_year)), values_to = "annual_mean", names_to = "Scenario")

# add PSU coordinates to individual and diff data
PSU_coords <- unique(dplyr::select(fish, c("PSU", "EASTING", "NORTHING")))
ind_fish_plot <- left_join(ind_fish_plot, PSU_coords, by = "PSU")      
diff_fish_plot <- left_join(daily_diff_map, PSU_coords, by = "PSU")

# Subset to correct plotting years (1978, 1989, 1995) - diff is already subset to correct years so only needs to be applied to individual plot data
ind_fish_plot <- filter(ind_fish_plot, YEAR == 1978 | YEAR == 1989 | YEAR == 1995)
diff_fish_plot <- filter(diff_fish_plot, YEAR == 1978 | YEAR == 1989 | YEAR == 1995)

# Round value prior to bin - bins were dropping values if not rounded first
ind_fish_plot$annual_mean_round <- round(ind_fish_plot$annual_mean, 2)
diff_fish_plot$mean_daily_diff_round <- round(diff_fish_plot$mean_daily_diff, 2)

# Create Breaks for plotting
fish_ind_cuts <- c(0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0, 14.0, 16.0, 17.19)
fish_ind_labels <- c("0.0 - 2.0", "2.1 - 4.0", "4.1 - 6.0", "6.1 - 8.0", "8.1 - 10.0", "10.1 - 12.0", "12.1 - 14.0", "14.1 - 16.0", "16.1 - 17.19")

fish_diff_cuts <- c(-Inf, -77.5, -55.1, -32.6, -10, 10.1, 32.7, 55.2, 77.6, Inf)
fish_diff_labels <- c("-77.6 to -100+", "-55.2 to -77.5", "-32.7 to -55.1", "-10.1 to -32.6", "-10.0 to 10.0", "10.1 to 32.6",
                      "32.7 to 55.1", "55.2 to 77.5", "77.6 to 100+")

# Add breaks and labels to the dataframes - for plotting maps
ind_fish_plot$breaks <- cut(ind_fish_plot$annual_mean_round, fish_ind_cuts, right = TRUE, include.lowest = TRUE)
ind_fish_plot$labs   <- cut(ind_fish_plot$annual_mean_round, fish_ind_cuts, fish_ind_labels, right = TRUE, include.lowest = TRUE)
ind_fish_plot$labs <- factor(ind_fish_plot$labs, levels = fish_ind_labels, ordered = TRUE) # order the bins so bin can correspond to point size on map

diff_fish_plot$breaks <- cut(diff_fish_plot$mean_daily_diff_round, fish_diff_cuts, right = FALSE, include.lowest = TRUE)
diff_fish_plot$labs <- cut(diff_fish_plot$mean_daily_diff_round, fish_diff_cuts, fish_diff_labels, right = FALSE, include.lowest = TRUE)

# set levels so all get plotted
ind_fish_plot$labs <- factor(ind_fish_plot$labs, levels = fish_ind_labels)
diff_fish_plot$labs <- factor(diff_fish_plot$labs, levels = fish_diff_labels)

ind_fish_plot$YEAR <- factor(ind_fish_plot$YEAR, levels = c(1978, 1989, 1995))
diff_fish_plot$YEAR <- factor(diff_fish_plot$YEAR, levels = c(1978, 1989, 1995))

#Name the Target Years to display on figure facet labels
name.labs <- TARGET_YEAR_LABELS
names(name.labs) <- TARGET_YEARS
name.labs

# Make maps
map_data_list <- list()
index <- 0
for(b in 1:length(base_names)){
  base_scenario <- base_names[b] # get base name
  for(a in 1:length(alt_names)){
    alt_scenario <- alt_names[a] # get alt name
    
    diff_scenario <- paste0(alt_scenario, "-", base_scenario) # make diff name
    
    print(paste0("Pulling Data to Map :: ", diff_scenario))
    
    # Subset for plotting
    ind_plot <- filter(ind_fish_plot, (Scenario == alt_scenario | Scenario == base_scenario)) 
    diff_plot <- filter(diff_fish_plot, (Scenario == diff_scenario))
    
    # Set levels
    ind_plot$Scenario <- factor(ind_plot$Scenario, levels = c(base_scenario, alt_scenario, diff_scenario))
    diff_plot$Scenario <- factor(diff_plot$Scenario, levels = c(base_scenario, alt_scenario, diff_scenario))
    
    # Make map
    print(paste0("Making Map :: ", diff_scenario))
    
    FISH_MAP(IND_FILL = "labs",
             DIF_FILL = "labs",
             SCENARIO_COL = "Scenario",
             YEAR_COL = "YEAR",
             AOI_PATH = AOI_PATH,
             MPR_PATH = MPR_PATH,
             WCAS_PATH = WCAS_PATH,
             FL_PATH = FL_PATH,
             MAP_EXTENT = MAP_EXTENT,
             MAP_TITLE = "Mean Total Fish Density",
             DF_IND = ind_plot,
             DF_DIF = diff_plot,
             OUTPUT_FILE_NAME = paste0(output_path, "/fish_map_", alt_scenario, "_", base_scenario, ".pdf"))
    
    #Save data used to plot map to list to reproduce if needed
    plot_list <- list("alt_scenario" = alt_scenario, "base_scenario" = base_scenario, "diff_scenario" = diff_scenario, "ind_plot" = ind_plot, "diff_plot" = diff_plot)
    index <- index +1
    map_data_list[[index]] <- plot_list
    names(map_data_list)[[index]] <- diff_scenario
  }
}

save(map_data_list, file = paste0(output_path, "/fish_processed_data.RData"))
