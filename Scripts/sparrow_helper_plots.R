# ------------------------------------------------------------------------------
# Generate Sparrow Helper Plots
# Create Annual summary of 4-yr hydroperiod data
# ------------------------------------------------------------------------------

# Load packages
library(tidyverse)

# Vector of all scenario names. scenario names set in workflow_inputs.R
scenario_names <- c(alt_names, base_names)
scenario_names

# Make vector with "|" between each scenario 
all_scenario_names <- paste0(scenario_names, collapse = "|")
all_scenario_names

# List Files
all_files <- list.files(path = parent_path,
                        pattern = "hydroperiod_cell_percent_\\(90.0-210.0).csv",
                        recursive = TRUE, full.names = TRUE)
all_files

# Read in Target data and combine to one dataframe
hydro_percent <- data.frame()
for (f in 1:length(all_files)) {
  
  # Extract scenario name from file
  name <- str_extract_all(all_files[f], all_scenario_names)[[1]]

  # Read csv
  hypd_per <- read.csv(all_files[[f]])
  
  # Add column with scenario name
  hypd_per$Scenario <- name
  
  # Bind rows together to generate one dataframe with all data
  hydro_percent <- bind_rows(hydro_percent, hypd_per)
}

# Make data long for easier plotting
hydro_percent <- pivot_longer(hydro_percent, cols = c(2:7))


# rename and relevel subpopulation names
hydro_percent$Scenario <- factor(hydro_percent$Scenario,
                                 levels = c(alt_names, base_names))
hydro_percent$name <- factor(hydro_percent$name,
                             levels = c("A", "B", "C", "D", "E", "F"))
pops <- levels(hydro_percent$name)

# set palette for bar plots
bar_pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
             "#0072B2", "#D55E00", "#CC79A7", "#000000")

# Generate barplot for each subpopulation
for (p in 1:length(pops)) {
  plot_data <- hydro_percent[hydro_percent$name == pops[p],]
  
  scale <- 3

csss_plot <- ggplot(data = plot_data,
                    aes(x = as.factor(Date), y = value, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge",
           width = 0.7, colour = "black") +
  scale_fill_manual(values = bar_pal ) +
  ylab("Proportion of Area Meeting Conditions") + xlab("Year") +
  ggtitle(paste0("Cape Sable Seaside Sparrow - Subpopulation ", pops[p])) +
  scale_y_continuous(limits = c(0,1)) +
  geom_hline(yintercept = 0.4) +
  theme(axis.title.x = element_text(size = scale*15),
        axis.title.y = element_text(size = scale*15),
        plot.title = element_text(size = scale*20),
        axis.text.x = element_text(size = scale*12, angle = 70, hjust = 1),
        axis.text.y = element_text(size = scale*12),
        legend.text = element_text(size = scale*12),
        legend.title = element_text(size = scale*15),
        legend.key.width = unit(scale*4, "mm"),
        plot.margin = margin(2,2,2,2, unit = "cm"))

filename <- paste0(output_path, "CSSS_hydroperiod_90_210_", pops[p], ".png")
filename

# Save plot
ggsave(filename, csss_plot, width = 15, height = 8.5,
       units = "in", dpi = 300, scale = 3)

}

# Save data that generated the barplot
names(hydro_percent) <- c("Year", "Scenario", "Subpopulation", "Cell_Percent")

write.csv(hydro_percent, file = paste0(OUTPUT_PATH,
                                       "Sparrow_Helper_Hydroperiod.csv"),
          row.names = FALSE)


