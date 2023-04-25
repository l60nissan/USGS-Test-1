## -----------------------------------------------------------------------------
# Function to generate marl prairie maps
#
# Caitlin Hackett chackett@usgs.gov
## -----------------------------------------------------------------------------

#Load Packages
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggnewscale)
library(sf)
library(ggsn)
library(shadowtext)

# Define map extent - use set extent instead of extent used for full aoi
# since marl extent is always the same
map_extent <- c(xmin = 485489.89,
                xmax = 554055.08,
                ymin = 2791119.88,
                ymax = 2850143.30) 

## -----------------------------------------------------------------------------
# Function to build marl prairie map 
## -----------------------------------------------------------------------------

MarlMap <- function(
    df_ind,       # Dataframe of individual score output with coordinate column names "x", and "y", 
    ind_fill,     # column name of individual score discrete values to be plotted (LOSOM is "labs")
    df_dif,       # Dataframe of difference output with coordinate column names "x", and "y"
    dif_fill,     # column name of discrete values to be plotted (LOSOM is "labs")
    scenario_col, # column name of scenarios to be plotted
    mpr_path,     # File path to shapefile of Main Park Road
    wcas_path,    # File path to shapefile of water conservation areas boundaries
    spop_path,    # File path to shapefile of subpopulation boundaries
    map_extent,   # Extent of map, format: c(xmin, xmax, ymin, ymax)
    map_title,    # Title to be printed at top of map, (ex. "Alligator HSI")
    output_file_name){# Name of Output File
  
  
  #-----------------
  # Set legend/palette information
  
  # Palettes
  ## Individual run score maps P
  score_pal <- viridis::viridis(n = 10)
  score_pal[10] <- "#FCFA95"
  
  # Difference maps
  diff_pal <- brewer.pal(9, "RdYlGn")
  # changes the middle color to match the yellow in viridis scale
  diff_pal[5] <- "#FCFA95" 
  diff_pal
  
  # Legend titles
  SCORE_LENGEND_NAME <- "Percent to Target"
  DIFF_LEGEND_NAME <- levels(df_dif$Scenario)[3]
  DIFF_LEGEND_NAME <- paste0("Difference","\n", DIFF_LEGEND_NAME)
  
  #-----------------
  # Read Shapefiles
  # reads shapefiles, checks for valid geometry, 
  # and makes geometry valid if not valid 
  # crops shapefiles to map extenct
  # create file used to generate scale bar
  
  # WCAS boundaries
  wcas.shp <- st_read(dsn = wcas_path)
  wcas_st_valid <- st_is_valid(wcas.shp)
  if (all(wcas_st_valid, TRUE)) {
    wcas.shp <- wcas.shp
  } else {
    wcas.shp <- st_make_valid(wcas.shp)}  
  
  # Main Park Road 
  mpr.shp <- st_read(dsn = mpr_path) %>%
    sf::st_transform(crs = 26917)
  mpr_st_valid <- st_is_valid(mpr.shp)
  if (all(mpr_st_valid, TRUE)) {
    mpr.shp <- mpr.shp
  } else {
    mpr.shp <- st_make_valid(mpr.shp)}  
  
  # Load CSSS Subpopulations
  # Load AOI mask shapefile
  spop.shp <- st_read(dsn = spop_path) %>%
    st_transform(crs = 26917)
  spop_st_valid <- st_is_valid(spop.shp)
  if (all(spop_st_valid, TRUE)) {
    spop.shp <- spop.shp
  } else {
    spop.shp <- st_make_valid(spop.shp)} 
  # rename NA subpop to AX for later plotting
  spop.shp$SubPopulat[is.na(spop.shp$SubPopulat)] <- "AX"  
  
  # Crop to map extent
  wca.crop <- st_crop(wcas.shp, map_extent)
  mpr.crop <- st_crop(mpr.shp, map_extent)
  spop.crop <- st_crop(spop.shp, map_extent)
  
  # Set scale to create scalebar
  df_scale <- df_dif

  # Get location for Scalebar
  scale_y <- levels(df_dif$Scenario)[3]
  #scale_x <- levels(df_dif$name)[length(levels(df_dif$name))]
  
  #-----------------
  # Set Theme and Scale Factor
  #theme_set(theme_bw())
  scale_factor = 1.5
  legend_scale_factor = 1.3

  #Get center of subpopulations for labels
  subpop_center <- st_centroid(spop.crop)
  subpop_coord <- as.data.frame(st_coordinates(subpop_center))
  subpop_coord$SubPopulat <- subpop_center$SubPopulat
  
  #-----------------
  # Create Main Map Plot
  
  ind_plot <- ggplot() +
  
    # Plot Individual score 
    geom_sf(data = df_ind, aes_string(fill = ind_fill),
            color = "grey25", lwd = 0.05) +
    scale_fill_manual(values = rev(score_pal),
                      guide = guide_legend(order = 1, reverse = TRUE,
                                           override.aes = list(color = NA)),
                      name = SCORE_LENGEND_NAME, drop = FALSE) +

    # Plot facets
    facet_wrap(~Scenario) +
    
    # Plot shapefiles for Main Park Road, WCAS, and area of interest
    geom_sf(data = mpr.crop, colour = "black", lwd = scale_factor*0.5,
            show.legend = F, lty = "dashed") +
    geom_sf(data = wca.crop, colour = "black", alpha = 0,
            lwd = scale_factor*0.5, show.legend = FALSE) +
    geom_sf(data = spop.crop, colour = "Red", alpha = 0,
            lwd = scale_factor*0.4, show.legend = FALSE) +
    geom_shadowtext(data = filter(subpop_coord,
                                  SubPopulat != "A" & SubPopulat != "AX"),
                    aes(x = X, y = Y,label = SubPopulat),
                    color = "White", size = 5, fontface = "bold") +
    geom_shadowtext(data = subpop_coord,
                    aes(x = 509491.978, y = 2837582.318,label = "A"),
                    color = "White", size = 5, fontface = "bold") +
    geom_shadowtext(data = subpop_coord,
                    aes(x = 496950.703, y = 2835201.064,label = "A"),
                    color = "White", size = 5, fontface = "bold") +
    geom_shadowtext(data = subpop_coord,
                    aes(x = 518937.622, y = 2844011.706,label = "AX"),
                    color = "White", size = 5, fontface = "bold") +
    geom_shadowtext(data = subpop_coord,
                    aes(x = 514730.738, y = 2827819.174,label = "AX"),
                    color = "White", size = 5, fontface = "bold") +
    
    # Can use this to set crs or crop the map output if needed,
    # Expand = FALSE does not add extra buffer around plotted extent
    coord_sf(expand = F, xlim = c(map_extent[1], map_extent[2]),
             ylim = c(map_extent[3], map_extent[4])) +
    
    # Set which graticule displayed on x and y axis
    scale_x_continuous(breaks = c(-80.5, -81, -81.5)) +
    scale_y_continuous(breaks = c(25.5, 26.0)) +
    
    # Set theme elements
    theme(
    panel.grid.major = element_blank(), # remove grid lines
    panel.grid.minor = element_blank(), # remove grid lines,
    
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    
    # rotate and position y axis labels
    axis.text.y = element_text(angle = 90, hjust = 0.5,
                               vjust = 0.0, size = 13), 
    # format x axis text
    axis.text.x = element_text(size = 13), 
    
    strip.background = element_rect(colour = "Black"),
    
    #set size of text in strip headings on the facets
    strip.text = element_text(size = legend_scale_factor*13), 
    axis.title = element_blank(), # Remove axis titles
    plot.margin = margin(1.5,1,0,1, unit = "cm"),
    legend.key.height = unit(5, "mm"),
    legend.key.width = unit(5, "mm"),
    legend.key = element_rect(colour = "black", size = 0.5),
    legend.title.align = 0.0,
    legend.margin = margin(0,0,0,0, unit = "mm"),
    text = element_text(family = "sans", size = 16)) 
ind_plot

diff_plot <- ggplot() +
  geom_sf(data = df_dif, aes_string(fill = ind_fill),
          color = "gray25", lwd = 0.05) +
  scale_fill_manual(values = diff_pal, name = DIFF_LEGEND_NAME, drop = FALSE) +
  guides(fill = guide_legend(ncol = 1, reverse = TRUE,
                             override.aes = list(color = NA))) +
  
  facet_wrap(~Scenario) +
  
  # Plot shapefiles for Main Park Road, WCAS, and area of interest
  geom_sf(data = mpr.crop, colour = "black",
          lwd = scale_factor*0.5, show.legend = F, lty = "dashed") +
  geom_sf(data = wca.crop, colour = "black", alpha = 0, 
          lwd = scale_factor*0.5, show.legend = FALSE) +
  geom_sf(data = spop.crop, colour = "Red", alpha = 0,
          lwd = scale_factor*0.4, show.legend = FALSE) +
  geom_sf_text(data = filter(spop.crop, SubPopulat != "A" & SubPopulat != "AX"),
               aes(label = SubPopulat),
               color = "black", size = 5, fontface = "bold") +
  annotate("text", x = 509491.978, y = 2837582.318,
           label = "A", size = 5, fontface = "bold") +
  annotate("text", x = 496950.703, y = 2835201.064,
           label = "A", size = 5, fontface = "bold") +
  annotate("text", x = 518937.622, y = 2844011.706,
           label = "AX", size = 5, fontface = "bold") +
  annotate("text", x = 514730.738, y = 2827819.174,
           label = "AX", size = 5, fontface = "bold") +

  # Add scalebar
  ggsn::scalebar(data = df_scale, transform = F, dist = 10, dist_unit = "km",
                 st.dist = 0.009, st.size = 3.5, st.bottom = TRUE,
                 height = 0.008, border.size = 1,
                 anchor = c(x = 508000, y = 2794151), family = "sans") +
  ggsn::north(data = df_scale, symbol = 12, scale = 0.03,
              anchor = c(x = 493000, y = 2800500)) +
  
  
  # Can use this to set crs or crop the map output if needed,
  # Expand = FALSE does not add extra buffer around plotted extent
  coord_sf(expand = F, xlim = c(map_extent[1], map_extent[2]),
           ylim = c(map_extent[3], map_extent[4])) +
  
  # Set which graticule displayed on x and y axis
  scale_x_continuous(breaks = c(-80.5, -81, -81.5)) +
  scale_y_continuous(breaks = c(25.5, 26.0)) +
  
  # Set theme elements
  theme(
    panel.grid.major = element_blank(), # remove grid lines
    panel.grid.minor = element_blank(), # remove grid lines,
    
    #panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    
    # rotate and position y axis labels
    axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.0, size = 13), 
    # format x axis text
    axis.text.x = element_text(size = 13), 
    
    strip.background = element_rect(colour = "Black"),
    
    #set size of text in strip headings on the facets
    strip.text = element_text(size = legend_scale_factor*13), 
    axis.title = element_blank(), # Remove axis titles
    plot.margin = margin(0.2,1,1,1, unit = "cm"),
    
    legend.key.height = unit(5, "mm"),
    legend.key.width = unit(5, "mm"),
    legend.key = element_rect(colour = "black", size = 0.8),
    legend.title.align = 0.0,
    legend.margin = margin(0,0,0,0, unit = "mm"),
    text = element_text(family = "sans", size = 16)) 
diff_plot

comb <- plot_grid(ind_plot, diff_plot,
                  ncol = 1, rel_widths = 1, rel_heights = 1)
comb <- ggdraw(comb) +
  draw_label(map_title, x = 0.32, y = .95, vjust = 0,
             fontfamily = "serif", size = 20)

ggsave(output_file_name, comb,
       height = 8.5, width = 11, units = "in", scale = 1)
}


