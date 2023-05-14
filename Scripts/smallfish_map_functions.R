## -----------------------------------------------------------------------------
# Function to generate FISH maps for restoration runs
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
library(raster)


## -----------------------------------------------------------------------------
## Function for map North arrow
## -----------------------------------------------------------------------------
# This is a modified version of the north2 function in the ggsn package
# This returns an arrow instead of printing plot with arrow on top.
# This allows the arrow to be added on top of a plot and saved using ggplot
# Original north2 function source code:
# https://github.com/oswaldosantos/ggsn/blob/master/R/north2.R
North2GetArrow <- function(symbol = 1, scale) {
  symbol <- sprintf("%02.f", symbol)
  symbol <- png::readPNG(paste0(system.file('symbols', package = 'ggsn'),
                                '/', symbol, '.png'))
  symbol <- grid::rasterGrob(symbol, interpolate = TRUE)
  ins <- ggplot() +
    theme_void() +
    draw_plot(symbol, scale = scale) 
  return(ins)
}


## -----------------------------------------------------------------------------
# Function to build map 
## -----------------------------------------------------------------------------
FishMap <- function(
    df_ind,       # Dataframe of individual score output with coordinate column names "x", and "y", 
    ind_fill,     # column name of individual score discrete values to be plotted (ex. "labs")
    df_dif,       # Dataframe of difference output with coordinate column names "x", and "y"
    dif_fill,     # column name of discrete values to be plotted (ex. "labs")
    scenario_col, # column name of scenarios to be plotted
    year_col,     # column name of years to be plotted
    aoi_path,     # File path to shapefile of Area of interest for map
    mpr_path,     # File path to shapefile of Main Park Road
    wcas_path,    # File path to shapefile of water conservation areas boundaries
    fl_path,      # File path to shapefile of Florida boundary
    landscape,      # TRUE/FALSE should output be landscape?
    map_title,    # Title to be printed at top of map, (ex. "Alligator HSI")
    output_file_name) { # Name of Output File
  
  #-----------------
  # Set legend/palette information
  
  # Difference maps
  diff_pal <- brewer.pal(9, "RdYlGn")
  # changes the middle color to match the yellow in viridis scale
  diff_pal[5] <- "#FCFA95"
  diff_pal
  
  # Legend Titles
  score_legend_name <- "Mean Total\nFish Density"
  diff_legend_name <- levels(df_dif$Scenario)[3]
  diff_legend_name <- paste0("Percent Change","\n", diff_legend_name)
  #diff_legend_name <- paste0("Percent Change")
  
  
  #-----------------
  # Read Shapefiles
  
  # read shapefiles, checks for valid geometry,
  # and makes geometry valid if not valid 
  # crop shapefiles to map extenct
  # create file used to generate scale bar
  
    # Florida boundary
  fl_shp <- st_read(dsn = fl_path) %>%
    st_transform(crs = 26917)
  fl_st_valid <- st_is_valid(fl_shp)
  if (all(fl_st_valid, TRUE)) {
    fl_shp <- fl_shp
    } else {
    fl_shp <- st_make_valid(fl_shp)}  
  
  # WCAS boundaries
  wcas_shp <- st_read(dsn = wcas_path)
  wcas_st_valid <- st_is_valid(wcas_shp)
  if (all(wcas_st_valid, TRUE)) {
    wcas_shp <- wcas_shp
    } else {
    wcas_shp <- st_make_valid(wcas_shp)}  
  
  # Main Park Road 
  mpr_shp <- st_read(dsn = mpr_path) %>%
    sf::st_transform(crs = 26917)
  mpr_st_valid <- st_is_valid(mpr_shp)
  if (all(mpr_st_valid, TRUE)) {
    mpr_shp <- mpr_shp
  } else {
    mpr_shp <- st_make_valid(mpr_shp)}  
  
  # Load AOI mask shapefile
  aoi_shp <- st_read(dsn = aoi_path)
  aoi_st_valid <- st_is_valid(aoi_shp)
  if (all(aoi_st_valid, TRUE)) {
    aoi_shp <- aoi_shp
  } else {
    aoi_shp <- st_make_valid(aoi_shp)}  
  
  # Set map extent from AOI file
  aoi_extent <- extent(aoi_shp)
  aoi_extent <- c(aoi_extent@xmin - 1000,
                  aoi_extent@xmax + 1000,
                  aoi_extent@ymin - 1000,
                  aoi_extent@ymax + 1000)
  map_extent <- aoi_extent
  
  # Crop to map extent
  wca_crop <- st_crop(wcas_shp, map_extent)
  mpr_crop <- st_crop(mpr_shp, map_extent)
  aoi_crop <- st_crop(aoi_shp, map_extent)
  
  # Set scale to create scalebar
  df_scale <- df_ind
  coordinates(df_scale) <- ~ EASTING+ NORTHING
  df_scale <- st_as_sf(df_scale)
  df_scale <- df_scale %>% st_set_crs(st_crs(wcas_shp))
  
  # Get location for Scalebar
  scale_y <- levels(df_dif$Scenario)[3]
  scale_x <- levels(df_dif$YEAR)[length(levels(df_dif$YEAR))]
  
  #-----------------
  # Set Theme and Scale Factor

    #theme_set(theme_bw())
  scale_factor = 1.5
  legend_scale_factor = 1.3

  #-----------------
  # Create Main Map Plot
 
  fish_plot <- ggplot() +
    
    # Plot Individual score 
    geom_sf(data = wca_crop, fill = "gray91", colour = NA) +
    geom_point(data = df_ind, aes(x = EASTING, y = NORTHING,
                                         size = !!sym(ind_fill)),
               fill = "dodgerblue",
               color = "black", shape = 21, stroke = 1.5, show.legend = TRUE) +
    guides(size = guide_legend(reverse = TRUE, order = 1,
                               title = score_legend_name)) +

    # new scale fill not needed here because using fill and size not 2 fills

    #Plot Difference
    geom_point(data = df_dif, aes(x = EASTING, y = NORTHING,
                                         fill = !!sym(dif_fill)), size = 3.5,
               shape = 21, stroke = 1.5) +
    scale_fill_manual(values = diff_pal, name = diff_legend_name,
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE, order = 2)) +

    # Plot facets
    facet_grid(as.formula(paste(scenario_col,"~", year_col)),
               labeller = labeller(!!year_col := name.labs)) +

    # Plot shapefiles for Main Park Road, WCAS, and area of interest
    geom_sf(data = mpr_crop, colour = "black",
            lwd = scale_factor*0.4, show.legend = F, lty = "dashed") +
    geom_sf(data = wca_crop, colour = "black", alpha = 0,
            lwd = scale_factor*0.3, show.legend = FALSE) +
    geom_sf(data = aoi_crop, colour = "Brown", alpha = 0,
            lwd = scale_factor*1, show.legend = FALSE) +

    # Add scalebar
    ggsn::scalebar(data = df_scale, transform = F, dist = 20, dist_unit = "km",
                   st.dist =  0.015, st.size = scale_factor*2.3,
                   st.bottom = TRUE, height = 0.014, border.size = 1,
                   #location = "bottomright",
                   anchor = c(x = as.numeric(aoi_extent[2] - 4000),
                              y = as.numeric(aoi_extent[3] + 3000)),
                   family = "sans",
                   facet.var = c(scenario_col, year_col),
                   facet.lev = c(scale_y, scale_x)) +
    
    # Can use this to set crs or crop the map output if needed,
    # Expand = FALSE does not add extra buffer around plotted extent
    coord_sf(expand = F) + 
    
    # Set which graticule displayed on x and y axis
    scale_x_continuous(breaks = c(-80.5, -81, -81.5)) +
    scale_y_continuous(breaks = c(25.5, 26.0)) +
    
    labs(title = map_title) + 
    
    # Set theme elements
    theme(
      
      # Format legend elements
      legend.key.height = unit(legend_scale_factor*8, "mm"),
      legend.title.align = 0.0,
      legend.text = element_text(family = "sans", size = legend_scale_factor*16),
      legend.title = element_text(family = "sans", size = legend_scale_factor*19),
      
      # Format panels
      panel.grid.major = element_blank(), # remove grid lines
      panel.grid.minor = element_blank(), # remove grid lines,
      panel.background = element_rect(fill = "white", colour = "black"),
      
      # Format x and y axis
      # rotate and position y axis labels
      axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.0, size = 16), 
      axis.text.x = element_text(size = 16), # format x axis text
      
      # Format strip background
      strip.background = element_rect(colour = "Black"),
      
      # Format size of text in strip headings on the facets
      strip.text = element_text(size = legend_scale_factor*18), 
      
      # Format axis titles
      axis.title = element_blank(), # Remove axis titles
      
      # Format title 
      plot.title = element_text(family = "sans", size = 30),
      
      # Format plot margins
      plot.margin = margin(1.5, 1.5, 1.5, 1.5, unit = "cm")) +
  
  # Set legend margins based on landscape or portrait
  if (landscape) {
    theme(
      legend.spacing = unit(10, "cm"), # increase space between legends
      legend.box.margin = margin(3, 0, 0, 0, unit = "cm"))
  } else {
      theme(
      legend.spacing = unit(20, "cm")) # increase space between legends
  }
  
  
  fish_plot
  #-----------------
  # Get North arrow and plot on map using coordinates
  
  arrow <- North2GetArrow(symbol = 12, scale = 0.25)
  
  # Make arrow grob
  arrow_grob <- cowplot::as_grob(arrow)
  
  # Create tibble to plot North arrow on map using coordinates
  arrow_coord <- tibble(x = as.numeric(aoi_extent[1] + 4000),
                        y = as.numeric(aoi_extent[3] + 3000),
                        grob = list(arrow_grob),
                        !!scenario_col := factor(scale_y, levels = levels(df_ind[[scenario_col]])),
                        !!year_col := scale_x)

  # Plot arrow on map
  fish_plot <- fish_plot +
    geom_grob(data = arrow_coord,
              aes(x, y, label = grob))

  fish_plot  
  
  #-----------------
  # Format Final Plot
  
  # If landscape = TRUE
  if (landscape) {
    
    # Save as full page landscape PDF 
    ggsave(output_file_name, fish_plot, height = 8.5, width = 11,
           units = "in", dpi = 300, scale = 2)
    
  } else { # If portrait (landscape = FALSE)
    
    # Save as full page portrait PDF 
    ggsave(output_file_name, fish_plot, height = 11, width = 8.5,
           units = "in", dpi = 300, scale = 2)
  }
}
  


