## -----------------------------------------------------------------------------
# Function to generate maps for netcdf outputs for restoration runs
#
# Caitlin Hackett chackett@usgs.gov
## -----------------------------------------------------------------------------
#Load Packages
packages <- c("tidyverse", "cowplot", "RColorBrewer",
                  "ggnewscale", "sf", "ggsn")
not_installed <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(not_installed)) install.packages(not_installed)

library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggnewscale)
library(sf)
library(ggsn)

## -----------------------------------------------------------------------------
## Function for map North arrow
## -----------------------------------------------------------------------------
# This is a modified version of the north2 function in the ggsn package
# This returns an arrow instead of printing plot with arrow on top.
# This allows the arrow to be added on top of a plot and saved using ggplot
# Original north2 function source code:
# https://github.com/oswaldosantos/ggsn/blob/master/R/north2.R
North2GetArrow <- function(symbol = 1) {
  symbol <- sprintf("%02.f", symbol)
  symbol <- png::readPNG(paste0(system.file('symbols', package = 'ggsn'),
                                '/', symbol, '.png'))
  symbol <- grid::rasterGrob(symbol, interpolate = TRUE)
  ins <- qplot(0:1, 0:1, geom = "blank") + blank() +
    ggmap::inset(symbol, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  #vp <- grid::viewport(x, y, scale, scale)
  return(ins)
}

## -----------------------------------------------------------------------------
# Function to build map 
## -----------------------------------------------------------------------------
RestorationRunMap <- function(
    df_ind,         # Dataframe of individual score output with coordinate column names "x", and "y", 
    ind_fill,       # column name of individual score discrete values to be plotted (LOSOM is "labs")
    df_dif,         # Dataframe of difference output with coordinate column names "x", and "y"
    dif_fill,       # column name of discrete values to be plotted (LOSOM is "labs")
    scenario_col,   # column name of scenarios to be plotted
    year_col,       # column name of years to be plotted
    aoi_path,       # File path to shapefile of Area of interest for map
    mpr_path,       # File path to shapefile of Main Park Road
    wcas_path,      # File path to shapefile of water conservation areas boundaries
    fl_path,        # File path to shapefile of Florida boundary
    landscape,      # TRUE/FALSE should output be landscape?
    map_title,      # Title to be printed at top of map, (ex. "Alligator HSI")
    output_file_name) { # Name of Output File
  
  #-----------------
  # Set legend/palette information

  # Palettes
  # Individual run score maps
  score_pal <- viridis::viridis(n = 10)
  score_pal[10] <- "#FCFA95"
  
  # Difference maps
  diff_pal <- brewer.pal(9, "RdYlGn")
  # change the middle color to match the yellow in viridis scale
  diff_pal[5] <- "#FCFA95" 
  diff_pal

  # Legend Titles
  if (grepl("Days Since Drydown", map_title)) {
    score_legend_name <- "Days"
    }
  if (grepl("Apple Snail", map_title)) {
    score_legend_name <- "Population Size"
  }
  if (!(grepl("Apple Snail|Days Since Drydown", map_title))) {
    score_legend_name <- "Score"
  }
  diff_legend_name <- levels(df_dif$Scenario)[3]
  diff_legend_name <- paste0("Difference","\n", diff_legend_name)
  
  #-----------------
  # Read Shapefiles
  
  # read shapefiles, checks for valid geometry,
  # and makes geometry valid if not valid 
  # crop shapefiles to map extenct
  # create file used to generate scale bar
  
    # Florida boundary
  fl.shp <- st_read(dsn = fl_path) %>%
    st_transform(crs = 26917)
  fl_st_valid <- st_is_valid(fl.shp)
  if (all(fl_st_valid, TRUE)) {
    fl.shp <- fl.shp
    } else {
    fl.shp <- st_make_valid(fl.shp)}  
  
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
  
  # Load AOI mask shapefile
  aoi.shp <- st_read(dsn = aoi_path)
  aoi_st_valid <- st_is_valid(aoi.shp)
  if (all(aoi_st_valid, TRUE)) {
    aoi.shp <- aoi.shp
  } else {
    aoi.shp <- st_make_valid(aoi.shp)}  
  
  # Set map extent from AOI file
  aoi_extent <- extent(aoi.shp)
  aoi_extent <- c(aoi_extent@xmin - 1000,
                  aoi_extent@xmax + 1000,
                  aoi_extent@ymin - 1000,
                  aoi_extent@ymax + 1000)
  map_extent <- aoi_extent
  
  # Crop to map extent
  fl.crop  <- st_crop(fl.shp, map_extent)
  wca.crop <- st_crop(wcas.shp, map_extent)
  mpr.crop <- st_crop(mpr.shp, map_extent)
  aoi.crop <- st_crop(aoi.shp, map_extent)
  
  # Set scale to create scalebar
  df_scale <- df_ind
  coordinates(df_scale) <- ~ x+ y
  df_scale <- st_as_sf(df_scale)
  df_scale <- df_scale %>% st_set_crs(st_crs(wcas.shp))
  
  # Get facet location for Scalebar
  scale_y <- levels(df_dif$Scenario)[3]
  scale_x <- levels(df_dif$name)[length(levels(df_dif$name))]

  #-----------------
  # Set Theme and Scale Factor
  
  #theme_set(theme_bw())
  scale_factor = 1.5
  legend_scale_factor = 1.3

  #-----------------
  # Create Legend
  # Extract legends from plots - the legends are added
  # after plotting the map since there are 2 difference scales for fill.
  # Join legends together in to one object
 
  # Individual score legend
  ind_legend <- get_legend(
  ggplot() +
    geom_raster(data = df_ind , aes(x = x, y = y, fill = !!sym(ind_fill)),
                na.rm = TRUE, show.legend = TRUE) +
    scale_fill_manual(values = rev(score_pal), 
                      name = score_legend_name, drop = FALSE) +
    guides(fill = guide_legend(ncol = 1, reverse = TRUE)) +
    theme(
      legend.key.height = unit(legend_scale_factor*8, "mm"),
      legend.key.width = unit(legend_scale_factor*8, "mm"),
      legend.key = element_rect(colour = "black",
                                size = legend_scale_factor*0.8),
      legend.title.align = 0.0,
      legend.margin = margin(0,0,0,0, unit = "mm"),
      text = element_text(family = "sans", size = legend_scale_factor*19),
    ))

  # Difference legend
  dif_legend <- get_legend(
  ggplot() +
    geom_raster(data = df_dif , aes(x = x, y = y, fill = !!sym(dif_fill)),
                na.rm = TRUE, show.legend = TRUE) +
    scale_fill_manual(values = diff_pal,
                      name = diff_legend_name, drop = FALSE) +
    guides(fill = guide_legend(ncol = 1, reverse = TRUE)) +
    theme(
      legend.key.height = unit(legend_scale_factor*8, "mm"),
      legend.key.width = unit(legend_scale_factor*8, "mm"),
      legend.key = element_rect(colour = "black",
                                size = legend_scale_factor*0.8),
      legend.title.align = 0.0,
      legend.margin = margin(15,0,0,0, unit = "mm"),
      
      text = element_text(family = "sans", size = legend_scale_factor*19),
    ))

  # Join legend together
  full_legend <- plot_grid(ind_legend, dif_legend, align = "v",
                           rel_widths = c(1, 1), ncol = 1)
  
  #-----------------
  # Create Main Map Plot
  
  map_plot <- ggplot() +
    
    # Plot Individual score 
    geom_raster(data = df_ind , aes(x = x, y = y, fill = !!sym(ind_fill)),
                na.rm = T, show.legend = FALSE) +
    scale_fill_manual(values = rev(score_pal),
                      name = score_legend_name, drop = FALSE) +
    guides(fill = guide_legend(ncol = 1, reverse = TRUE)) +
  
    # Allows for new scale fill
    # this is how the individual score and differnce maps can be on one figure
    new_scale_fill() + 
  
    #Plot Difference 
    geom_raster(data = df_dif , aes(x = x, y = y, fill = !!sym(dif_fill)),
                na.rm = TRUE, show.legend = FALSE) +
    scale_fill_manual(values = diff_pal,
                      name = diff_legend_name, drop = FALSE) +
    guides(fill = guide_legend(ncol = 1, reverse = TRUE)) +
  
    # Plot facets
    facet_grid(as.formula(paste(scenario_col,"~", year_col)),
               labeller = labeller(name = name.labs)) +
    
    # Plot shapefiles for Main Park Road, WCAS, and area of interest
    geom_sf(data = mpr.crop, colour = "black",
            lwd = scale_factor*0.4, show.legend = F, lty = "dashed") +
    geom_sf(data = wca.crop, colour = "black", alpha = 0,
            lwd = scale_factor*0.3, show.legend = FALSE) +
    geom_sf(data = aoi.crop, colour = "SADDLEBROWN", alpha = 0,
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

    # Can use this to set crs or crop the map output if needed
    # Expand = FALSE does not add extra buffer around plotted extent
    coord_sf(expand = F) +
    
    # Set which graticule displayed on x and y axis
    scale_x_continuous(breaks = c(-80.5, -81, -81.5)) +
    scale_y_continuous(breaks = c(25.5, 26.0)) +
    
    # Set theme elements
    theme(
      panel.grid.major = element_blank(), # remove grid lines
      panel.grid.minor = element_blank(), # remove grid lines,
      
      panel.background = element_rect(fill = "white", colour = "black"),
      
      # rotate and position y axis labels
      axis.text.y = element_text(angle = 90,
                                 hjust = 0.5, vjust = 0.0, size = 16), 
      # format x axis text
      axis.text.x = element_text(size = 16), 
      
      strip.background = element_rect(colour = "Black"),
     
      #set size of text in strip headings on the facets
      strip.text = element_text(size = legend_scale_factor*18), 
      
      # Remove axis title
      axis.title = element_blank())
      
  # GET NORTH ARROW FOR MAP
  arrow <- North2GetArrow(symbol = 12)
  #arrow <- North2GetArrow(symbol = 1)
  arrow
  
  ## FORMAT FINAL PLOT##
  # combines main plot with legends and adds title
  # saves file
  
  # ----------
  # Format for Landscape layout
  if (landscape) {
    if (grepl("Snail Kite", map_title)) {
      # add margin to PLOT
      map_plot <- map_plot +
        theme(plot.margin = margin(4,5,1,1, unit = "cm"))
      
      combined_plot <- ggdraw(map_plot) +
        draw_plot(full_legend, x = 0.31, y = 0.0, vjust = 0.02) +
        draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                   fontfamily = "serif", size = 30) +
        draw_plot(arrow, scale = 0.025, x = -0.05, y = -0.44)
        draw_plot(arrow, scale = 0.025, x = 0.085, y = -0.45)
      
    }
    
    if (grepl("Apple Snail", map_title)) {
      # add margin to map_plot
      map_plot <- map_plot +
        theme(plot.margin = margin(4,5,1,1, unit = "cm"))
      
      combined_plot <- ggdraw(map_plot) +
        draw_plot(full_legend, x = 0.33, y = 0.0, vjust = 0.02) +
        draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                   fontfamily = "serif", size = 30) +
        draw_plot(arrow, scale = 0.025, x = 0.085, y = -0.45)
    }
    
    if (grepl("Days Since Drydown", map_title)) {
      map_plot <- map_plot +
        theme(plot.margin = margin(4,5,1,1, unit = "cm"))
      
      combined_plot <- ggdraw(map_plot) +
        draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
        draw_label(map_title, x = 0.5, y = .95, vjust = 0,
                   fontfamily = "serif", size = 30) +
        draw_plot(arrow, scale = 0.025, x = 0.085, y = -0.45)
    }
    
    if (grepl("Alligator", map_title)) {
        map_plot <- map_plot +
          theme(plot.margin = margin(4,5,1,1, unit = "cm"))
        
        combined_plot <- ggdraw(map_plot) +
          draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
          draw_label(map_title, x = 0.27, y = .95, vjust = 0,
                     fontfamily = "serif", size = 30) +
          draw_plot(arrow, scale = 0.025, x = 0.085, y = -0.45)
        
    }
    
    if (grepl("Occupancy", map_title)) {
      
      if (grepl("Wood Stork", map_title)) {
        wost_shp <- st_read(dsn = wost_path) %>%
          st_transform(crs = 26917)
        wost_st_valid <- st_is_valid(wost_shp)
        if (all(wost_st_valid, TRUE)) {
          wost_shp <- wost_shp
        } else {
          wost_shp <- st_make_valid(wost_shp)}
        
        # crop wost colonies
        wost.crop  <- st_crop(wost_shp, aoi.shp)
        
        map_plot <- map_plot +
          geom_sf(data = wost.crop, color = "black", lwd = 4) +
          geom_sf(data = wost.crop, colour = "orange", lwd = 2) +
          coord_sf(expand = F) +
          theme(plot.margin = margin(4,5,1,1, unit = "cm"))
        
        combined_plot <- ggdraw(map_plot) +
          draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
          draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                     fontfamily = "serif", size = 30) +
          draw_plot(arrow, scale = 0.025, x = 0.085, y = -0.45)
      } else {
        
        map_plot <- map_plot +
          theme(plot.margin = margin(4,5,1,1, unit = "cm"))
        
        combined_plot <- ggdraw(map_plot) +
          draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
          draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                     fontfamily = "serif", size = 30) +
          draw_plot(arrow, scale = 0.025, x = 0.085, y = -0.45)
      }}
    ggsave(output_file_name, combined_plot, height = 8.5, width = 11,
           units = "in", dpi = 300, scale = 2)
  } else {
  
  # ----------
  # Format for Portrait layout
  if (grepl("Snail Kite", map_title)) {
    # add margin to PLOT
    map_plot <- map_plot +
      theme(plot.margin = margin(4, 7.9, 2, 2, unit = "cm"))
    
    combined_plot <- ggdraw(map_plot) +
      draw_plot(full_legend, x = 0.31, y = 0.0, vjust = 0.02) +
      draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                 fontfamily = "serif", size = 30) +
      draw_plot(arrow, scale = 0.025, x = -0.05, y = -0.44)
  }
  
  if (grepl("Apple Snail", map_title)) {
    # add margin to map_plot
    map_plot <- map_plot +
      theme(plot.margin = margin(4, 7.9, 2, 2, unit = "cm"))
    
    combined_plot <- ggdraw(map_plot) +
      draw_plot(full_legend, x = 0.33, y = 0.0, vjust = 0.02) +
      draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                 fontfamily = "serif", size = 30) +
      draw_plot(arrow, scale = 0.025, x = -0.05, y = -0.44)
  }
  
  if (grepl("Days Since Drydown", map_title)) {
    map_plot <- map_plot +
      theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
    
    combined_plot <- ggdraw(map_plot) +
      draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
      draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                 fontfamily = "serif", size = 30) +
      draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
  }
  
  if (grepl("Alligator", map_title)) {
      map_plot <- map_plot +
        theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
    
      combined_plot <- ggdraw(map_plot) +
    draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
    draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                   fontfamily = "serif", size = 30) +
    draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
    }
  
  if (grepl("Occupancy", map_title)) {
    
    if (grepl("Wood Stork", map_title)) {
      wost_shp <- st_read(dsn = wost_path) %>%
        st_transform(crs = 26917)
      wost_st_valid <- st_is_valid(wost_shp)
      if (all(wost_st_valid, TRUE)) {
        wost_shp <- wost_shp
      } else {
        wost_shp <- st_make_valid(wost_shp)}
      
      # crop wost colonies
      wost.crop  <- st_crop(wost_shp, aoi.shp)
      
      map_plot <- map_plot +
        geom_sf(data = wost.crop, color = "black", lwd = 4) +
        geom_sf(data = wost.crop, colour = "orange", lwd = 2) +
        coord_sf(expand = F) +
        theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
      
      combined_plot <- ggdraw(map_plot) +
        draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
        draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                   fontfamily = "serif", size = 30) +
        draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
    } else {
    
    map_plot <- map_plot +
      theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
    
    combined_plot <- ggdraw(map_plot) +
      draw_plot(full_legend, x = 0.405, y = 0.0, vjust = 0.02) +
      draw_label(map_title, x = 0.5, y = .95, vjust = 0, hjust = 0.5,
                 fontfamily = "serif", size = 30) +
      draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
  }}
  
## SAVE FINAL PLOT ##  
  ggsave(output_file_name, combined_plot, height = 11, width = 8.5,
      units = "in", dpi = 300, scale = 2)
  }
}

