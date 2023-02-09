###########################################
# Function to generate maps for netcdf outputs for restoration runs
#
# Caitlin Hackett chackett@usgs.gov
###########################################
#Load Packages
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggnewscale)
library(sf)
library(ggsn)

# SHAPEFILES FOR MAP BACKGROUND - CAN BE CHANGED
#AOI_PATH <- "../../GIS_Library/COP_AOI_mask/COP_AOI_mask.shp" #Area of Interest
#MPR_PATH <- "../../GIS_Library/ENP_main_road/ENP_main_road.shp" # Main Park Road
#WCAS_PATH <- "../../GIS_Library/EVERareas_UTM/EVERareas_UTM/EVERareas_UTM.shp" # WCA Boundaries
#FL_PATH <- "../../GIS_Library/FL_outline_ESRI_2010/FL_outline_ESRI_2010/FL_outline_ESRI_2010.shp" # ESRI Fl boundary

# MAP EXTENT - Will be used to crop shapefiles to desired map extent - can be changed manually to fit different extenxt 
MAP_EXTENT <- c(xmin = 459285.739, xmax= 566303.192, ymin = 2923705.077, ymax = 2774389.980) # Fits COP extent
#MAP_EXTENT <- c(xmin = 459285.739, xmax= 566303.192, ymin = 2921722.375, ymax = 2774389.980) # Fits COP extent

## FUNCTION FOR NORTH ARROW ON MAP
# Edited source code from the north2 function in the ggsn function to return arrow instead of
# jsut printing plot with arrow on top. This allows the arrow to be added on top of a plot and 
# saved using ggplot.Original north2 function source code :https://github.com/oswaldosantos/ggsn/blob/master/R/north2.R
north2_get_arrow <- function(symbol = 1) {
  symbol <- sprintf("%02.f", symbol)
  symbol <- png::readPNG(paste0(system.file('symbols', package = 'ggsn'),
                                '/', symbol, '.png'))
  symbol <- grid::rasterGrob(symbol, interpolate = TRUE)
  ins <- qplot(0:1, 0:1, geom = "blank") + blank() +
    ggmap::inset(symbol, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  #vp <- grid::viewport(x, y, scale, scale)
  return(ins)
}

# FUNCTION TO BUILD MAP 
RESTORATION_RUN_MAP <- function(DF_IND,                       # Dataframe of individual score output with coordinate column names "x", and "y", 
                                IND_FILL,                     # column name of individual score discrete values to be plotted (LOSOM is "labs")
                                DF_DIF,                       # Dataframe of difference output with coordinate column names "x", and "y"
                                DIF_FILL,                     # column name of discrete values to be plotted (LOSOM is "labs")
                                SCENARIO_COL,                 # column name of scenarios to be plotted
                                YEAR_COL,                     # column name of years to be plotted
                                AOI_PATH,                     # File path to shapefile of Area of interest for map
                                MPR_PATH,                     # File path to shapefile of Main Park Road
                                WCAS_PATH,                    # File path to shapefile of water conservation areas boundaries
                                FL_PATH,                      # File path to shapefile of Florida boundary
                                MAP_EXTENT,                   # Extent of map, format: c(xmin, xmax, ymin, ymax)
                                MAP_TITLE,                    # Title to be printed at top of map, (ex. "Alligator HSI")
                                OUTPUT_FILE_NAME){            # Name of Output File
  
  ## SET LEGEDND/PALETTE INFORMATION ##
  
  # PALETTES
  ## Individual run score maps P
  score_pal <- viridis::viridis(n = 10)
  score_pal[10] <- "#FCFA95"
  
  # Difference maps
  diff_pal <- brewer.pal(9, "RdYlGn")
  diff_pal[5] <- "#FCFA95" # changes the middle color to match the yellow in viridis scale
  diff_pal
  
  # LEGEND TITLES
  if (grepl("Days Since Drydown", MAP_TITLE)) {
    SCORE_LENGEND_NAME <- "Days"
    }
  if (grepl("Apple Snail", MAP_TITLE)) {
    SCORE_LENGEND_NAME <- "Population Size"
  }
  if (!(grepl("Apple Snail|Days Since Drydown", MAP_TITLE))) {
    SCORE_LENGEND_NAME <- "Score"
  }
  DIFF_LEGEND_NAME <- levels(DF_DIF$Scenario)[3]
  DIFF_LEGEND_NAME <- paste0("Difference","\n", DIFF_LEGEND_NAME)
  
  ## SHAPEFILES ##
  # reads shapefiles, checks for valid geometry, and makes geometry valid if not valid 
  # crops shapefiles to map extenct
  # create file used to generate scale bar
  
    # Florida boundary
  fl.shp <- st_read(dsn = FL_PATH)%>%
    st_transform(crs=26917)
  fl_st_valid <- st_is_valid(fl.shp)
  if(all(fl_st_valid, TRUE)){
    fl.shp <- fl.shp
    } else {
    fl.shp <- st_make_valid(fl.shp)}  
  
  # WCAS boundaries
  wcas.shp <- st_read(dsn = WCAS_PATH)
  wcas_st_valid <- st_is_valid(wcas.shp)
  if(all(wcas_st_valid, TRUE)){
    wcas.shp <- wcas.shp
    } else {
    wcas.shp <- st_make_valid(wcas.shp)}  
  
  # Main Park Road 
  mpr.shp <- st_read(dsn = MPR_PATH)%>%
    sf::st_transform(crs=26917)
  mpr_st_valid <- st_is_valid(mpr.shp)
  if(all(mpr_st_valid, TRUE)){
    mpr.shp <- mpr.shp
  } else {
    mpr.shp <- st_make_valid(mpr.shp)}  
  
  # Load AOI mask shapefile
  aoi.shp <- st_read(dsn = AOI_PATH)
  aoi_st_valid <- st_is_valid(aoi.shp)
  if(all(aoi_st_valid, TRUE)){
    aoi.shp <- aoi.shp
  } else {
    aoi.shp <- st_make_valid(aoi.shp)}  
  
  # Crop to map extent
  fl.crop  <- st_crop(fl.shp, MAP_EXTENT)
  wca.crop <- st_crop(wcas.shp, MAP_EXTENT)
  mpr.crop <- st_crop(mpr.shp, MAP_EXTENT)
  aoi.crop <- st_crop(aoi.shp, MAP_EXTENT)
  
  # Set scale to create scalebar
  df_scale <- DF_IND
  coordinates(df_scale) <- ~ x+ y
  df_scale <- st_as_sf(df_scale)
  df_scale <- df_scale %>% st_set_crs(st_crs(wcas.shp))
  
  # Get location for Scalebar
  scale_y <- levels(DF_DIF$Scenario)[3]
  scale_x <- levels(DF_DIF$name)[length(levels(DF_DIF$name))]

    ## SET THEME AND SCALE FACTORS ##
  #theme_set(theme_bw())
  scale_factor = 1.5
  legend_scale_factor = 1.3

  ## CREATE LEGEND ##
  # Extract legends from plots -the legends are added in after plotting the map since there are 2 difference scales for fill 
  # Join legends together in to one object
 
  # Individual score legend
  IND_legend <- get_legend(
  ggplot()+
    geom_raster(data = DF_IND , aes_string(x="x", y="y", fill = IND_FILL), na.rm = TRUE, show.legend = TRUE)+
    #scale_fill_viridis_d(option = "D", direction = -1)+
    scale_fill_manual(values = rev(score_pal), name = SCORE_LENGEND_NAME, drop = FALSE)+
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) +
    theme(
      legend.key.height = unit(legend_scale_factor*8, "mm"),
      legend.key.width = unit(legend_scale_factor*8, "mm"),
      legend.key = element_rect(colour = "black", size = legend_scale_factor*0.8),
      legend.title.align = 0.0,
      legend.margin = margin(0,0,0,0, unit = "mm"),
      text = element_text(family = "sans", size = legend_scale_factor*19),
    ))
  #plot(score.legend)

  # Difference legend
  DIF_legend <- get_legend(
  ggplot()+
    geom_raster(data = DF_DIF , aes_string(x="x", y="y", fill = DIF_FILL), na.rm = TRUE, show.legend = TRUE)+
    scale_fill_manual(values = diff_pal, name = DIFF_LEGEND_NAME, drop = FALSE)+
    #scale_fill_brewer(palette = "RdYlBu", name = "Difference", drop = F)+
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) +
    theme(
      legend.key.height = unit(legend_scale_factor*8, "mm"),
      legend.key.width = unit(legend_scale_factor*8, "mm"),
      legend.key = element_rect(colour = "black", size = legend_scale_factor*0.8),
      legend.title.align = 0.0,
      legend.margin = margin(15,0,0,0, unit = "mm"),
      
      text = element_text(family = "sans", size = legend_scale_factor*19),
    ))
  #plot(diff.legend)

  # Join legend together
  FULL_legend <- plot_grid(IND_legend, DIF_legend, align = "v",rel_widths = c(1, 1), ncol = 1)

  ## CREATE MAIN PLOT ##
  PLOT <- ggplot()+
    
    # Plot Individual score 
    #geom_sf(data = fl.crop, fill = "gray90", show.legend = F, lwd = 0.1, alpha = 0.8, colour = "white")+ # show fl coastline
    geom_raster(data = DF_IND , aes_string(x="x", y="y", fill = IND_FILL), na.rm = T, show.legend = FALSE)+
    scale_fill_manual(values = rev(score_pal), name = SCORE_LENGEND_NAME, drop = FALSE)+
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) +
  
    new_scale_fill()+ # Allows for new scale fill (this is how the individual score and differnce maps can be on one figure)
  
    #Plot Difference 
    geom_raster(data = DF_DIF , aes_string(x="x", y="y", fill = DIF_FILL), na.rm = TRUE, show.legend = FALSE)+
    scale_fill_manual(values = diff_pal, name = DIFF_LEGEND_NAME, drop = FALSE)+
    guides(fill=guide_legend(ncol=1, reverse = TRUE)) +
  
    # Plot facets
    facet_grid(as.formula(paste(SCENARIO_COL,"~", YEAR_COL)), labeller = labeller(name = name.labs))+
    
    # Plot shapefiles for Main Park Road, WCAS, and area of interest
    geom_sf(data = mpr.crop, colour = "black", lwd = scale_factor*0.4, show.legend = F, lty = "dashed")+
    geom_sf(data = wca.crop, colour = "black", alpha = 0, lwd = scale_factor*0.3, show.legend = FALSE)+
    geom_sf(data = aoi.crop, colour = "SADDLEBROWN", alpha = 0, lwd = scale_factor*1, show.legend = FALSE)+
    
    # Add scalebar
    ggsn::scalebar(data = df_scale, transform = F, dist = 20, dist_unit = "km", st.dist= 0.015, st.size=scale_factor*2.3, st.bottom = TRUE,
                 height=0.014, border.size = 1, anchor = c(x = 561000 , y = 2778500), family = "sans", facet.var = c(SCENARIO_COL, YEAR_COL), facet.lev = c(scale_y, scale_x))+

    # Can use this to set crs or crop the map output if needed, Expand = FALSE does not add extra buffer around plotted extent
    coord_sf(expand = F)+
    
    # Set which graticule displayed on x and y axis
    scale_x_continuous(breaks = c(-80.5, -81, -81.5))+
    scale_y_continuous(breaks = c(25.5, 26.0))+
    
    # Set theme elements
    theme(
      panel.grid.major = element_blank(), # remove grid lines
      panel.grid.minor = element_blank(), # remove grid lines,
      
      panel.background = element_rect(fill = "white", colour = "black"),
      
      axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.0, size = 16), # rotate and position y axis labels
      axis.text.x = element_text(size = 16), # format x axis text
      
      strip.background = element_rect(colour = "Black"),
      strip.text = element_text(size = legend_scale_factor*18), #set size of text in strip headings on the facets
      axis.title = element_blank())#, # Remove axis titles
      #plot.margin = margin(0,7.9,0,1, unit = "cm")) # Set margins for plot - wider on right side to make space fore legend
  
  # GET NORTH ARROW FOR MAP
  arrow <- north2_get_arrow(symbol = 12)
  arrow
  
  ## FORMAT FINAL PLOT##
  # combines main plot with legends and adds title
  # saves file
  if(grepl("Snail Kite", MAP_TITLE)){
    # add margin to PLOT
    PLOT <- PLOT+
      theme(plot.margin = margin(4, 7.9, 2, 2, unit = "cm"))
    
    combined_plot<- ggdraw(PLOT)+
      draw_plot(FULL_legend, x = 0.31, y = 0.0, vjust = 0.02)+
      draw_label(MAP_TITLE, x = 0.35, y = .94, vjust = 0, fontfamily = "serif", size = 30)+
      draw_plot(arrow, scale = 0.025, x = -0.05, y = -0.44)
  }
  if(grepl("Apple Snail", MAP_TITLE)){
    # add margin to PLOT
    PLOT <- PLOT+
      theme(plot.margin = margin(4, 7.9, 2, 2, unit = "cm"))
    
    combined_plot<- ggdraw(PLOT)+
      draw_plot(FULL_legend, x = 0.33, y = 0.0, vjust = 0.02)+
      draw_label(MAP_TITLE, x = 0.32, y = .94, vjust = 0, fontfamily = "serif", size = 30)+
      draw_plot(arrow, scale = 0.025, x = -0.05, y = -0.44)
  }
  if(grepl("Days Since Drydown", MAP_TITLE)){
    PLOT <- PLOT+
      theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
    
    combined_plot<- ggdraw(PLOT)+
      draw_plot(FULL_legend, x = 0.405, y = 0.0, vjust = 0.02)+
      draw_label(MAP_TITLE, x = 0.16, y = .93, vjust = 0, fontfamily = "serif", size = 30)+
      draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
  }
  if(grepl("Alligator", MAP_TITLE)){
    PLOT <- PLOT+
      theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
    
    combined_plot<- ggdraw(PLOT)+
    draw_plot(FULL_legend, x = 0.405, y = 0.0, vjust = 0.02)+
    draw_label(MAP_TITLE, x = 0.21, y = .93, vjust = 0, fontfamily = "serif", size = 30)+
    draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
  }
  if(grepl("Occupancy", MAP_TITLE)){
    
    if(grepl("Wood Stork", MAP_TITLE)){
      wost_shp <- st_read(dsn = WOST_PATH)%>%
        st_transform(crs=26917)
      wost_st_valid <- st_is_valid(wost_shp)
      if(all(wost_st_valid, TRUE)){
        wost_shp <- wost_shp
      } else {
        wost_shp <- st_make_valid(wost_shp)}
      
      # crop wost colonies
      wost.crop  <- st_crop(wost_shp, aoi.shp)
      
      PLOT <- PLOT+
        geom_sf(data = wost.crop, color = "black", lwd = 4)+
        geom_sf(data = wost.crop, colour = "orange", lwd = 2)+
        coord_sf(expand = F)+
        theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
      
      combined_plot<- ggdraw(PLOT)+
        draw_plot(FULL_legend, x = 0.405, y = 0.0, vjust = 0.02)+
        draw_label(MAP_TITLE, x = 0.17, y = .93, vjust = 0, fontfamily = "serif", size = 30)+
        draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
    }else{
    
    PLOT <- PLOT+
      theme(plot.margin = margin(0,7.9,0,1, unit = "cm"))
    
    combined_plot<- ggdraw(PLOT)+
      draw_plot(FULL_legend, x = 0.405, y = 0.0, vjust = 0.02)+
      draw_label(MAP_TITLE, x = 0.17, y = .93, vjust = 0, fontfamily = "serif", size = 30)+
      draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.399)
  }}
  
## SAVE FINAL PLOT ##  
  #ggsave(OUTPUT_FILE_NAME, combined_plot, height = 9.5, width = 9.5, units = "in", dpi= 300, scale = 2)
  ggsave(OUTPUT_FILE_NAME, combined_plot, height = 11, width = 8.5, units = "in", dpi= 300, scale = 2)
  }


