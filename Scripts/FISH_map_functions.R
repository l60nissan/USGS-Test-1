########################################################
# Function to generate FISH maps for restoration runs
#
# Caitlin Hackett chackett@usgs.gov
########################################################
#Load Packages
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggnewscale)
library(sf)
library(ggsn)
library(raster)

# SHAPEFILES FOR MAP BACKGROUND - CAN BE CHANGED
AOI_PATH <- "../../GIS_Library/COP_AOI_mask/COP_AOI_mask.shp" #Area of Interest
MPR_PATH <- "../../GIS_Library/ENP_main_road/ENP_main_road.shp" # Main Park Road
WCAS_PATH <- "../../GIS_Library/EVERareas_UTM/EVERareas_UTM/EVERareas_UTM.shp" # WCA Boundaries
FL_PATH <- "../../GIS_Library/FL_outline_ESRI_2010/FL_outline_ESRI_2010/FL_outline_ESRI_2010.shp" # ESRI Fl boundary

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
           FISH_MAP <- function(DF_IND,                       # Dataframe of individual score output with coordinate column names "x", and "y", 
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
  
  # Difference maps
  diff_pal <- brewer.pal(9, "RdYlGn")
  diff_pal[5] <- "#FCFA95" # changes the middle color to match the yellow in viridis scale
  diff_pal
  
  # LEGEND TITLES
  SCORE_LENGEND_NAME <- "Mean Total\nFish Density"
  DIFF_LEGEND_NAME <- levels(DF_DIF$Scenario)[3]
  DIFF_LEGEND_NAME <- paste0("Percent Change","\n", DIFF_LEGEND_NAME)
  #DIFF_LEGEND_NAME <- paste0("Percent Change")
  
  
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
  coordinates(df_scale) <- ~ EASTING+ NORTHING
  df_scale <- st_as_sf(df_scale)
  df_scale <- df_scale %>% st_set_crs(st_crs(wcas.shp))
  
  # Get location for Scalebar
   #scale_y <- "ECBr"
   #scale_x <- 1995
  scale_y <- levels(DF_DIF$Scenario)[3]
  scale_x <- levels(DF_DIF$YEAR)[length(levels(DF_DIF$YEAR))]
  
  ## SET THEME AND SCALE FACTORS ##
  #theme_set(theme_bw())
  scale_factor = 1.5
  legend_scale_factor = 1.3

  ## CREATE MAIN PLOT ##
  PLOT <- ggplot()+
    
    # Plot Individual score 
    geom_sf(data = wca.crop, fill = "gray91", colour = NA)+
    geom_point(data = DF_IND, aes_string(x = "EASTING", y = "NORTHING", size = IND_FILL),fill = "dodgerblue",
               color = "black", shape = 21, stroke = 1.5, show.legend = TRUE)+
    guides(size=guide_legend(reverse = TRUE, order = 1, title = SCORE_LENGEND_NAME))+

    #new_scale_fill()+ # Allows for new scale fill (this is how the individual score and differnce maps can be on one figure)
    # new scale fill not needed here because using fill and size not 2 fills.
  
    #Plot Difference 
    geom_point(data = DF_DIF, aes_string(x = "EASTING", y = "NORTHING", fill = DIF_FILL), size = 3.5, shape = 21, stroke = 1.5)+
    scale_fill_manual(values = diff_pal, name = DIFF_LEGEND_NAME, drop = FALSE)+
    guides(fill=guide_legend(reverse = TRUE, order = 2)) +

    # Plot facets
    facet_grid(as.formula(paste(SCENARIO_COL,"~", YEAR_COL)), labeller = labeller(YEAR = name.labs))+

    # Plot shapefiles for Main Park Road, WCAS, and area of interest
    geom_sf(data = mpr.crop, colour = "black", lwd = scale_factor*0.4, show.legend = F, lty = "dashed")+
    geom_sf(data = wca.crop, colour = "black", alpha = 0, lwd = scale_factor*0.3, show.legend = FALSE)+
    geom_sf(data = aoi.crop, colour = "Brown", alpha = 0, lwd = scale_factor*1, show.legend = FALSE)+
    
    # Add scalebar
    ggsn::scalebar(data = df_scale, transform = F, dist = 20, dist_unit = "km", st.dist= 0.016, st.size=scale_factor*2.3, st.bottom = TRUE,
                 height=0.014, border.size = 1, anchor = c(x = 561000 , y = 2778500), family = "sans", facet.var = c(SCENARIO_COL, YEAR_COL), facet.lev = c(scale_y, scale_x))+

    # Can use this to set crs or crop the map output if needed, Expand = FALSE does not add extra buffer around plotted extent
    coord_sf(expand = F)+
    
    # Set which graticule displayed on x and y axis
    scale_x_continuous(breaks = c(-80.5, -81, -81.5))+
    scale_y_continuous(breaks = c(25.5, 26.0))+
    
    # Set theme elements
    theme(
      legend.key.height = unit(legend_scale_factor*8, "mm"),
      #legend.key.width = unit(legend_scale_factor*8, "mm"),
      #legend.key = element_rect(colour = "black", size = legend_scale_factor*0.8),
      legend.title.align = 0.0,
      legend.text = element_text(family = "sans", size = legend_scale_factor*16),
      legend.title = element_text(family = "sans", size = legend_scale_factor*19),
      legend.spacing = unit(20, "cm"), # increase space between legends

      panel.grid.major = element_blank(), # remove grid lines
      panel.grid.minor = element_blank(), # remove grid lines,
      
      panel.background = element_rect(fill = "white", colour = "black"),
      
      axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.0, size = 16), # rotate and position y axis labels
      axis.text.x = element_text(size = 16), # format x axis text
      
      strip.background = element_rect(colour = "Black"),
      strip.text = element_text(size = legend_scale_factor*18), #set size of text in strip headings on the facets
      axis.title = element_blank(), # Remove axis titles
      plot.margin = margin(0,1,0,1, unit = "cm")) # Set margins for plot - wider on right side to make space fore legend

  #PLOT
  
  # GET NORTH ARROW FOR MAP
  arrow <- north2_get_arrow(symbol = 12)
  arrow
  
  ## FORMAT FINAL PLOT##
  # combine main plot with arrow and add title
  combined_plot<- ggdraw(PLOT)+
    draw_label(MAP_TITLE, x = 0.17, y = .93, vjust = 0, fontfamily = "serif", size = 30)+
    draw_plot(arrow, scale = 0.025, x = 0.064, y = -0.397)
  
  ## SAVE FINAL PLOT ##  
  ggsave(OUTPUT_FILE_NAME, combined_plot, height = 11, width = 8.5, units = "in", dpi= 300, scale = 2)
}  
  


