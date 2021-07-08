########################################
# Function to generate marl prairie maps
#
# Caitlin Hackett chackett@usgs.gov
#######################################


#Load Packages
library(tidyverse)
library(cowplot)
library(RColorBrewer)
library(ggnewscale)
library(sf)
library(ggsn)


MPR_PATH <- "../../GIS_Library/ENP_main_road/ENP_main_road.shp" # Main Park Road
WCAS_PATH <- "../../GIS_Library/EVERareas_UTM/EVERareas_UTM/EVERareas_UTM.shp" # WCA Boundaries
FL_PATH <- "../../GIS_Library/FL_outline_ESRI_2010/FL_outline_ESRI_2010/FL_outline_ESRI_2010.shp" # ESRI Fl boundary
SUBPOP_PATH <- "../../GIS_Library/CSSS_subpopulations_wAX/CSSS_subpopulations_wAX.shp" # CSSS subpopulation - includes AX

MAP_EXTENT <- c(xmin = 485489.89, xmax= 554055.08, ymin = 2791119.88, ymax = 2850143.30) # Fits COP extent


MARL_MAP <- function(DF_IND,                       # Dataframe of individual score output with coordinate column names "x", and "y", 
                     IND_FILL,                     # column name of individual score discrete values to be plotted (LOSOM is "labs")
                     DF_DIF,                       # Dataframe of difference output with coordinate column names "x", and "y"
                     DIF_FILL,                     # column name of discrete values to be plotted (LOSOM is "labs")
                     SCENARIO_COL,                 # column name of scenarios to be plotted
                     MPR_PATH,                     # File path to shapefile of Main Park Road
                     WCAS_PATH,                    # File path to shapefile of water conservation areas boundaries
                     SPOP_PATH,                    # File path to shapefile of subpopulation boundaries
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
  SCORE_LENGEND_NAME <- "Percent to Target"
  DIFF_LEGEND_NAME <- levels(DF_DIF$Scenario)[3]
  DIFF_LEGEND_NAME <- paste0("Difference","\n", DIFF_LEGEND_NAME)
  
  ## SHAPEFILES ##
  # reads shapefiles, checks for valid geometry, and makes geometry valid if not valid 
  # crops shapefiles to map extenct
  # create file used to generate scale bar
  
  
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
  
  
  # Load CSSS Subpopulations
  # Load AOI mask shapefile
  spop.shp <- st_read(dsn = SUBPOP_PATH)%>%
    st_transform(crs=26917)
  spop_st_valid <- st_is_valid(spop.shp)
  if(all(spop_st_valid, TRUE)){
    spop.shp <- spop.shp
  } else {
    spop.shp <- st_make_valid(spop.shp)} 
  # rename NA subpop to AX for later plotting
  spop.shp$SubPopulat[is.na(spop.shp$SubPopulat)] <- "AX"  
  
  
  # Crop to map extent
  wca.crop <- st_crop(wcas.shp, MAP_EXTENT)
  mpr.crop <- st_crop(mpr.shp, MAP_EXTENT)
  #aoi.crop <- st_crop(aoi.shp, MAP_EXTENT)
  spop.crop <- st_crop(spop.shp, MAP_EXTENT)
  
  # Set scale to create scalebar
  df_scale <- DF_DIF
  #coordinates(df_scale) <- ~ x+ y
  #df_scale <- st_as_sf(df_scale)
  #df_scale <- df_scale %>% st_set_crs(st_crs(wcas.shp))
  
  # Get location for Scalebar
  scale_y <- levels(DF_DIF$Scenario)[3]
  #scale_x <- levels(DF_DIF$name)[length(levels(DF_DIF$name))]
  
  ## SET THEME AND SCALE FACTORS ##
  #theme_set(theme_bw())
  scale_factor = 1.5
  legend_scale_factor = 1.3

  
ind_plot <- ggplot()+
  
  # Plot Individual score 
  #geom_sf(data = fl.crop, fill = "gray90", show.legend = F, lwd = 0.1, alpha = 0.8, colour = "white")+ # show fl coastline
  geom_sf(data = DF_IND, aes_string(fill = IND_FILL), color = "grey25", lwd = 0.05)+
  scale_fill_manual(values = rev(score_pal), guide = guide_legend(order = 1, reverse = TRUE, override.aes = list(color = NA)), name = SCORE_LENGEND_NAME, drop = FALSE)+
  #guides(fill=guide_legend(ncol=1, reverse = TRUE, order = 1)) +
  
  # Plot facets
  #facet_grid(as.formula(paste(SCENARIO_COL,"~", YEAR_COL)), labeller = labeller(name = name.labs))+
  facet_wrap(~Scenario)+
  
  # Plot shapefiles for Main Park Road, WCAS, and area of interest
  geom_sf(data = mpr.crop, colour = "black", lwd = scale_factor*0.5, show.legend = F, lty = "dashed")+
  geom_sf(data = wca.crop, colour = "black", alpha = 0, lwd = scale_factor*0.5, show.legend = FALSE)+
  geom_sf(data = spop.crop, colour = "Red", alpha = 0, lwd = scale_factor*0.4, show.legend = FALSE)+
  #geom_sf_text(data = filter(spop.crop, SubPopulat == "A"), aes(label = SubPopulat), size = 5)+
  geom_sf_text(data = filter(spop.crop, SubPopulat != "A" & SubPopulat != "AX"), aes(label = SubPopulat), color = "White", size = 5)+
  annotate("text", x = 509491.978, y = 2837582.318, label = "A", size = 5)+
  annotate("text", x = 496950.703, y= 2835201.064, label = "A", size = 5)+
  annotate("text", x = 518937.622, y = 2844011.706, label = "AX", size = 5)+
  annotate("text", x = 514730.738, y = 2827819.174, label = "AX", size = 5)+
  #geom_sf(data = aoi.crop, colour = "Brown", alpha = 0, lwd = scale_factor*1, show.legend = FALSE)+
  
  # Add scalebar
  #ggsn::scalebar(data = df_scale, transform = F, dist = 20, dist_unit = "km", st.dist= 0.015, st.size=scale_factor*2.3, st.bottom = TRUE,
  #               height=0.014, border.size = 1, anchor = c(x = 561000 , y = 2778500), family = "sans", facet.var = c(SCENARIO_COL), facet.lev = c(scale_y))+
  
  # Can use this to set crs or crop the map output if needed, Expand = FALSE does not add extra buffer around plotted extent
  coord_sf(expand = F, xlim = c(MAP_EXTENT[1], MAP_EXTENT[2]), ylim = c(MAP_EXTENT[3], MAP_EXTENT[4]))+
  
  # Set which graticule displayed on x and y axis
  scale_x_continuous(breaks = c(-80.5, -81, -81.5))+
  scale_y_continuous(breaks = c(25.5, 26.0))+
  
  # Set theme elements
  theme(
    panel.grid.major = element_blank(), # remove grid lines
    panel.grid.minor = element_blank(), # remove grid lines,
    
    #panel.background = element_rect(fill = "white", colour = "black"),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    
    axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.0, size = 13), # rotate and position y axis labels
    axis.text.x = element_text(size = 13), # format x axis text
    
    strip.background = element_rect(colour = "Black"),
    strip.text = element_text(size = legend_scale_factor*13), #set size of text in strip headings on the facets
    axis.title = element_blank(), # Remove axis titles
    plot.margin = margin(1.5,1,0,1, unit = "cm"),
    legend.key.height = unit(5, "mm"),
    legend.key.width = unit(5, "mm"),
    legend.key = element_rect(colour = "black", size = 0.5),
    legend.title.align = 0.0,
    legend.margin = margin(0,0,0,0, unit = "mm"),
    text = element_text(family = "sans", size = 16)) 
ind_plot

diff_plot <- ggplot()+
  # Plot Individual score 
  #geom_sf(data = fl.crop, fill = "gray90", show.legend = F, lwd = 0.1, alpha = 0.8, colour = "white")+ # show fl coastline
  geom_sf(data = DF_DIF, aes_string(fill = IND_FILL), color = "gray25", lwd = 0.05)+
  scale_fill_manual(values = diff_pal, name = DIFF_LEGEND_NAME, drop = FALSE)+
  guides(fill=guide_legend(ncol=1, reverse = TRUE, override.aes = list(color = NA))) +
  
  # Plot facets
  #facet_grid(as.formula(paste(SCENARIO_COL,"~", YEAR_COL)), labeller = labeller(name = name.labs))+
  facet_wrap(~Scenario)+
  
  # Plot shapefiles for Main Park Road, WCAS, and area of interest
  geom_sf(data = mpr.crop, colour = "black", lwd = scale_factor*0.5, show.legend = F, lty = "dashed")+
  geom_sf(data = wca.crop, colour = "black", alpha = 0, lwd = scale_factor*0.5, show.legend = FALSE)+
  geom_sf(data = spop.crop, colour = "Red", alpha = 0, lwd = scale_factor*0.4, show.legend = FALSE)+
  geom_sf_text(data = filter(spop.crop, SubPopulat != "A" & SubPopulat != "AX"), aes(label = SubPopulat), color = "black", size = 5)+
  annotate("text", x = 509491.978, y = 2837582.318, label = "A", size = 5)+
  annotate("text", x = 496950.703, y= 2835201.064, label = "A", size = 5)+
  annotate("text", x = 518937.622, y = 2844011.706, label = "AX", size = 5)+
  annotate("text", x = 514730.738, y = 2827819.174, label = "AX", size = 5)+
  #geom_sf(data = aoi.crop, colour = "Brown", alpha = 0, lwd = scale_factor*1, show.legend = FALSE)+
  
  # Add scalebar
  #ggsn::scalebar(data = df_scale, transform = F, dist = 10, dist_unit = "km", st.dist= 0.009, st.size= 3, st.bottom = TRUE,
  #               height=0.008, border.size = 1, anchor = c(x = 551300, y= 2794634.939), family = "sans")+
  ggsn::scalebar(data = df_scale, transform = F, dist = 10, dist_unit = "km", st.dist= 0.009, st.size= 3.5, st.bottom = TRUE,
                 height=0.008, border.size = 1, anchor = c(x = 508000, y= 2794151), family = "sans")+
  ggsn::north(data = df_scale, symbol = 12, scale = 0.03, anchor = c(x = 493000, y = 2800500))+
  
  
  # Can use this to set crs or crop the map output if needed, Expand = FALSE does not add extra buffer around plotted extent
  coord_sf(expand = F, xlim = c(MAP_EXTENT[1], MAP_EXTENT[2]), ylim = c(MAP_EXTENT[3], MAP_EXTENT[4]))+
  
  # Set which graticule displayed on x and y axis
  scale_x_continuous(breaks = c(-80.5, -81, -81.5))+
  scale_y_continuous(breaks = c(25.5, 26.0))+
  
  # Set theme elements
  theme(
    panel.grid.major = element_blank(), # remove grid lines
    panel.grid.minor = element_blank(), # remove grid lines,
    
    #panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill = NA, color = "black", size = 1),
    
    axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.0, size = 13), # rotate and position y axis labels
    axis.text.x = element_text(size = 13), # format x axis text
    
    strip.background = element_rect(colour = "Black"),
    strip.text = element_text(size = legend_scale_factor*13), #set size of text in strip headings on the facets
    axis.title = element_blank(), # Remove axis titles
    plot.margin = margin(0.2,1,1,1, unit = "cm"),
    
    legend.key.height = unit(5, "mm"),
    legend.key.width = unit(5, "mm"),
    legend.key = element_rect(colour = "black", size = 0.8),
    legend.title.align = 0.0,
    legend.margin = margin(0,0,0,0, unit = "mm"),
    text = element_text(family = "sans", size = 16)) 
diff_plot

comb <- plot_grid(ind_plot, diff_plot, ncol = 1, rel_widths = 1, rel_heights = 1)
comb <- ggdraw(comb)+
  draw_label(MAP_TITLE, x = 0.32, y = .95, vjust = 0, fontfamily = "serif", size = 20)

#cairo_pdf(OUTPUT_FILE_NAME, pointsize = 1)
#print(plot(comb))
#dev.off()

ggsave(OUTPUT_FILE_NAME, comb, height = 8.5, width = 11, units = "in", scale = 1)
}


