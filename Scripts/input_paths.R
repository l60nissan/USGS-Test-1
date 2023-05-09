## -----------------------------------------------------------------------------
# Restoration Run Process
# Shapefile Locations and Area of Interest (AOI) extent
## -----------------------------------------------------------------------------

# Area of Interest (AOI)
aoi_path <- "../../GIS_Library/rest_run_boundary/rest_run_boundary.shp"
  
  # - Set Extent to match AOI for all output EXCEPT Marl Praire since
  #   Marl prairie extent does not change when project AOI changes
  # - Source extents scripts to see possible options in console
  #   OR you may opt to manually set map_extent using the following format:
  #   map_extent <- c(xmin = minimum x extent value,
  #                 xmax = maximum x extent value,
  #                 ymin = minimum y extent value,
  #                 ymax = maximum y extent value)
  source("./Scripts/extent_options.R") # CHECK CONSOLE FOR OPTIONS 
  map_extent <- werp23_extent

# Main Park Road
mpr_path <- "../../GIS_Library/ENP_main_road/ENP_main_road.shp" 

#WCAs Boundary
wcas_path <- "../../GIS_Library/EVERareas_UTM/EVERareas_UTM/EVERareas_UTM.shp" 

# florida outline
fl_path <- "../../GIS_Library/FL_outline_ESRI_2010/FL_outline_ESRI_2010/FL_outline_ESRI_2010.shp" 

# Wood stork colonies
wost_path <- "../../GIS_Library/WOST_colonies/WOST_colonies_JB.shp"

# CSSS subpopulation - includes AX
subpop_path <- "../../GIS_Library/CSSS_subpopulations_wAX/CSSS_subpopulations_wAX.shp" 

# Marl Prairie Shapefile
# This can be any MP_scores.shp file from the target restoration run output
mp_shp <- "../LOSOM/Data/LOSOM_Round1_2021_05/Model Output/MarlPrairie/MarlPrairie_Data/MarlPrairie_Data/depth_AA/MP_Scores.shp"