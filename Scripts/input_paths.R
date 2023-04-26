## -----------------------------------------------------------------------------
# Restoration Run Process
# Shapefile Locations
## -----------------------------------------------------------------------------

# Area of Interest
aoi_path <- "../../GIS_Library/rest_run_boundary/rest_run_boundary.shp"

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
mp_shp <- "./Data/LOSOM_Round1_2021_05/Model Output/MarlPrairie/MarlPrairie_Data/MarlPrairie_Data/depth_AA/MP_Scores.shp"

# Map extent - Will be used to crop shapefiles to desired map extent - 
# can be changed manually to fit different extenxt 
map_extent <- c(xmin = 459839,
                xmax = 578905,
                ymin = 2952364,
                ymax = 2777166) # Fits COP + EVER areas extent
