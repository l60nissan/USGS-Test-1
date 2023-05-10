## -----------------------------------------------------------------------------
# Map Extent Options
## -----------------------------------------------------------------------------
# Map extent - Will be used to crop shapefiles to desired map extent
# If new extents are added, add to option for map_extent in message()

# Fits COP + EVER areas extent (includes LOX)
cop_ever_extent <- c(xmin = 459839,
                     xmax = 578905,
                     ymin = 2952364,
                     ymax = 2777166) 

# Fits WERP 2023 AOI
werp23_extent <- c(xmin = 459384,
                   xmax = 559882,
                   ymin = 2914164,
                   ymax = 2817477)

message("Use one of the following to set map extent
      (see extent_options.R for more detailed descriptions of options):
        map_extent <- cop_ever_extent
        map_extent <- werp23_extent")

# Marl prairie Map extent - Always the same extent for Marl Prairie
# focuses on Cape Sable Seaside Sparrow Subpopulations in Everglades National Park 
marl_map_extent <- c(xmin = 485489.89,
                     xmax = 554055.08,
                     ymin = 2791119.88,
                     ymax = 2850143.30)


