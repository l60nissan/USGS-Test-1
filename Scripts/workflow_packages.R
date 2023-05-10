## -----------------------------------------------------------------------------
## Install packages needed for Workflow
## -----------------------------------------------------------------------------

packages <- c("tidyverse",
              "cowplot",
              "RColorBrewer",
              "ggnewscale",
              "sf",
              "ggsn",
              "ncdf4", 
              "raster",
              "writexl")
not_installed <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(not_installed)) install.packages(not_installed)
