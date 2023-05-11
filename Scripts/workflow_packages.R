## -----------------------------------------------------------------------------
## Install packages needed for Workflow
## -----------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Checking for and installing packages"))

packages <- c("tidyverse",
              "cowplot",
              "RColorBrewer",
              "ggnewscale",
              "sf",
              "ggsn",
              "ncdf4", 
              "raster")
not_installed <- packages[!(packages %in% installed.packages()[,"Package"])]
not_installed
if (length(not_installed)) {
  message("installing packages: ", not_installed)
  install.packages(not_installed)
} else {
  message("All necessary packages are already installed")
}
