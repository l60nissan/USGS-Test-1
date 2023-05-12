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
  print(paste0("installing packages: ", not_installed))
  install.packages(not_installed)
} else {
  print(paste0("All necessary packages are already installed"))
}
