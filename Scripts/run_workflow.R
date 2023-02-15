# ------------------------------------------------------------------------------
# Process workflow to make maps, tables and plots for each species
# ------------------------------------------------------------------------------

# ----------
# Alligator
print(paste0("ALLIGATOR :: START -- ", Sys.time()))
source("./scripts/run_alligator.R")
print(paste0("ALLIGATOR :: END -- ", Sys.time()))

# ----------
# AppleSnail
print(paste0("APPLESNAIL :: START -- ", Sys.time()))
source("./scripts/run_applesnail.R")
print(paste0("APPLESNAIL :: END -- ", Sys.time()))

# ----------
# Days Since dry
print(paste0("DAYS SINCE DRY :: START -- ", Sys.time()))
source("./scripts/run_dayssincedry.R")
print(paste0("DAYS SINCE DRY :: END -- ", Sys.time()))

# ----------
# EverWaders
print(paste0("EVERWADERS :: START -- ", Sys.time()))
source("./scripts/run_everwaders.R")
print(paste0("EVERWDAERS :: END -- ", Sys.time()))

# ----------
# KiteNest
print(paste0("KITENEST :: START -- ", Sys.time()))
source("./scripts/run_kitenest.R")
print(paste0("KITENEST :: END -- ", Sys.time()))

