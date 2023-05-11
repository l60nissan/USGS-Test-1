# ------------------------------------------------------------------------------
# Inputs for workflow
# -Set by user
# 1) Scenarios
# 2) Paths to species input/ouput locations & TRUE/FALSE if those data are cropped
# 3) Path to area of interest & desired extent for output maps
# 4) choose if output should belandscape or portrait (portrait is best for long
#    AOIs (ex.LOSOM, COP,COP + EVER areas) and Landscape best for square or wide 
#    AOIs (ex. WERP))
# ------------------------------------------------------------------------------
print(paste0("INFO [", Sys.time(), "] Loading Workflow inputs"))

# ------------------------------------------------------------------------------
# 1) Scenarios
#   - Order of alternative and baseline names does not matter (ex. does not need
#     to be in alphabetical order)
#   - Alternative and basline names should match the format of the names in 
#     in the file path (ex. Scenario AA must be named as "AA" not "aa")
# ------------------------------------------------------------------------------
# Set Alternate scenario names
alt_names <- c("AA", "BB", "CC", "DD", "EE1", "EE2")
alt_names


# Set Baseline scenario names
base_names <- c("ECBr", "NA25")
base_names

# ------------------------------------------------------------------------------
# 2 ) Input path, output, path, and cropped info for each species
# ------------------------------------------------------------------------------

## ----------
# Alligator

gator_parent_path <- "../WERP/JEM_Alligator_Production_Probability_Model_Data/JEM_Alligator_Production_Probability_Model_Data/"
gator_output_path <- "../data_release_develop/Alligator/"
gator_cropped <- FALSE

## ----------
# EverWaders

waders_parent_path <- "../LOSOM/Data/LOSOM_Round3_2021_12/Model Output/EverWaders/JEM_EverWaders_Data/JEM_EverWaders_Data/"
waders_output_path <- "../data_release_develop/waders/"
waders_cropped <- FALSE

## ----------
# Apple Snail

apsn_parent_path <- NULL
apsn_output_path <- "../data_release_develop/"
apsn_cropped <- FALSE

## ----------
# Snail Kite

snki_parent_path <- NULL
snki_output_path <- "../data_release_develop/"
snki_cropped <- FALSE

## ----------
# Days Since Dry

dsd_parent_path <- NULL
dsd_output_path <- "../data_release_develop/"
dsd_cropped <- FALSE

## ----------
# Marl Prairie 

marl_parent_path <- "../LOSOM/Data/LOSOM_Round1_2021_05/Model Output/MarlPrairie/MarlPrairie_Data/MarlPrairie_Data/"
marl_output_path <- "../data_release_develop/Marl_Prairie/"

## ----------
# Small Fish 

# Path should point to "FISH_TIMESERIES_PSU.csv"
fish_path <- "../LOSOM/Data/LOSOM_Round1_2021_05/Model Output/Fish/JEM_Small_Fish_Density_Model_Data_SA/JEM_Small_Fish_Density_Model_Data_SA/FISH_TIMESERIES_PSU.csv"
fish_output_path <- "../data_release_develop/smallfish/"

## ----------
# Sparrow Helper
sparrow_parent_path <- "../LOSOM/Data/LOSOM_Round1_2021_05/Model Output/CSSSHelper/JEM_Sparrow_Helper_Data/JEM_Sparrow_Helper_Data/"
sparrow_output_path <- "../data_release_develop/CSSS/"

# ------------------------------------------------------------------------------
# 3) Area of interest and desired extent for maps
# ------------------------------------------------------------------------------
# Area of Interest (AOI)
aoi_path <- "../../GIS_Library/WERP_AOI_2023_utm/WERP_AOI_2023_utm.shp"

# ------------------------------------------------------------------------------
# 4) Set output as Landscape or portrait
#     Landscape: landscape <- TRUE
#     Portrait:  landscape <- FALSE
# ------------------------------------------------------------------------------
landscape <- TRUE
