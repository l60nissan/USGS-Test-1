# ------------------------------------------------------------------------------
# Inputs for workflow
# -Set by user
# 1) Scenarios
# 2) Paths to species input/output locations & TRUE/FALSE if those data are cropped
# 3) Path to area of interest & desired extent for output maps
# 4) choose if output should be landscape or portrait (portrait is best for long
#    AOIs (e.g., LOSOM, COP,COP + EVER areas) and Landscape best for square or wide 
#    AOIs (e.g., WERP))
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 1) Scenarios
#   - Order of alternative and baseline scenario names. Scenario names should 
#     be listed in the order the user wants the scenario names listed in the 
#     figure legends. Items will be listed in legend in the same order as 
#     `c(alt_names, base_names)`
#   - Alternative and baseline names should match the format of the names in 
#     in the file path (e.g., Scenario AA must be named as "AA" not "aa")
# ------------------------------------------------------------------------------
# Set Alternate scenario names
alt_names <- c("AA")
alt_names


# Set Baseline scenario names
base_names <- c("ECBr")
base_names

# ------------------------------------------------------------------------------
# 2 ) Input path, output path, and cropped info for each species
# Input and Output paths default to "Data" and "Output" folders within the
# repository but the user may change these paths if necessary
# It is advised to open model outputs to target variable to ensure the `cropped`
# object is set correctly
# ------------------------------------------------------------------------------

## ----------
# Alligator

gator_parent_path <- "./Data/Alligator/"
gator_output_path <- "./Output/Alligator/"
gator_cropped <- FALSE

## ----------
# EverWaders

waders_parent_path <- "./Data/EverWaders/"
waders_output_path <- "./Output/EverWaders/"
waders_cropped <- FALSE

## ----------
# Apple Snail

apsn_parent_path <- "./Data/Apple_Snail/"
apsn_output_path <- "./Output/Apple_Snail/"
apsn_cropped <- FALSE

## ----------
# Snail Kite

snki_parent_path <- "./Data/SnailKite/"
snki_output_path <- "./Output/SnailKite/"
snki_cropped <- TRUE

## ----------
# Days Since Dry

dsd_parent_path <- "./Data/DaysSinceDry/"
dsd_output_path <- "./Output/DaysSinceDry/"
dsd_cropped <- FALSE

## ----------
# Marl Prairie 
# No option for cropped needed

marl_parent_path <- "./Data/MarlPrairie/"
marl_output_path <- "./Output/MarlPrairie/"

## ----------
# Small Fish 
# No option for cropped needed

fish_parent_path <- "./Data/SmallFish/"
fish_output_path <- "./Output/SmallFish/"

## ----------
# Sparrow Helper
# No option for cropped needed

sparrow_parent_path <- "./Data/CSSSHelper/"
sparrow_output_path <- "./Output/CSSSHelper/"

# ------------------------------------------------------------------------------
# 3) Area of interest and desired extent for maps
# ------------------------------------------------------------------------------
# Area of Interest (AOI)
aoi_path <- "./GIS/COP_AOI_mask/COP_AOI_mask.shp"

# ------------------------------------------------------------------------------
# 4) Set output as Landscape or portrait
# portrait is best for long AOIs (e.g., LOSOM, COP,COP + EVER areas)
# Landscape best for square or wide AOIs (e.g., WERP))
#     Landscape: landscape <- TRUE
#     Portrait:  landscape <- FALSE
# ------------------------------------------------------------------------------
landscape <- FALSE
