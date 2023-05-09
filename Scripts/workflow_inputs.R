# ------------------------------------------------------------------------------
# Inputs for workflow
# -Set by user
# ------------------------------------------------------------------------------

# Scenarios
# Set Alternate scenario names
alt_names <- c("AA", "BB")
alt_names

# Set Baseline scenario names
base_names <- c("NA25")
base_names

# Input path, output, path, and cropped info for each species

## ----------
# Alligator

gator_parent_path <- "../LOSOM/Data/LOSOM_Round3_2021_12/Model Output/Alligator/JEM_Alligator_Production_Probability_Model_Data/"
gator_output_path <- "../data_release_develop/Alligator/"
gator_cropped <- FALSE

## ----------
# EverWaders

waders_parent_path <- NULL
waders_output_path <- "../data_release_develop/"
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

fish_path <- "../LOSOM/Data/LOSOM_Round1_2021_05/Model Output/Fish/JEM_Small_Fish_Density_Model_Data_SA/JEM_Small_Fish_Density_Model_Data_SA/FISH_TIMESERIES_PSU.csv"
fish_output_path <- "../data_release_develop/smallfish/"

