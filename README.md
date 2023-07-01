# restoration_runs

## Overview

restoration_runs is a library of R scripts used to summarize output of JEM models used in restoration planning for projects such as LOSOM and WERP. The scripts create figures and maps for each model.

"process_functions" scripts contain functions that summarize all the data, write it out, and create bar graphs

"maps_functions" scripts contain functions that create maps from the summarized data

"workflow" scripts source the above scripts to create all outputs

## Script Descriptions

### Universal
* `workflow_packages.R` Checks to see if the necessary packages are install and installs any missing packages
* `workflow_inputs.R` Inputs set by the user to process model outputs for specific restoration run
* `species_string_definitions.R` Definitions of species string used in file paths - this might be a string in a parent folder and not the specific NetCDF file. For example, the alligator model output is saved to the folder named JEM_Alligator_Production_Probability_Model_Data and the output NetCDF is named Output.nc
* `shapefile_paths.R` Paths to shapefiles that are needed to process and map model output
* `process_definitions.R` Definition of process criteria for model outputs including: target years, breaks and labels for mapped data, titles for maps and figures, variable name in NetCDF. These default to the definitions used for LOSOM, but can be updated by the user if necessary for other restoration runs.
* `session_info.R` contains sessionInfo() from R session used to generate workflow

### Sparrow Helper
* `sparrow_helper_table.R` Process Sparrow Helper model output and generate table. Write table to csv
* `sparrow_helper_plots.R` Process Sparrow Helper model output and generates bar plot
* `run_SparrowHelper.R` Run full workflow to process Sparrow Helper model output

### Small Fish
* `smallfish_map_functions.R` Functions to generate Small Fish maps
* `smallfish_barplot_functions.R` Functions to generate Small Fish bar plot
* `smallfill_workflow.R` Workflow that sources dependency functions, pulls in model output that needs processing, and generates processed model outputs
* `run_smallfish.R` Run full workflow to process Small Fish model output

### NetCDF model output: Alligator, EverSnail, EverWaders, KiteNest, Days Since Dry
* `rest_run_process_functions.R` Functions to process restoration run output that is in NetCDF format. Does not work to process WADEM.
* `rest_run_map_functions.R` Functions to generate maps for model output that is in NetCDF format
* `rest_run_workflow.R` Workflow that sources dependency functions, pulls in model output that needs processing and generates maps, bar plots, and acreage calculations
* `run_kitenest.R` Run full workflow to process KiteNest
* `run_everwaders.R` Run full workflow to process EverWaders
* `run_dayssincedry.R` Run full workflow to process DaysSinceDry
* `run_applesnail.R` Run full workflow to process AppleSnail
* `run_alligator.R` Run full workflow to process Alligator

### Marl Prairie
* `marlprairie_process_function.R` Functions to process Marl Prairie model outputs
* `marlprairie_map_function.R` Functions to generate maps for Marl Prairie model output
* `marlprairie_workflow.R` Workflow that sources dependency functions, pulls in model output, and generates maps and acreage calculations
* `run_marlprairie.R` Run full workflow to process Marl Prairie 

## Steps to Process Species Model Output
* Download repository - this download will include processing scripts as well as empty directory folders necessary to run the workflow
* Place model output in the associated species sub folder within the `Data` folder
* Run the `run` file for the target species model. For example, run `run_alligator.R` to process alligator model output
* Find processed model output in the associated species sub folder within the `Output` folder

## Joint Ecosystem Modeling (JEM) Species Models Incuded in Repository
* Alligator Production Suitability Index Model (APSI): https://www.jem.gov/Modeling/Alligator
* EverSnail (apple snail): https://www.jem.gov/Modeling/EverSnail