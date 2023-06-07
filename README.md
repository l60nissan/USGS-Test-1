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
* `input_paths.R` Paths to shapefiles that are needed to process and map model output
* `process_definitions.R` Definition of process criteria for model outputs including: target years, breaks and labels for mapped data, titles for maps and figures, variable name in netcdf. These default to the definitions used for LOSOM, but can be updated by the user if necessary for other restoration runs. 

### Sparrow Helper
* `sparrow_helper_table.R` Process Sparrow Helper model output and generate table. Write table to csv
* `sparrow_helper_plots.R` Process Sparrow Helper model output and generates barplot
* `run_SparrowHelper.R` Run full workflow to process Sparrow Helper model output

### Small Fish
* `smallfish_map_functions.R` Functions to generate Small Fish maps
* `smallfish_barplot_functions.R` Functions to generate Small Fish barplot
* `smallfill_workflow.R` Workflow that sources dependency functions, pulls in model output that needs processing, and generates processed model outputs
* `run_smallfish.R` Run full workflow to process Small Fish model output

### NetCDF model output: Alligator, EverSnail, EverWaders, KiteNest, Days Since Dry
* `rest_run_process_functions.R` Functions to process restoration run output that is in NetCDF format. Does not work to process WADEM.
* `rest_run_map_functions.R` Functions to generate maps for model output that is in netcdf format
* `rest_run_workflow.R` Workflow that sources dependency functions, pulls in model output that needs processing and generates maps, barplots, and acreage calculations
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