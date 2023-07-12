# restoration_runs

## Overview

restoration_runs is a library of R scripts used to summarize/post-process output of JEM models used in restoration planning for projects such as Lake Okeechobee System Operating Manual (LOSOM) and Western Everglades Restoration Project (WERP). The scripts summarize model output and create figures and maps for each model.

"process_functions" scripts contain functions that summarize all the data, write it out, and create bar graphs

"maps_functions" scripts contain functions that create maps from the summarized data

"workflow" scripts source the above scripts to create all outputs

**This repository can post-process model output from the following species models:**

*Alligator* 

Shinde, D., L. Pearlstine, L.A. Brandt, F.J. Mazzotti, M.W. Parry, B. Jeffery, and A. LoGalbo, 2014. Alligator Production Suitability Index Model (GATOR--PSIM v. 2.0): Ecological and Design Documentation. South Florida Natural Resources Center, Everglades National Park, Homestead, Florida, USA. Ecological Model Report. SFNRC Technical Series 2014:1. <https://jem.gov/downloads/docs/Alligator_Model_Report_Feb18,2015.pdf>

*EverSnail (apple snail)* 

Darby, P.C., D.L. DeAngelis, S.S. Romañach, K. Suir, and J. Bridevaux, 2015. Modeling apple snail population dynamics on the Everglades landscape. Landscape Ecology 30(8): 1497--1510. DOI: 10.1007/s10980-015-0205-5 <https://doi.org/10.1007/s10980-015-0205-5>

*EverSparrow (Cape Sable seaside sparrow)* 

Haider, S.M., Benscoter, A.M., Pearlstine, L., D'Acunto, L.E. and Romañach, S.S., 2021. Landscape-scale drivers of endangered Cape Sable Seaside Sparrow (Ammospiza maritima mirabilis) presence using an ensemble modeling approach. Ecological Modelling, <https://doi.org/10.1016/j.ecolmodel.2021.109774.>

*Small fish* 

Donalson, D., J. Trexler, D. DeAngelis, and A. Logalbo, 2010. Prey-based Freshwater Fish Density Performance Measure (Greater Everglades Aquatic Trophic Levels). DECOMP Performance Measure Documentation Sheet. United States Army Corps of Engineers, Jacksonville, Florida, USA.

*KiteNest (Everglades snail kite)* 

Benscoter AM, D'Acunto LE, Haider SM, Fletcher Jr RJ, Romañach SS. Nest‐site selection model for endangered Everglade snail kites to inform ecosystem restoration. Ecosphere. 2023 Jan;14(1):e4362. <https://doi.org/10.1002/ecs2.4362>

*EverWaders (wading birds)* 

D'Acunto LE, Pearlstine L, Romañach SS (2021) Joint species distribution models of Everglades wading birds to inform restoration planning. PLoS ONE 16(1): e0245973. <https://doi.org/10.1371/journal.pone.0245973>

## Descriptions of R scripts:

### Universal - input paths, string defintions, package installation used in workflow for each species
**Sourced:**
* `workflow_packages.R` Checks to see if the necessary packages are install and installs any missing packages
* `workflow_inputs.R` Inputs set by the user to process model outputs for specific restoration run
* `species_string_definitions.R` Definitions of species string used in file paths - this might be a string in a parent folder and not the specific NetCDF file. For example, the alligator model output is saved to the folder named JEM_Alligator_Production_Probability_Model_Data and the output NetCDF is named Output.nc
* `shapefile_paths.R` Paths to shapefiles that are needed to process and map model output
* `process_definitions.R` Definition of process criteria for model outputs including: target years, breaks and labels for mapped data, titles for maps and figures, variable name in NetCDF. These default to the definitions used for LOSOM, but can be updated by the user if necessary for other restoration runs.

**SessionInfo()**
* `session_info.R` contains sessionInfo() from R session used to generate workflow

### Sparrow Helper
**sourced:**
* `sparrow_helper_table.R` Process Sparrow Helper model output and generate table. Write table to csv
* `sparrow_helper_plots.R` Process Sparrow Helper model output and generates bar plot

**run:**
* `run_SparrowHelper.R` Run full workflow to process Sparrow Helper model output

### Small Fish
**sourced:**
* `smallfish_map_functions.R` Functions to generate Small Fish maps
* `smallfish_barplot_functions.R` Functions to generate Small Fish bar plot
* `smallfill_workflow.R` Workflow that sources dependency functions, pulls in model output that needs processing, and generates processed model outputs

**run:**
* `run_smallfish.R` Run full workflow to process Small Fish model output

### NetCDF model output: Alligator, EverSnail, EverWaders, KiteNest, Days Since Dry
**sourced:**
* `rest_run_process_functions.R` Functions to process restoration run output that is in NetCDF format. Does not work to process WADEM.
* `rest_run_map_functions.R` Functions to generate maps for model output that is in NetCDF format
* `rest_run_workflow.R` Workflow that sources dependency functions, pulls in model output that needs processing and generates maps, bar plots, and acreage calculations

**run:**
* `run_kitenest.R` Run full workflow to process KiteNest
* `run_everwaders.R` Run full workflow to process EverWaders
* `run_dayssincedry.R` Run full workflow to process DaysSinceDry
* `run_applesnail.R` Run full workflow to process AppleSnail
* `run_alligator.R` Run full workflow to process Alligator

### Marl Prairie
**sourced:**
* `marlprairie_process_function.R` Functions to process Marl Prairie model outputs
* `marlprairie_map_function.R` Functions to generate maps for Marl Prairie model output
* `marlprairie_workflow.R` Workflow that sources dependency functions, pulls in model output, and generates maps and acreage calculations

**run:**
* `run_marlprairie.R` Run full workflow to process Marl Prairie 

## Steps to Process Species Model Output
The following steps describe steps to post-process JEM model output. For instructions on how to run individual JEM models please see: https://jem.gov/Modeling

1. Download repository - this download will include processing scripts, GIS files, and empty directory folders necessary to run the workflow

2. Change R working directory to this `restoration_runs` directory. This will ensure all file paths are relative to this repository.

3. If processing COP or LOSOM model output skip to step 4. For restoration projects other than COP or LOSOM:
     - navigate to the associated [ScienceBase](https://www.sciencebase.gov/catalog/) release
     - download the appropriate area of interest (AOI) shapefile
     - place AOI shapefile in the `GIS` directory

4. Place output from each JEM species model in the associated species sub folder within the `Data` folder

5. Open `workflow_inputs.R` and set/review the following inputs:
     - define `alt_names` (line 22) and `base_names` (line 27) as described in script (line 13-19)
     - set AOI shapefile path in the `GIS` directory (line 98). This will only need to updated for projects other than COP or LOSOM
     - set if mapped output should be landscape or portrait (line 107) as described in script (line 101-105) 

6. Execute the `run` script for the target species model. For example, run `run_alligator.R` to process alligator model output

7. Find processed model output in the associated species sub folder within the `Output` folder

## GIS Sources:
**/GIS/EVERareas**: Shapefile containing Everglades management area boundaries used to generate maps. Sourced from:\
FWSBoundaries.zip - Boundaries for USFWS refuges and other properties\
Found: https://ecos.fws.gov/ServCat/Reference/Profile/128178 

Indian_Reservations.zip - Boundaries for American Indian Reservations/Federally Recognized Tribal Entities 
Found: https://www.sciencebase.gov/catalog/item/4f4e4a2ee4b07f02db61576c

NPS_-_Land_Resources_Division_Boundary_and_Tract_Data_Service-shp.zip - Boundaries for National Park Service lands\
Found: https://public-nps.opendata.arcgis.com/datasets/nps-boundary-1

Wildlife_Management_Areas_Florida-shp.zip: Boundaries for Florida wildlife management areas\
Found: https://gis.myfwc.com/Data/KMZ_files/Management%20-%20Wildlife%20Mgt%20Areas%20-%20Generalized%20-%20FL.kmz

**/GIS/WOST_colonies**: Shapefile containing Wood stork nesting colonies. Sourced from:         
  Found: https://ecos.fws.gov/ServCat/Reference/Profile/124368 \
  United States Fish and Wildlife Service, South Florida Field Office, Vero Beach, FL. 2018. Wood Stork Colonies 2009-2018

**/GIS/CSSS_subpopulations**: Shapefile containing Cape Sable seaside sparrow subpopulation boundaries. Producer defined.

**/GIS/ENP_main_road**: Shapefile containing location of Main Park Road in Everglades National Park. Sourced from:\
  Functional_Classification_TDA.zip - roadway functional classifications\
  Found: https://gis-fdot.opendata.arcgis.com/datasets/5c629bc81b104fca8d937343cdcefe29_0/explore 
  
**/GIS/area_of_interest**: Shapefile containing an example restoration run area of interest (AOI). The AOI is specific to each restoration project. The AOI provided here was used for the Combined Operational Plan (COP) and Lake Okeechobee System Operating Manual (LOSOM) projects. For restoration projects other than COP or LOSOM, please see the associated ScienceBase data release to obtain the correct AOI. Sourced from: https://www.sciencebase.gov/catalog/item/621fa557d34ee0c6b38a854f 

## Suggested Citatation
To cite this repository, please use:

## Contact
Caitlin Hackett (chackett@usgs.gov)

## Disclaimer
See [DISCLAIMER.md](https://code.usgs.gov/warc/jem/models/restoration_runs/-/blob/main/DISCLAIMER.md) in this repository

## License
CC0 1.0. See [LICENSE.md](https://code.usgs.gov/warc/jem/models/restoration_runs/-/blob/main/LICENSE.md) for more details.