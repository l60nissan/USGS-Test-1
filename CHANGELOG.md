# restoration_runs

## 1.1.2 2023-08-01
* Reconcile changes from reviews
* Add DOI link to README.md
* Add viridis to workflow_packages.R and map_functions scripts
* minor comment edits

## 1.1.1 2023-07-19
* Minor edits to README.md (update title, fix typos)

## 1.1.0 2023-07-13
* Update Information in README.md (Overview, process steps, links to models, etc.)
* Add empty directories for "Data" and "Output"
* Add GIS files
* remove extent_options.R because it is not sourced by other scripts

## 1.0.0 2023-07-06
* Pull out user input options (scenario names, file paths, process definitions, etc.) and source within necessary scripts to streamline workflow
* Add option to generate landscape or portrait map outputs
* Update placement of arrow and scale bar on map to use map extent
* Initial commit of "run" scripts that are used to run the workflow for each species model
* Rename files to remove words that are capitalized
* Clean scripts to match style guidelines 
* Add print/INFO statements to scripts
* Update scripts to work with newer R package versions
* Add file the contains sessionInfo() to document R and package versions
* remove unused scripts

## 0.1.2 2020-07-13
* Add scripts to process fish: FISH_workflow.R (perform calculations; generate bar plot and maps) and Fish_map_functions.R (function that makes maps)

## 0.1.1 2020-07-08
* Minor edits to labels

## 0.1.0 2020-07-07
* Init commit of scripts