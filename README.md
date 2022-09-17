# Age Differences in Economic Preferences: Cumulative meta-analyses of risk, time, and social preferences (Bagaïni et al., 2022)
This repository contains all the scripts and data files used to conduct the anaylses reported in the manuscript

 ## Files & Folders
 - Bagaini_etal_workflow: Contains all the data pre-processing, processing, analysis and plotting steps that are reproted in the paper.
 - data: files with the data used to in the analyses. Includes codebooks.
   - covidence : csv files of the raw and formated data extracted from covidence
   - plots: images of plots for certain studies and csvfiles with the data extracted from those plots
   - raw_data: csv files containing the raw data of the studies (obtained from repositiories)
   - summary: csv files of the tidy study data (i.e., merging data extracted from covidence, raw data, plots and data tables) and the effect sizes calculated from these
   - tables: csv files with data tables obtained from studies that contain outcomes of interes.
 - code: scripts and functions used in the analyses *(refer to the Bagaini_etal_workflow.Rmd file)*
 - figures: pngs of figures from the manuscript
 - output: rds and csv files containing output from the analyses *(refer to the Bagaini_etal_workflow.Rmd file)*

## Analyses
For an overview of the analysis workflow and how the different scripts work and files are created/used, save the entire project and run the Bagaini_etal_workflow.Rmd file
