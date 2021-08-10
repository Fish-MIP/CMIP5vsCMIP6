
README.md for scripts to accompany the **"Next-generation ensemble projections reveal higher climate risks for marine ecosystems"** paper by Tittensor et al. 

This folder contains four master scripts that load libraries and data, and perform analyses on Earth System Models inputs and Marine Ecosystem Models outputs. To reproduce the paper's analyses and figures, these scripts should be run in the following order:        

**Step 1**: run Batch_run_timeseries_inputs.Rmd which a) loads ESMs outputs (temperature, Primary Production, phytoplankton, zooplakton) from CMIP5 and CMIP6 runs, and saves temporary RData files; b) loads temporary files, averages the data and produces Fig 1.  

**Step 2**: run Batch_run_maps_inputs.Rmd which a) loads ESMs outputs (temperature, Primary Production,  phytoplankton, zooplakton) from CMIP5 and CMIP6 runs, averages the data, and saves RData temporary files; b) loads temporary files, calculates changes between the 1990s and the 2090s and produces Fig 2, Fig S1 and Fig S2.

**Step 3**: run Batch_run_timeseries.Rmd which a) loads MEMs outputs (ocean biomass) from CMIP5 and CMIP6 and saves temporary RData files; b) loads temporary files, averages the data and produces Fig 3, Fig 4, Fig S7, Fig S8, Fig S11 and Fig S17.  

**Step 4**: run Batch_run_maps.Rmd which a) loads MEMs outputs (ocean biomass) from CMIP5 and CMIP6, averages the data, and saves temporary RData files; b) loads temporary files, calculates changes between the 1990s and the 2090s and produces Fig 5, Fig S9, Fig S12, Fig S18, Fig S5, Fig S13, Fig S19, Fig S6, Fig S10, Fig S16, Fig S20, Fig S4, Fig S15, Fig S3, Fig S14 (in order of production).  

** All outputs from the Marine Ecosystem Models and standardized forcing variables from Earth System Models are made available via ISI-MIP (see: https://www.isimip.org/gettingstarted/data-access/). Please *NOTE* that ISI-MIP team is currently checking and editing the format of some MEMs output files. This means scripts in steps 2 to 4 will need editing to read these files. We are updating the scripts as these files reach the final stage and become ready for publication.    

*** Temporary RData files are saved in a separate folder not part of this GitHub repository for space limits - here called ../CMIP5vsCMIP6_data/temp_data/ and automatically created by each master script

**** Figures are saved in a separate folder not part of this GitHub repository for space limits - here called ../CMIP5vsCMIP6_data/figures/ and automatically created by each master script

**** Additional scripts are Funcs_maps.r, which creates maps of environmental data and ocean biomass, and HelperScripts.r which includes functions to extract and average information from ESMs and MEMs' netcdfs. 
