## Efforts to tax carbon in Washington State

This repository contains the data and code for the article:

> [Karceski, SM (2022) Efforts to tax carbon in Washington State. *PLOS Climate*, 1(10): : e0000076. https://doi.org/10.1371/journal.pclm.0000076](https://journals.plos.org/climate/article?id=10.1371/journal.pclm.0000076) 

### Structure 

The repository includes the data and scripts necessary to replicate the analysis in the paper. The repository is organized as follows: 

- `data` contains all the data necessary to replicate the analysis presented in the paper. 
  - `acs_county_2018.RData` is the `.RData` object containing the American Community Survey (ACS) 5-year data for 2018 used in the analysis. See `acs_prep.R` for more information. 
  - `annual_generation_state_eia.csv` contains data on the composition of electricity generation within each state; data are from the United States Energy Information Administration (and can be found [here](https://www.eia.gov/electricity/data/state/)). 
  - `countypres_2000-2020.csv` contains presidential election results by U.S. counties for the 2000 through 2020 elections (the 2016 results are used in the paper). This data was retrieved via the MIT Election Data and Science Lab, hosted at the Harvard Dataverse (the data can be found [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)).  
  - `eia_state_production.csv` contains data from the United States Energy Information Administration on the sources of energy electricity production for each state (the data can be found [here](https://www.eia.gov/electricity/data/state/)).
  - `state_1976-200_president_mitdl.csv` contains state-level presidential election results. The data are from the MIT Election Data and Science Lab, hosted at the Harvard Dataversem, and can be found  [here](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX). 
  - `state_level_taxes.csv` contains data on state-level tax rates. The data are from the [Tax Foundation](https://taxfoundation.org/) (the specific web pages are linked to in the .csv file).  
  - `YCOM_2016_Data.01.csv` contains public opinion data from the Yale Program on Climate Change Communication (YPCCC) aggregated at the county level for the year 2016 (the 2016 data is no longer available on the website, but the data for other years can be found [here](https://climatecommunication.yale.edu/visualizations-data/ycom-us/)).
  - `YCOM_2018_Data.csv` contains public opinion data from the YPCCC aggregated at the county level for the year 2018. 
- `syntax` contains scripts used in cleaning and manipulating the data, and those that can be used to replicate the analysis in the paper. 
  - `acs_prep.R` contains code to download and clean the ACS 5-year data for the analysis. 
  - `analysis_figures.R` contains the script 
  - `data_prep.R` contains a script that will load the data used in the `analysis_figures.R` script (and replicate the analysis and figures from the paper). 
  - `energy_data_prep.R` contains 
  - `yale_pccc_prep.R` contains code to load and merge the public opinion data from YPCCC for 2016 and 2018. 
- `tables_figures` contains the final versions of each table and figure included in the paper and in the appendix. 
