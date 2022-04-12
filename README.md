# WaterConsumptionAnalysis

Quantitative data and code for an analysis of water consumption in Indianapolis, Indiana at the census tract level. The results from the analysis are currently under review:

Obringer, R., Nateghi, R., Ma, Z., and Kumar, R. The interpretation of data-drive water consumption models via the use of social norms.

A permanent version of this repository is available on Zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6452575.svg)](https://doi.org/10.5281/zenodo.6452575). 

The code was developed in R version 4.1.2 and last ran on 01 April 2022. The code can be found in waterconsumptionanalysis.R and the associated Rdata files can be found in the 'rdatafiles' folder. To run the R code, make sure to update the path with the location of the downloaded files on your personal device. Each section can be run individually if one uploads the Rdata files beforehand, otherwise, the sections should be run sequentially. Users will need the following R packages to run the code: rgdal (v1.5.28), sp (v1.4.6), dplyr (v1.0.8), ggplot2 (v3.3.5), randomForest (v4.7.1), zoo (v1.8.9), maptools (v1.1.2), rgeos (v0.5.9), corrplot (v0.92), cowplot (v1.1.1), VSURF (v1.1.0).

Three categories of data were collected: demographic data, climate data, and water consumption data, which can be found in the 'InputData' folder. All data were collected in 2019.

Census tract shapefiles can be found in the 'CensusTracts' folder.
