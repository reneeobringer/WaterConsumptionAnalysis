# WaterConsumptionAnalysis

Quantitative data and code for an analysis of water consumption in Indianapolis, Indiana at the census tract level. The results from the analysis are currently under review:

Obringer, R., Nateghi, R., Ma, Z., and Kumar, R. The interpretation of data-drive water consumption mdoels via the use of social norms.

The code was developed in R version 4.1.2 and last ran on 01 April 2022. The code can be found in waterconsumptionanalysis.R and the associated Rdata files can be found in the 'rdatafiles' folder. To run the R code, make sure to update the path with the location of the downloaded files on your personal device. Each section can be run individually if one uploads the Rdata files beforehand, otherwise, the sections should be run sequentially. Users will need the following R packages to run the code: rgdal, sp, dplyr, ggplot2, randomForest, zoo, maptools, rgeos, corrplot, cowplot, VSURF

Three categories of data were collected: demographic data, climate data, and water consumption data, which can be found in the 'InputData' folder. All data were collected in 2019.

Census tract shapefiles can be found in the 'CensusTracts' folder.
