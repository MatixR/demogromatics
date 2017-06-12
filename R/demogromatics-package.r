#' demogromatics.
#'
#' @name demogromatics
#' @docType package
#' # Census Data Aquisition and Mapping

#'### Installation

#'To install the demogromatics package using R:
#'
#'```r
#'install.packages("devtools")
#'library("devtools")
#'devtools::install_github("nancyorgan/demogromatics")
#'library("demogromatics")
#'```
#'
#'### About
#'
#'This package provides tools for:
#'- Batch-downloading United States Census data
#'- Downloading and processing geographic shapefiles in R
#'- Geocoding addresses
#'- Matching coordinates with a Federal Informatin Processing Standards (FIPS) code
#'- Generating a nationwide SES Index based on work by the Agency for Healthcare Research and Quality (AHRQ)
#'
#'### Credit
#'
#'Data obtained using this package comes from the United States Census Bureau. The SES Index was developed by the Agency for Healthcare Research and Quality (AHRQ). Information about the index can be found on the AHRQ website at https://archive.ahrq.gov/research/findings/final-reports/medicareindicators/medicareindicators3.html 
#'
#'### Functions Overview
#'
#'```r
#'state.data(token, state = "*", variables, year = 2010, survey = "sf1")
#'county.data(token, state = "*", county = "*", variables, year = 2010, survey = "sf1")
#'tract.data(token, state = "*", county = "*", variables, year = 2010, survey = "sf1")
#'blockgroup.data(token, state = "*", county = "*", blockgroup = "*", variables, year = 2010, survey = "sf1")
#'FIPS.find(lat, long, year)
#'process.shapefiles(folder, shapefile)
#'download.shapefiles(url)
#'census.geocoder(id, street, city, state, zip, year)
#'census.2000.bg(token, state = "*", variables)
#'process.shapefiles(folder, shapefile)
#'mapquest.geocoder(id, street, city, state, zip, key, year)
#'process.api.data(datalist)
#'SES.index(addresses, mapquest.key, census.key)
#'```
#'
#'### Upcoming Developments
#'
#'I am currently working on updating the demogromatics package to reflect more recent Census Data. The function namespace will reflect this accordingly. 
#'
#'### Details, details!
#'The current version of demogromatics supports US Census 2000 and 2010 Summary File 1 and 3 demographics and corresponding shape files at the block group, tract, county, and state levels.
#'
#'Dependencies are R (>= 3.0.2), rjson, rgdal, ggplot2, data.table, Hmisc, plyr, XML

