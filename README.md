# Census Data Aquisition and Mapping

### Installation

To install the demogromatics package using R:

```r
install.packages("devtools")
library("devtools")
devtools::install_github("nancyorgan/demogromatics")
library("demogromatics")
```

### Credit

Data obtained using this package comes from the United States Census Bureau. The SES Index was developed by the Agency for Healthcare Research and Quality (AHRQ). Information about the index can be found on the AHRQ website at https://archive.ahrq.gov/research/findings/final-reports/medicareindicators/medicareindicators3.html 

### Functions Overview

```r
state.data(token, state = "*", variables, year = 2010, survey = "sf1")
county.data(token, state = "*", county = "*", variables, year = 2010, survey = "sf1")
tract.data(token, state = "*", county = "*", variables, year = 2010, survey = "sf1")
blockgroup.data(token, state = "*", county = "*", blockgroup = "*", variables, year = 2010, survey = "sf1")
FIPS.find(lat, long, year)
process.shapefiles(folder, shapefile)
download.shapefiles(url)
census.geocoder(id, street, city, state, zip, year)
census.2000.bg(token, state = "*", variables)
process.shapefiles(folder, shapefile)
mapquest.geocoder(id, street, city, state, zip, key, year)
process.api.data(datalist)
SES.index(addresses, mapquest.key, census.key)
```

### Upcoming Developments

I am currently working on updating the demogromatics package to reflect more recent Census Data. The function namespace will reflect this accordingly. 
 
