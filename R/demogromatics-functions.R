#' Process TIGER shapefiles
#' Process shapefiles into a usable format. This function is helpful if you have already downloaded the shapefiles and need to process them from data stored locally. Alternatively, you can use this function as part of an API download.
#'
#'@param folder The path of the folder that contains the shapefile and other files with .dbf, .prj, .xml, and .shx extensions.
#'@param shapefile The name of the shapefile WITHOUT the path or .shp extension
#'@export
#'@examples
#'process.shapefiles(folder = "./my_shapefiles", shapefile = "tl_2010_47_county00")
#'process.shapefiles(folder = "./my_shapefiles", shapefile = "tl_2010_53_bg10")


process.shapefiles = function(folder, shapefile){
  block = readOGR(dsn = folder, layer = shapefile)
  block@data$id = rownames(block@data)
  block.points = fortify(block, region = "id")
  block.df = join(block.points, block@data, by = "id")
  block.df
}

#' Puts json data into a usable format.  
#'@param datalist The resulting list from using fromJSON()
#'@export
#'@examples
#'process.api.data(fromJSON(file=url(paste("http://api.census.gov/data/2000/sf3?key=", token,"&get=P001001&for=county:*&in=state:*", sep = ""))))
#'

process.api.data = function(datalist){
  remove.header = datalist[-1]
  remove.NULL = lapply(remove.header, function(x) do.call(c,lapply(x, function(z) {z[is.null(z)] <- NA;z})))
  df = ldply(remove.NULL)
  names(df) = ldply(datalist[1])
  df
}


#'Batch downloads, unzips, and processes TIGER shapefiles. Unprocessed files are stored in a temporary directory and deleted post facto. 
#'@param url URL to download page for TIGER shapefiles.
#'@export
#'@examples
#'TN = download.shapefiles(url = "http://www2.census.gov/geo/tiger/TIGER2010/BG/2000/tl_2010_47_bg00.zip")
#'head(TN)
#'       long      lat group        GEOID
#'1 -88.21252 34.99558   0.1 470719806003
#'2 -88.21373 34.99558   0.1 470719806003
#'3 -88.21489 34.99558   0.1 470719806003
#'4 -88.21767 34.99557   0.1 470719806003
#'5 -88.22340 34.99554   0.1 470719806003
#'6 -88.22428 34.99554   0.1 470719806003
#'
#'TN_2010 = download.shapefiles(url = c("https://www2.census.gov/geo/tiger/TIGER2010/BG/2010/tl_2010_47_bg10.zip"))
#'
download.shapefiles = function(url){
  state.shapes = list(rep(data.frame(NULL), length(url)))
  for(i in 1:length(url)){
    temp.dir = tempdir()
    temp.file = tempfile()
    download.file(url = url[i], destfile = temp.file)
    data = unzip(zipfile = temp.file, exdir = temp.dir)
    state.shapes[[i]] = process.shapefiles(temp.dir, strsplit(basename(url[i]), split = "\\.")[[1]][1])
    unlink(temp.dir)
    unlink(temp.file)
  }
  shapefiles.glued = data.frame(rbindlist(state.shapes))
  return(shapefiles.glued)
}

#'Geocodes street addresses to latitude, longitude, and 2010 Census fips code (GEOID). 
#'@param id Name of each address or row in the data.frame containing addresses
#'@param street Street address excluding appartment or unit numbers
#'@param city City name
#'@param state State abbreviation, ie "TN" vs "Tennessee"
#'@param zip Zip code. First five characters are taken
#'@export
#'@examples
#'census.geocoder("Cloyne", "2600 Ridge Road", "Berkeley", "CA", "94709", 2010)
#'
#'@details There are NO DEFAULTS set. 
#'
#'
census.geocoder = function (id, street, city, state, zip, year){
  street = gsub("[.#,]", "", street)
  street = gsub(" ", "+", street)
  city = gsub(" ", "", city)
  zip = substring(zip, 1, 5)
  url = paste("https://geocoding.geo.census.gov/geocoder/geographies/", 
              "address?street=", street, "&city=", city, "&state=", 
              state, "&zip=", zip, "&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&", 
              "layers=10&format=json", sep = "")
  doc = lapply(url, function(x){
    unlist(fromJSON(file = x))
  })
  scraped = lapply(doc, function(x){
    x[grep("coordinates.x|coordinates.y|GEOID", names(x))]
  })
  scraped.frame = data.frame(NULL)
  pb <- txtProgressBar(min = 0, max = length(scraped), style = 3)
  for (i in 1:length(scraped)) {
    scraped.frame[i, 1] = ifelse(!is.na(unlist(scraped[i])["result.addressMatches.coordinates.y"]), 
                                 as.numeric(as.character(unlist(scraped[i])["result.addressMatches.coordinates.y"])), NA)
    scraped.frame[i, 2] = ifelse(!is.na(unlist(scraped[i])["result.addressMatches.coordinates.x"]), 
                                 as.numeric(as.character(unlist(scraped[i])["result.addressMatches.coordinates.x"])), NA)
    scraped.frame[i, 3] = ifelse(!is.na(scraped.frame[i, 1]) & !is.na(scraped.frame[i, 2]),
                                 FIPS.find(long = scraped.frame[i, 2], lat = scraped.frame[i, 1], year), NA)
    scraped.frame[i, 4] = street[i]
    scraped.frame[i, 5] = city[i]
    scraped.frame[i, 6] = state[i]
    scraped.frame[i, 7] = zip[i]
    setTxtProgressBar(pb, i)
  }
  scraped.frame = cbind(id, scraped.frame)
  names(scraped.frame) = c("id", "lat", "long", "GEOID", "street", 
                           "city", "state", "zip")
  return(scraped.frame)
}

#' 2000 Block Group Data (deprecated)
#'@param token Go to http://www.census.gov/developers/ to request an API key. This function will not work if you do not have your own unique key. There is no default.
#'@param state A state FIPS code (stored as a character, ie "47"). Defaults to "*" for every state in the US. 
#'@param variables The desired variables for which you would like data. Variable names found here: http://api.census.gov/data/2000/sf3/variables.html
#'@export
#'@examples
#'example = census.2000.bg(token = "nancysKey", state = "47", variables = "P001001")
#'head(example)
#' P001001        GEOID
#'1    1035 470010201001
#'2     751 470010201002
#'3     577 470010201003
#'4     970 470010202001
#'5    3519 470010202002
#'6     714 470010202003
#'
#'
#'
#'
census.2000.bg = function(token, state = "*", variables ){
  mycounties.us = process.api.data(fromJSON(file=url(paste("http://api.census.gov/data/2000/sf3?key=", 
                                                           token,"&get=P001001&for=county:*&in=state:", state, sep = ""))))$county 
  mystates.us = process.api.data(fromJSON(file=url(paste("http://api.census.gov/data/2000/sf3?key=", 
                                                         token,"&get=P001001&for=county:*&in=state:", state, sep = ""))))$state
  per.county.us = matrix(paste(paste(paste("http://api.census.gov/data/2000/sf3?key=", token,
                                           "&get=",variables,"&for=block+group:*&in=state:", mystates.us, sep = ""),
                                     "+county:", sep = ""), mycounties.us, sep = ""),ncol = 1)
  
  
  us.blocks = apply(per.county.us, 1, function(x) process.api.data(fromJSON(file=url(x))))
  us.blocks.merged = data.frame(rbindlist(us.blocks))
  us.blocks.merged$county = ifelse(nchar(us.blocks.merged$county) == 1, paste("00",us.blocks.merged$county, sep = ""),
                                   ifelse(nchar(us.blocks.merged$county) == 2, paste("0", us.blocks.merged$county, sep = ""),
                                          us.blocks.merged$county))
  
  us.blocks.merged$tract = ifelse(nchar(us.blocks.merged$tract) == 3, paste("000",us.blocks.merged$tract, sep = ""),
                                  ifelse(nchar(us.blocks.merged$tract) == 4, paste("00", us.blocks.merged$tract, sep = ""),
                                         ifelse(nchar(us.blocks.merged$tract) == 5, paste("0", us.blocks.merged$tract, sep = ""),
                                                us.blocks.merged$tract)))
  
  us.blocks.merged = data.frame(us.blocks.merged, GEOID = apply(data.frame(us.blocks.merged)[,c("state", "county", "tract", "block.group")],
                                                                1, paste, collapse = ""))
  us.blocks.merged = subset(us.blocks.merged, select = -c(state, county, tract, block.group))
  us.blocks.merged = apply(us.blocks.merged, 2, function(x){as.numeric(x)})
  us.blocks.merged = data.frame(us.blocks.merged)
  us.blocks.merged
}


#' Block-Group Level American Community Survey 5-Year Data from 2013
#'@param token Go to http://www.census.gov/developers/ to request an API key. This function will not work if you do not have your own unique key. There is no default.
#'@param state A state FIPS code (stored as a character, ie "47"). Defaults to "*" for every state in the US. 
#'@param variables The desired variables for which you would like data. Variable names found here: http://api.census.gov/data/2011/acs5/variables.html
#'@export
#'@examples
#'test = acs5.2013.blockgroup(token = "yourkey", state = c("47", "53), variables = c("B01003_001E", "B02001_002E", "B02001_003E"))
#'head(test)
#'GEOID B01003_001E B02001_002E B02001_003E
#'1 470010201001        1416         719         418
#'2 470010201002        1639        1246         341
#'3 470010202011        2838        2384         221
#'4 470010202012        1220         948         133
#'5 470010202021        1526        1406          18
#'6 470010202022        1232        1083          43


acs5.2013.blockgroup = function(token, state = "*", variables ){
  
  county.url = paste0("http://api.census.gov/data/2013/acs5?key=", token,"&get=B01003_001E&for=county:*&in=state:", state)
  mycounties.us = rbindlist(lapply(county.url, function(x){process.api.data(fromJSON(file = url(x)))[,c("county", "state")]}))
  vars = paste0("http://api.census.gov/data/2013/acs5?key=", token, "&get=",variables,"&for=block+group:*&in=state:")
  per.county.us = unlist(lapply(vars, function(x){url = paste0(x, mycounties.us$state, "+county:", mycounties.us$county)}))
  
  us.blocks = lapply(per.county.us, function(x){process.api.data(fromJSON(file=url(x)))})
  us.blocks = lapply(us.blocks, function(x) melt(x, id.vars = c("state", "county", "tract", "block group")))
  us.blocks.merged = data.frame(rbindlist(us.blocks))
  us.blocks.merged$county = ifelse(nchar(us.blocks.merged$county) == 1, paste("00",us.blocks.merged$county, sep = ""),
                                   ifelse(nchar(us.blocks.merged$county) == 2, paste("0", us.blocks.merged$county, sep = ""),
                                          us.blocks.merged$county))
  
  us.blocks.merged$tract = ifelse(nchar(us.blocks.merged$tract) == 3, paste("000",us.blocks.merged$tract, sep = ""),
                                  ifelse(nchar(us.blocks.merged$tract) == 4, paste("00", us.blocks.merged$tract, sep = ""),
                                         ifelse(nchar(us.blocks.merged$tract) == 5, paste("0", us.blocks.merged$tract, sep = ""),
                                                us.blocks.merged$tract)))
  
  us.blocks.merged$GEOID = paste0(us.blocks.merged$state, us.blocks.merged$county, us.blocks.merged$tract, us.blocks.merged$block.group)
  us.blocks.merged = subset(us.blocks.merged, select = -c(state, county, tract, block.group))
  us.blocks.merged$value = as.numeric(us.blocks.merged$value)
  us.blocks.merged = dcast(us.blocks.merged, GEOID ~ variable)
  us.blocks.merged
}


#'Geocodes street addresses to latitude, longitude, and desired year's FIPS code (GEOID).
#'@param id Name of each address or row in the data.frame containing addresses
#'@param street Street address excluding appartment or unit numbers
#'@param city City name
#'@param state State abbreviation, ie "TN" vs "Tennessee"
#'@param zip Zip code. First five characters are taken
#'@export
#'@examples
#'mapquest.geocoder("Cloyne", "2600 Ridge Road", "Berkeley", "CA", "94709", key = YOURKEY, year = 2010)
#'
#'@details There are NO DEFAULTS set. 
#'
#'

mapquest.geocoder = function(id, street, city, state, zip, key, year){
  coordinates = data.frame(NULL)
  for(i in 1:length(id)){
    url = paste('http://open.mapquestapi.com/geocoding/v1/address?key=', URLdecode(key),
                '&outFormat=xml&xml=<address><location>',
                '<street>', street[i], '</street>',
                '<city>', city[i], '</city>',
                '<state>', state[i], '</state>',
                '<postalCode>', zip[i], '</postalCode></location></address>',  sep = "")
    doc = xmlTreeParse(url)
    root = xmlRoot(doc)
    coordinates[i,1] = id[i]
    coordinates[i,2] = xmlValue(root[[2]][[1]][["locations"]][["location"]][["latLng"]][["lat"]]) #lat
    coordinates[i,3] = xmlValue(root[[2]][[1]][["locations"]][["location"]][["latLng"]][["lng"]]) #lng
    coordinates[i,4] = FIPS.find(coordinates[i,2], coordinates[i,3], year)
    coordinates[i,5] = street[i]
    coordinates[i,6] = city[i]
    coordinates[i,7] = state[i]
    coordinates[i,8] = zip[i]
  }
  names(coordinates) = c("id", "lat", "long", "GEOID", "street", "city", "state", "zip")
  coordinates
}

#'Finds FIPS code for a given latitude and longitude
#'@param lat Vector of latitudes
#'@param long Vector of longitudes
#'@param year Census year, ie. 2000, 2010, etc. 
#'@export
#'@examples
#'FIPS.find(lat = 37.87622, long = -122.2585, year = 2000)
#'[1] "060014225002"
#'

FIPS.find = function(lat, long, year){
  url = paste("http://data.fcc.gov/api/block/", year,
              "/find?latitude=", lat,
              "&longitude=", long,
              "&showall=true",
              "&format=xml",sep = "")
  doc = lapply(url, function(x) {xmlParse(x)})
  root = lapply(doc, function(x) {xmlRoot(x)})
  fips = lapply(root, function(x) {unname(xmlAttrs(x[["Block"]]))})
  fips = unlist(lapply(fips, function(x) {ifelse(is.null(x), NA, x)}))
  fips = lapply(fips, function(x) {substr(x, 1, 12)})
  as.vector(unlist(fips))
}


#' State-level Decennial Census data
#' @param token Unique API token
#' @param state Vector of state numbers. Defaults to "*" for all states. 
#' @param variables Vector of variable codes.
#' @param year Either 2000 or 2010; 1990 is partially supported but may have errors. 
#' @param survey Either "sf1" or "sf3"
#' @export
#' @examples
#' state.data(token = token, state = c(47:49), variables = c("P001001", "PCT014003"))
#' state.data(token = token, variables = c("P001001", "PCT014003"))
#' state.data(token = token, state = "*", variables = c("P001001", "PCT014003"))

state.data = function(token, state = "*", variables, year = 2010, survey = "sf1"){
  state = as.character(state)
  variables = paste(variables, collapse = ",")
  year = as.character(year)
  my.url = matrix(paste("http://api.census.gov/data/", year, "/", survey, "?key=", token,
                        "&get=",variables, "&for=state:", state, sep = ""), ncol = 1)
  
  process.url = apply(my.url, 1, function(x) process.api.data(fromJSON(file=url(x))))
  rbind.dat = data.frame(rbindlist(process.url))
  rbind.dat = rbind.dat[, c(tail(seq_len(ncol(rbind.dat)), 1), seq_len(ncol(rbind.dat) - 1))] 
  rbind.dat
}


#' State Level American Community Survey 5-Year Data from 2013
#' @param token Unique API token
#' @param state Vector of state numbers. Defaults to "*" for all states. 
#' @param variables Vector of variable codes.
#' @export
#' @examples
#' acs5.2013.state(token, state = c("47", "53"), variables = c("B01003_001E", "B02001_002E", "B02001_003E"))

acs5.2013.state = function(token, state = "*", variables){
  base = paste0("http://api.census.gov/data/2011/acs5?key=", token,"&get=", variables)
  ss = paste0("&for=state:", state)
  state.url = state = apply(expand.grid(base, ss), 1, function(x){paste0(x[1], x[2])})
  mystates.us = lapply(state.url, FUN = function(x){process.api.data(fromJSON(file = url(x)))})
  mystates.us = lapply(mystates.us, function(x) melt(x, id.vars = c("state")))
  
  rbind.dat = data.frame(rbindlist(mystates.us))
  rbind.dat = dcast(rbind.dat, state ~ variable)
  rbind.dat
}


#' County-level Decennial Census data
#' @param token Unique API token
#' @param state Vector of state numbers. Defaults to "*" for all states. 
#' @param county Vector of county numbers. Defaults to "*" for all counties. 
#' @param variables Vector of variable codes.
#' @param year Either 2000 or 2010; 1990 is partially supported but may have errors. 
#' @param survey Either "sf1" or "sf3"
#' @export
#' @examples
#' county.data(token = token, state = c(47:49), county = "*", variables = c("P001001", "PCT014003"))
#' county.data(token = token, state = c(47), county = 5,variables = c("P001001", "PCT014003","P001001"))


county.data = function(token, state = "*", county = "*", variables, year = 2010, survey = "sf1"){
  state = as.character(state)
  county = as.character(county)
  year = as.character(year)
  variables = paste(variables, collapse = ",")
  
  if(state == "*"){
    state = process.api.data(fromJSON(file=url(
      paste("http://api.census.gov/data/2000/sf3?key=", token,"&get=P001001&for=state:*", sep = ""))))$state
    my.url = matrix(paste("http://api.census.gov/data/", year, "/", survey, "?key=", token, "&get=", 
                          variables,"&for=county:*&in=state:", state, sep = ""),ncol = 1)
  }
  if(state != "*"){
    if(county == "*"){mycounties = as.list(rep("*", length(state)))
    }else{
      mycounties = list(county)
    }
    names(mycounties) = state
    mystates = expand.states(mycounties)
    my.url = matrix(paste("http://api.census.gov/data/", year, "/", survey, "?key=", token, "&get=", 
                          variables,"&for=county:", unlist(mycounties), "&in=state:", 
                          unlist(mystates), sep = ""),ncol = 1)
  }
  
  process.url = apply(my.url, 1, function(x) process.api.data(fromJSON(file=url(x))))
  rbind.dat = data.frame(rbindlist(process.url))
  rbind.dat = rbind.dat[, c(tail(seq_len(ncol(rbind.dat)), 2), seq_len(ncol(rbind.dat) - 2))] 
  rbind.dat
}

#' County Level American Community Survey 5-Year Data from 2013
#' @param token Unique API token
#' @param state Vector of state numbers. Defaults to "*" for all states. 
#' @param county Vector of county numbers. Defaults to "*" for all counties. 
#' @param variables Vector of variable codes.
#' @export
#' @examples
#' example = acs5.2013.county(token, state = c("47", "53"), county = "*", variables)
#' example = acs5.2013.county(token, variables =  variables)
#' head(example)
#' state county B01003_001E B02001_002E B02001_003E
#' 1    01    001       53944       42577        9755
#' 2    01    003      179523      155068       16936
#' 3    01    005       27546       13576       12632
#' 4    01    007       22746       17437        5153
#' 5    01    009       57140       54446         806
#' 6    01    011       10877        3115        7619

acs5.2013.county = function(token, state = "*", county = "*", variables){
  base = paste0("http://api.census.gov/data/2011/acs5?key=", token,"&get=", variables)
  cc = paste0("&for=county:", county)
  ss = paste0("&in=state:", state)
  county_state = apply(expand.grid(cc, ss), 1, function(x){paste0(x[1], x[2])})
  county.url = apply(expand.grid(base, county_state), 1, function(x){paste0(x[1], x[2])})
  mycounties.us = lapply(county.url, FUN = function(x){process.api.data(fromJSON(file = url(x)))})
  us.blocks = lapply(mycounties.us, function(x) melt(x, id.vars = c("state", "county")))
  
  rbind.dat = data.frame(rbindlist(us.blocks))
  rbind.dat = dcast(rbind.dat, state + county ~ variable)
  rbind.dat
}


#' Tract-level Decennial Census data
#' @param token Unique API token
#' @param state Vector of state numbers. Defaults to "*" for all states. 
#' @param county Vector of county numbers. Defaults to "*" for all counties. 
#' @param variables Vector of variable codes.
#' @param year Either 2000 or 2010; 1990 is partially supported but may have errors. 
#' @param survey Either "sf1" or "sf3"
#' @export
#' @examples
#' tract.data(token = token, state = 47, county = "*", variables = c("P001001", "PCT014003"))
#' tract.data(token = token, state = 47, county = 5, variables = c("P001001", "PCT014003"))

tract.data = function(token, state = "*", county = "*", variables, year = 2010, survey = "sf1"){
  state = as.character(state)
  county = as.character(county)
  year = as.character(year)
  variables = paste(variables, collapse = ",")
  
  if(state == "*"){
    state = process.api.data(fromJSON(file=url(
      paste("http://api.census.gov/data/2000/sf3?key=", token,"&get=P001001&for=state:*", sep = ""))))$state
  }
  if(county == "*"){
    my.url = matrix(paste("http://api.census.gov/data/", year, "/", survey, "?key=", token,
                          "&get=",variables,"&for=tract:*&in=state:", state, sep = ""),ncol = 1)
  }else{
    mycounties = list(county)
    names(mycounties) = state
    mystates = expand.states(mycounties)
    my.url = matrix(paste("http://api.census.gov/data/", year, "/", survey, "?key=", token,
                          "&get=",variables,"&for=tract:*&in=state:", unlist(mystates),
                          "+county:", unlist(mycounties), sep = ""),ncol = 1)
  }
  process.url = apply(my.url, 1, function(x) process.api.data(fromJSON(file=url(x))))
  rbind.dat = data.frame(rbindlist(process.url))
  rbind.dat = rbind.dat[, c(tail(seq_len(ncol(rbind.dat)), 3), seq_len(ncol(rbind.dat) - 3))] 
  rbind.dat
}


#' Blockgroup-level Decennial Census data
#' @param token Unique API token
#' @param state Vector of state numbers. Defaults to "*" for all states. 
#' @param county Vector of county numbers. Defaults to "*" for all counties. 
#' @param blockgroup Vector of blockgroup numbers. Defaults to "*" for all blockgroup 
#' @param variables Vector of variable codes.
#' @param year Either 2000 or 2010; 1990 is partially supported but may have errors. 
#' @param survey Either "sf1" or "sf3"
#' @export
#' @examples
#' blockgroup.data(token = token, state = c(47, 48), county = "*", blockgroup = "*", variables = c("P001001","P001001"))
#' blockgroup.data(token = token, state = 47, county = 1, blockgroup = "*", variables = c("P001001","P001001"))
#' 
blockgroup.data = function(token, state = "*", county = "*", blockgroup = "*", variables, year = 2010, survey = "sf1"){
  state = as.character(state)
  county = as.character(county)
  blockgroup = as.character(blockgroup)
  year = as.character(year)
  variables = paste(variables, collapse = ",")
  
  if(state == "*"){
    state = process.api.data(fromJSON(file=url(
      paste("http://api.census.gov/data/2000/sf3?key=", token,"&get=P001001&for=state:*", sep = ""))))$state
  }
  
  if(county == "*"){
    mycounties = sapply(state, get.counties, token = token)
    #mystates = expand.states(mycounties)
    my.url = matrix(paste("http://api.census.gov/data/", year, "/", survey , "?key=", token,
                          "&get=",variables,"&for=block+group:*&in=state:", state,
                          "+county:", unlist(mycounties), sep = ""),ncol = 1)
  }else{
    mycounties = list(county)
    names(mycounties) = state
    mystates = expand.states(mycounties)
    my.url = matrix(paste("http://api.census.gov/data/", year, "/", survey, "?key=", token,
                          "&get=",variables,"&for=block+group:*&in=state:", unlist(mystates),
                          "+county:", unlist(mycounties), sep = ""),ncol = 1)
  }
  
  process.url = apply(my.url, 1, function(x) process.api.data(fromJSON(file=url(x))))
  rbind.dat = data.frame(rbindlist(process.url))
  rbind.dat = rbind.dat[, c(tail(seq_len(ncol(rbind.dat)), 4), seq_len(ncol(rbind.dat) - 4))] 
  rbind.dat
}

#' Blockgroup-level Decennial Census data
#' @param addresses File path to a .csv or data frame of addresses
#' @param mapquest.key You can get a key by creating a developper's account at http://developer.mapquest.com/web/info/account/app-keys
#' @param census.key You can get a key from http://www.census.gov/developers/
#' @export

SES.index = function(addresses, mapquest.key, census.key){
  
  addresses = read.csv(addresses)
  names(addresses) = c("id", "street", "city", "state", "zip")
  addresses$street = gsub("[.#@,]", "", addresses$street)
  addresses$street = gsub(" ", "+", addresses$street)
  addresses$street = gsub("apt", "", addresses$street)
  addresses$zip = substring(addresses$zip, 1, 5)
  addresses = subset(addresses, street != "")
  
  mapquest = mapquest.geocoder(addresses$id, addresses$street, addresses$city, addresses$state,
                               addresses$zip, mapquest.key, year = 2000)
  mapquest$GEOID = as.character(mapquest$GEOID)
  mapquest$GEOID = ifelse(nchar(mapquest$GEOID) == 11, paste("0", mapquest$GEOID, sep = ""), 
                          mapquest$GEOID)
  
  variables=paste("P043007,P043014,P043003,P043010,P088002,P088003,P088004,",
                  "P088001,P053001,H085001,P037003,P037004,P037005,P037006,",
                  "P037007,P037008,P037009,P037020,P037021,P037022,P037023,",
                  "P037024,P037025,P037026,P037015,P037016,P037017,P037018,",
                  "P037032,P037033,P037034,P037035,P037001,H020005,H020006,",
                  "H020007,H020011,H020012,H020013,H020001,H020008", sep = "")
  
  us.blocks.merged = census.2000.bg(token = census.key,
                                    state = "*" ,
                                    variables = variables)
  
  
  pct_unemp = 100*(us.blocks.merged$P043007 + us.blocks.merged$P043014)/(us.blocks.merged$P043003 + us.blocks.merged$P043010)
  pct_poverty = 100*(us.blocks.merged$P088002 + us.blocks.merged$P088003 + us.blocks.merged$P088004)/us.blocks.merged$P088001
  hhinc100 = us.blocks.merged$P053001/max(us.blocks.merged$P053001)*100
  prop100 = us.blocks.merged$H085001/max(us.blocks.merged$H085001)*100
  low_educ = 100*(us.blocks.merged$P037003 + us.blocks.merged$P037004 + us.blocks.merged$P037005 + 
                    us.blocks.merged$P037006 + us.blocks.merged$P037007 + 
                    us.blocks.merged$P037008 + us.blocks.merged$P037009 +
                    us.blocks.merged$P037020 + us.blocks.merged$P037021 + 
                    us.blocks.merged$P037022 + us.blocks.merged$P037023 + 
                    us.blocks.merged$P037024 + us.blocks.merged$P037025 + 
                    us.blocks.merged$P037026)/us.blocks.merged$P037001
  high_educ = 100*(us.blocks.merged$P037015 + us.blocks.merged$P037016 + us.blocks.merged$P037017 + 
                     us.blocks.merged$P037018 + us.blocks.merged$P037032 + us.blocks.merged$P037033 + 
                     us.blocks.merged$P037034 + us.blocks.merged$P037035)/us.blocks.merged$P037001
  crowded = 100*(us.blocks.merged$H020005 + us.blocks.merged$H020006  + us.blocks.merged$H020007 + 
                   us.blocks.merged$H020011 + us.blocks.merged$H020012 + us.blocks.merged$H020013)/
    (us.blocks.merged$H020001 + us.blocks.merged$H020008)
  
  SES_measures = data.frame(GEOID = us.blocks.merged$GEOID, crowded,prop100, pct_poverty,hhinc100,high_educ,low_educ,pct_unemp)
  SES_measures = SES_measures[complete.cases(SES_measures),] #THIS IS WHERE IT GETS SHRUNK to include only complete rows
  SES_score = 50 + (-0.07*SES_measures$crowded) + (0.08*SES_measures$prop100) + (-0.10*SES_measures$pct_poverty) + 
    (0.11*SES_measures$hhinc100) + (0.10*SES_measures$high_educ) + (-0.11*SES_measures$low_educ) + 
    (-0.08*SES_measures$pct_unemp)
  
  index = data.frame(GEOID = SES_measures$GEOID, SES_score)
  index$GEOID = as.character(index$GEOID)
  index$GEOID = ifelse(nchar(index$GEOID) == 11, paste("0", index$GEOID, sep = ""),
                       index$GEOID)
  index.needed = index[index$GEOID %in% mapquest$GEOID,]  
  final.merge = join(mapquest, index.needed)
  final = subset(final.merge, select = c(id, lat, long, GEOID, SES_score))
  final
}  


#'


get.counties = function(single.state, token){
  process.api.data(fromJSON(file=url(
    paste("http://api.census.gov/data/2000/sf3?key=", token,"&get=P001001&for=county:*&in=state:", single.state, sep = ""))))$county
}

expand.states = function(a){
  thing = list(NULL)
  for(i in 1:length(a)){
    thing[[i]] = rep(names(a)[i], length(a[[i]]))
  }
  thing
}
