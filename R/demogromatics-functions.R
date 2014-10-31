#' Process TIGER shapefiles
#' Process shapefiles into a usable format. This function is helpful if you have already downloaded the shapefiles and need to process them from data stored locally. Alternatively, you can use this function as part of an API download.
#'
#'@param folder The path of the folder that contains the shapefile and other files with .dbf, .prj, .xml, and .shx extensions.
#'@param shapefile The name of the shapefile WITHOUT the path or .shp extension
#'@export
#'@examples
#'process.shapefiles(folder = "./my_shapefiles", shapefile = "tl_2010_47_county00")
#'

process.shapefiles = function(folder, shapefile){
  block = readOGR(dsn = folder, layer = shapefile)
  block@data$id = rownames(block@data)
  block.points = fortify(block, region = "id")
  block.df = join(block.points, block@data, by = "id")
  #block.df = subset(block.df, select = c(long, lat, group, BKGPIDFP00))
  #names(block.df) = c("long", "lat", "group", "GEOID")
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
#'
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
#'geocode.2010("Cloyne", "2600 Ridge Road", "Berkeley", "CA", "94709", 2010)
#'
#'@details There are NO DEFAULTS set. 
#'
#'
census.geocoder = function(id, street, city, state, zip, year){
  street = gsub("[.#,]", "", street)
  street = gsub(" ", "+", street)
  city = gsub(" ", "", city)
  zip = substring(zip, 1, 5)
  url = paste("http://geocoding.geo.census.gov/geocoder/geographies/",
            "address?street=", street,
            "&city=", city, 
            "&state=", state,
            "&zip=", zip,
            "&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&",
            "layers=10&format=json", sep = "")

  doc = lapply(url, function(x) {fromJSON(file = x)})
  scraped = lapply(doc, function(x){ data.frame(unlist(x))})
  scraped = lapply(scraped, function(x) {x[grep("coordinates.x$|coordinates.y$|GEOID$",row.names(x)),]})

  scraped.frame = data.frame(NULL)

  for(i in 1:length(scraped)){
    scraped.frame[i,1] = ifelse(!is.na(unlist(scraped[i])[2]), as.numeric(as.character(unlist(scraped[i])[2])), NA)
    scraped.frame[i,2] = ifelse(!is.na(unlist(scraped[i])[1]), as.numeric(as.character(unlist(scraped[i])[1])), NA)
    scraped.frame[i,3] = ifelse(!is.na(unlist(scraped[i])[3]) & year == 2010, as.numeric(as.character(unlist(scraped[i])[3])),
                        ifelse(!is.na(unlist(scraped[i])[3]) & year != 2010, FIPS.find(long = scraped.frame[i,2], lat = scraped.frame[i,1], year), 
                                       NA))
    scraped.frame[i,4] = street[i]
    scraped.frame[i,5] = city[i]
    scraped.frame[i,6] = state[i]
    scraped.frame[i,7] = zip[i]
  }
  scraped.frame = cbind(id, scraped.frame)
  names(scraped.frame) = c("id", "lat", "long", "GEOID", "street", "city", "state", "zip")
  return(scraped.frame)
}

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

#'
#'@param center Coordinates of the center point. Defaults to c(0,0).
#'@param r Radius in miles. Defaults to 100 miles. 
#'@param n Number of points to define the circle. Defaults to 100. 
#'@export
#'@examples
#'states = Cs(tennessee, kentucky, mississippi, alabama, georgia)
#'tn = map_data("county", region = states)
#'Y = mean(tn[tn$subregion == "davidson",]$lat)
#'X = mean(tn[tn$subregion == "davidson",]$long)
#'around.nashville = circle(center = c(X,Y), r = 100)
#'around.nashville = data.frame(around.nashville, group = rep(1, dim(around.nashville)[1]))
#'ggplot(tn, aes(x = long, y = lat, group = group)) + 
#'geom_polygon(aes(fill = group),color = NA) +
#'  coord_map() +
#'  geom_point(aes(X,Y)) + 
#'  geom_path(dat = around.nashville, aes(x = x, y = y, group = group)) + 
#'  geom_polygon(dat = around.nashville, aes(x = x, y = y, group = group), alpha = 1/5, fill = "hotpink") 

geo.circle = function(center = c(0,0), r = 100, n = 100){
  theta = seq(0,365,length.out = n)
  x = center[1] + abs(cos(center[2]*pi/180))*(r/69)*cos(theta*pi/180)
  y = center[2] + abs(sin(center[2]*pi/180))*(r/69)*sin(theta*pi/180)
  return(data.frame(x,y))
}


#'Geocodes street addresses to latitude, longitude, and desired year's FIPS code (GEOID).
#'@param id Name of each address or row in the data.frame containing addresses
#'@param street Street address excluding appartment or unit numbers
#'@param city City name
#'@param state State abbreviation, ie "TN" vs "Tennessee"
#'@param zip Zip code. First five characters are taken
#'@export
#'@examples
#'mapquest.geocoder("Cloyne", "2600 Ridge Road", "Berkeley", "CA", "94709")
#'
#'@details There are NO DEFAULTS set. 
#'
#'

mapquest.geocoder = function(id, street, city, state, zip, key, year){
  coordinates = data.frame(NULL)
  for(i in 1:length(id)){
    url = paste('http://www.mapquestapi.com/geocoding/v1/address?key=', key,
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
#'
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


