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