# Hexagon shape source:
# http://www.arcgis.com/home/item.html?id=593037bc399e460bb7c6c631ceff67b4 - GB Local Authority Hexagon Cartogram (Esri Shapefile)
# http://www.arcgis.com/home/item.html?id=15baaa6fecd54aa4b7250780b6534682 - GB Parliamentary Constituency Hexagon Cartogram (Esri Shapefile)
# https://www.arcgis.com/home/item.html?id=d348614c97264ae19b0311019a5f2276 - Cartogram geoprocessing tool


pacman::p_load(rgdal, raster)
# Read SHAPEFILE.shp from the current working directory (".")
# For the CSV we should use the local authority shapes only, so constituency below is not needed
# ogrInfo(dsn = './data/GB_Hex_Cartogram_Const', layer = 'GB_Hex_Cartogram_Const')
# const_shp <- readOGR(dsn = './data/GB_Hex_Cartogram_Const', layer = "GB_Hex_Cartogram_Const")
# plot(const_shp)
# const_shp$NAME %>% head
auth_shp <- readOGR(dsn = './data/GB_Hex_Cartogram_LAs', layer = "GB_Hex_Cartogram_LAs")
summary(auth_shp)
plot(head(auth_shp))
names_shp <- as.character(auth_shp$LAD12NM)
names(auth_shp)



# get the actual shapes ====
auth_1 <- readOGR('./data/Local_Authority_Districts_December_2016_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain',
                  layer = 'Local_Authority_Districts_December_2016_Ultra_Generalised_Clipped_Boundaries_in_Great_Britain')