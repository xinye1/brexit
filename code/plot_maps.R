# setup and loading packages
pacman::p_load(
  devtools, purrr, dplyr, htmlwidgets, stringr, rvest, xml2,
  htmltools, leaflet, cartogram, maptools, broom, ggplot2, ggmap)
source('./code/process_shapes.R')
source('./code/process_votes.R')


# Before plotting the map, the shape file is checked against the CSV for location matching
names_diff <- setdiff(names_votes, names_shp)
names_diff1 <- setdiff(names_shp, names_votes)

summary(la_shp)
# EPSG:3857 project

la_84 <- spTransform(la_shp, CRS('+init=epsg:4326'))
la_bng <- spTransform(la_shp, CRS('+init=epsg:27700'))
# Transverse Mercator projection aka British National Grid

test <- la_84@polygons[[1]]

# Try to understand how the hexagon map is constructed
la_84_f <- fortify(la_84)
b <- bbox(la_1)
b[2, 2] <- 58
basemap <- ggmap(
  get_map(location = b,
          source = "stamen",
          maptype = "watercolor",
          crop = T))
basemap + geom_polygon(
  data = la_84_f,
  aes(x = long, y = lat, group = group),
  fill = 'darkorchid',
  alpha = 0.7)

# Use the British National Grid version to construct NI and Gibralta



# change the name in the CSV (add 'The')
raw_votes$Area[grepl('glamorgan', raw_votes$Area, ignore.case = T)] <- 'The Vale of Glamorgan'

shp_data <- data.frame(name = names_shp) %>% merge(raw_votes, by.x = 'name', by.y = 'Area')
shp_data$Result <- with(shp_data, ifelse(Remain > Leave, 'Remain', 'Leave')) %>% as.factor
table(shp_data$Result)


test <- auth_shp %>% merge(raw_votes, by.x = 'LAD12NM', by.y = 'Area')
test$Result <- ifelse(test$Remain > test$Leave, 'Remain', 'Leave') %>% as.factor
cols <- c('#cc0000', '#009933')
plot(test, col = cols[test$Result])


# Before running the following line, if encounter this error
# Error: isTRUE(gpclibPermitStatus()) is not TRUE
# on Windows, make sure Rtools is installed, then install package gpclib
# install.packages('gpclib', type = 'source')
# To verify everything is fine, run
# gpclibPermitStatus()
# gpclibPermit()
# If all True then continue

# ggplot of hexagons ----
test_f <- test %>% tidy(region = "LAD12NM")
str(as.factor(test_f$id))
test_f1 <- test_f %>%
  left_join(data.frame(id = as.character(test$LAD12NM), Result = as.factor(test$Result), stringsAsFactors = F))
ggplot() + theme_bw() + theme_nothing(legend = TRUE) + coord_fixed() +
  geom_polygon(data = test_f1, aes(x = long, y = lat, group = id, fill = Result))


# Cartogram ----
# install_github("omegahat/Rcartogram")
# install_github('chrisbrunsdon/getcartr', subdir='getcartr')
carto <- cartogram(test, 'Valid_Votes')
plot(carto, col = cols[test$Result])
png('./output/brexit1.png')
plot(carto, col = cols[test$Result])
dev.off()

test_f_cart <- carto %>% tidy(region = 'LAD12NM')
test_f_cart1 <- test_f_cart %>%
  left_join(data.frame(id = as.character(test$LAD12NM), Result = as.factor(test$Result), stringsAsFactors = F))
png('./output/brexit2.png')
ggplot() + theme_bw() + theme_nothing(legend = TRUE) + coord_fixed() +
  geom_polygon(data = test_f_cart1, aes(x = long, y = lat, group = id, fill = Result, colour = Result))
dev.off()



# normal shape ====
auth_data <- auth_1 %>% merge(raw_votes, by.x = 'lad16nm', by.y = 'Area')
auth_data$Result <- ifelse(auth_data$Remain > auth_data$Leave, 'Remain', 'Leave')
# proj4string(auth_data) <- ee_get_const()$wgs84
auth_data@proj4string <- CRS(as.character(NA))
auth_data@proj4string <- ee_get_const()$wgs84
carto1 <- cartogram(auth_data, 'Valid_Votes')
plot(carto1)
carto2 <- carto1 %>% fortify(region = 'Region')
# ggplot()  + theme_bw() + theme_nothing(legend = TRUE) + coord_fixed() +
#   geom_polygon(data = carto1, aes(x = long, y = lat, group = id, fill = as.factor(Result), colour = as.factor(Result)))
# doesn't work because fortifying doesn't work with small shapes?
# Error in RGEOSBinTopoFunc(spgeom1, spgeom2, byid, id, drop_lower_td, unaryUnion_if_byid_false,  : 
# TopologyException: Input geom 1 is invalid: Self-intersection at or near point 0.9290865878767145 51.887877493086037 at 0.9290865878767145 51.887877493086037


# Try leaflet ====
epsg4326 <- leafletCRS(
  crsClass = 'L.Proj.CRS',
  code = 'EPSG:4326',
  proj4def = '+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',
  resolutions = 2^(16:7))


cols <- c('#800000', '#006622')
labels1 <- sprintf(
  '<strong>%s</strong><br/>Remain: %s (%.0f%%)<br/>Leave: %s (%.0f%%)',
  carto1$lad16nm,
  prettyNum(carto1$Remain, big.mark = ','), 100 * carto1$Remain / carto1$Valid_Votes,
  prettyNum(carto1$Leave, big.mark = ','), 100 * carto1$Leave / carto1$Valid_Votes
) %>% lapply(htmltools::HTML)

leaflet(carto1, options = leafletOptions(crs = epsg4326)) %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    label = labels1,
    opacity = 1.0, fillOpacity = 0.5, fillColor = cols[as.factor(carto1$Result)],
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))



# Try leaflet with the hexagon map ====
test$Valid_Votes_sq <- test$Valid_Votes ^ 2
carto_x <- cartogram(test, 'Valid_Votes_sq')
tmp <- data.frame(
  remain = carto_x$Remain,
  leave = carto_x$Leave,
  tot = carto_x$Remain + carto_x$Leave,
  valid = carto_x$Valid_Votes) %>%
  mutate(ratio = tot / valid - 1)
carto_x$fill_op <- abs(carto_x$Pct_Remain - 50) / max(abs(carto_x$Pct_Remain - 50))
labels2 <- sprintf(
  '<strong>%s</strong><br/>Remain: %s (%.0f%%)<br/>Leave: %s (%.0f%%)',
  carto$LAD12NM,
  prettyNum(carto$Remain, big.mark = ','), 100 * carto$Remain / carto$Valid_Votes,
  prettyNum(carto$Leave, big.mark = ','), 100 * carto$Leave / carto$Valid_Votes
) %>% lapply(htmltools::HTML)

leaflet(carto_x, options = leafletOptions(crs = epsg4326)) %>%
  addPolygons(
    color = "#444444", weight = 1, smoothFactor = 0.5,
    label = labels2,
    opacity = 1.0, fillOpacity = carto_x$fill_op, fillColor = cols[as.factor(carto_x$Result)],
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))