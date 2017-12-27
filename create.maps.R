library(rgdal)
library(RColorBrewer)
library(dplyr)

setwd("~/Documents/nypdPrecincts/")
precincts.2016 <- readOGR(dsn=getwd(), layer="nypp")

sqf <- readRDS("~/Documents/d3Demo/sqfTotal.rds")

sqf.pct.cuts <- count(sqf, "pct")
sqf.pct.cuts <- sqf.pct.cuts[1:77,]

colors <- brewer.pal(5, "Reds")
cuts <- quantile(table(sqf$pct),probs=seq(0,1,.2))

sqf.pct.cuts$group <- NA
sqf.pct.cuts$group[which(sqf.pct.cuts$freq < cuts[2])] <- colors[1]
sqf.pct.cuts$group[which(sqf.pct.cuts$freq >= cuts[2] & sqf.pct.cuts$freq < cuts[3])] <- colors[2]
sqf.pct.cuts$group[which(sqf.pct.cuts$freq >= cuts[3] & sqf.pct.cuts$freq < cuts[4])] <- colors[3]
sqf.pct.cuts$group[which(sqf.pct.cuts$freq >= cuts[4] & sqf.pct.cuts$freq < cuts[5])] <- colors[4]
sqf.pct.cuts$group[which(sqf.pct.cuts$freq >= cuts[5])] <- colors[5]

pct.colors <- sqf.pct.cuts[,-c(2)]

names(pct.colors) <- c("Precinct","Color")

pct.colors$Precinct <- as.numeric(pct.colors$Precinct)

precincts.2016@data <- join(precincts.2016@data, pct.colors, by="Precinct")

subdat<-spTransform(precincts.2016, CRS("+init=epsg:4326"))

subdat_data<-subdat@data[,c("Precinct", "Color")]
row.names(subdat_data) <- row.names(subdat)
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
mapdat<- "~/Documents/d3Demo/precincts.colors.geojson"
writeOGR(subdat, mapdat, layer="", driver="GeoJSON")
