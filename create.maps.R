library(rgdal)
library(RColorBrewer)
library(dplyr)

## Read in Precicnt Shape File
setwd("~/Documents/nypdPrecincts/")
precincts.2016 <- readOGR(dsn=getwd(), layer="nypp")

## Read in sqf data
sqf <- readRDS("~/Documents/d3Demo/sqfTotal.rds")

## Read in populations by precinct
pct.pop <- read.csv("~/Documents/d3Demo/precinct.population.csv")


## Use 2015 population data for 2016
pct.pop.03.16 <- pct.pop[,c(1,5:17,17)]
names(pct.pop.03.16)[15] <- "X2016"

## Create stop rate for each precinct and add color coding
sqf.pct.year <- table(sqf$pct, sqf$year)[1:77,]

sqf.pct.year.rate <- cbind("Precinct"=pct.pop.03.16$Precinct, sqf.pct.year/pct.pop.03.16[,-1]*100000)

sqf.pct.cuts <- as.data.frame(cbind("Precinct"=sqf.pct.year.rate$Precinct,"avgRate"=apply(sqf.pct.year.rate[,-1],1,mean,na.rm=TRUE)))
sqf.pct.cuts$avgRate[which(sqf.pct.cuts$Precinct == 22)] <- NA

colors <- brewer.pal(5, "Reds")
cuts <- quantile(sqf.pct.cuts$avgRate,probs=seq(0,1,.2),na.rm=TRUE)

## Group precicnts into five color-coded groups
sqf.pct.cuts$group <- NA
sqf.pct.cuts$group[which(sqf.pct.cuts$avgRate < cuts[2])] <- colors[1]
sqf.pct.cuts$group[which(sqf.pct.cuts$avgRate >= cuts[2] & sqf.pct.cuts$avgRate < cuts[3])] <- colors[2]
sqf.pct.cuts$group[which(sqf.pct.cuts$avgRate >= cuts[3] & sqf.pct.cuts$avgRate < cuts[4])] <- colors[3]
sqf.pct.cuts$group[which(sqf.pct.cuts$avgRate >= cuts[4] & sqf.pct.cuts$avgRate < cuts[5])] <- colors[4]
sqf.pct.cuts$group[which(sqf.pct.cuts$avgRate >= cuts[5])] <- colors[5]

pct.colors <- sqf.pct.cuts[,-c(2)]

names(pct.colors) <- c("Precinct","Color")

pct.colors$Precinct <- as.numeric(pct.colors$Precinct)

precincts.2016@data <- join(precincts.2016@data, pct.colors, by="Precinct")

subdat<-spTransform(precincts.2016, CRS("+init=epsg:4326"))

## Write out geojson with precincts and corresponding colors
subdat_data<-subdat@data[,c("Precinct", "Color")]
row.names(subdat_data) <- row.names(subdat)
subdat<-SpatialPolygonsDataFrame(subdat, data=subdat_data)
mapdat<- "/Users/quinnhood/Documents/d3Demo/precincts.colors.geojson"
writeOGR(subdat, mapdat, layer="", driver="GeoJSON")

## Create file for each precinct,year with stop coordinates


for (y in 2006:2016) {
    ydfname <- paste0("sqf",y)
    ydf <- eval(parse(text=ydfname))
    ydf <- ydf[complete.cases(ydf$xcoord),]

    print(ydfname)

    ## Clean date variable
    ## Add leading zero to months 1-9
    if (y != 2006) {
        ydf$datestop <- ifelse(nchar(as.character(ydf$datestop))==7,paste0("0",ydf$datestop),
                           ydf$datestop)

        ydf$date <- paste0(substr(ydf$date,5,8),
                           substr(ydf$date,1,2),
                           substr(ydf$date,3,4))
    } else {
        ydf$date <- paste0(substr(ydf$date,1,4),
                           substr(ydf$date,6,7),
                           substr(ydf$date,9,10))
    }
    ## SQF x,y correspond to state plane coordinates NYC-Long Island (espg:2263)
    coors <- SpatialPoints(cbind(ydf$xcoord,ydf$ycoord),
                           proj4string=CRS("+init=epsg:2263"))

    ## Convert to lat,long
    longlatcoor<-spTransform(coors,CRS("+proj=longlat"))



    ## Create dataframe with date of stop and precinct to be merged
    ## Other - Yellow
    ydf$fill <- "#ffff33"
    ## Black - Blue
    ydf$fill[which(trimws(ydf$race) == "B")] <- "#377eb8"
    ## White - Green
    ydf$fill[which(trimws(ydf$race) == "W")] <- "#4daf4a"
    ## Asian - Orange
    ydf$fill[which(trimws(ydf$race) == "A")] <- "#ff7f00"
    ## Hispanic (Black or White) - Purple
    ydf$fill[which(trimws(ydf$race) == "P" | trimws(ydf$race) == "Q")] <- "#984ea3"

    df <- data.frame("Date"=ydf$date,"Precinct"=ydf$pct,"Fill"=ydf$fill)

    ## Merge data and coordinates
    spdf <- SpatialPointsDataFrame(longlatcoor,df)

    for (p in levels(factor(sqftest$pct))) {
        df <- spdf[which(spdf$Precinct == p),]
        path <- paste("/Users/quinnhood/Documents/d3Demo/Data/points",p,y,"geojson",sep=".")
        writeOGR(df, path, layer="", driver="GeoJSON")
    }
}







