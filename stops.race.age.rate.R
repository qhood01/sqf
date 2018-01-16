census <- read.csv("~/Downloads/nycTotal.csv")

census.black <- census[c(14:26,26),c(1,grep("Black",names(census)))]
census.black <- census.black[,c(1,121:169,18:66)]
names <- unlist(lapply(names(census.black), function(x) substr(x,8,nchar(x))))
names(census.black) <- names

census.white <- census[c(14:26,26),c(1,grep("White",names(census)))]
census.white <- census.white[,c(1,121:169,18:66)]
names(census.white) <- names

census.hisp <- census[c(14:26,26),c(1,grep("Hisp",names(census)))]
census.hisp <- census.hisp[,c(1,121:169,18:66)]
names(census.hisp) <- names


write.csv(census.black,"~/Documents/d3Demo/census.black.csv",row.names=FALSE)
write.csv(census.white,"~/Documents/d3Demo/census.white.csv",row.names=FALSE)
write.csv(census.hisp,"~/Documents/d3Demo/census.hisp.csv",row.names=FALSE)

sqf$sex <- trimws(sqf$sex)
sqf$race <- trimws(sqf$race)
sqf$age <- as.numeric(trimws(sqf$age))

sqf$sex.age <-paste0(sqf$sex,sqf$age)

sqf.mf <- sqf[which(sqf$sex == "M" | sqf$sex == "F"),]
sqf.mf.16.64 <- sqf.mf[which(sqf.mf$age >= 16 & sqf.mf$age <= 64),]

sqf.mf.16.64.black <- sqf.mf.16.64[which(sqf.mf.16.64$race == "B"),]
sqf.mf.16.64.white <- sqf.mf.16.64[which(sqf.mf.16.64$race == "W"),]
sqf.mf.16.64.hisp <- sqf.mf.16.64[which(sqf.mf.16.64$race == "P" | sqf.mf.16.64$race == "Q"),]

stops.black <- as.data.frame.matrix(table(sqf.mf.16.64.black$year, sqf.mf.16.64.black$sex.age))
stops.hisp <- as.data.frame.matrix(table(sqf.mf.16.64.hisp$year, sqf.mf.16.64.hisp$sex.age))
stops.white <- as.data.frame.matrix(table(sqf.mf.16.64.white$year, sqf.mf.16.64.white$sex.age))

write.csv(stops.black,"~/Documents/d3Demo/stops.black.csv")
write.csv(stops.white,"~/Documents/d3Demo/stops.white.csv")
write.csv(stops.hisp,"~/Documents/d3Demo/stops.hisp.csv")
