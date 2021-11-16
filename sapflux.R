
#### libraries ----
library(lubridate)
library(ggplot2)
library(dplyr)

#### data directory ----
userNumber <- 3
#sapflow and sensor data parent directory
dirData <- c("E:/Google Drive/research/projects/campus/buckthorn/sapflux",#windows office
             "/Users/hkropp/Google Drive/research/projects/campus/buckthorn/sapflux", # teaching mac
             "/Users/hazelschrader/Documents/Buckthorn-data-sapflux/sapflux") #hazels mac
dirWeather <- c("E:/Google Drive/research/projects/Data/campus_weather/METER/",
                "/Users/hkropp/Google Drive/research/projects/Data/campus_weather/METER/",
                "/Users/hazelschrader/Documents/Buckthorn-data-sapflux/weather") #hazels mac
#sapflow download date for file
sversion <- "09_24_2021"


#### read in data ----
#dT sapflow
sapRaw <- read.csv(paste0(dirData[userNumber],"/campbell/",sversion,"/Sapflow_TableDT.dat"),
                    header=FALSE,skip=4,na.strings=c("NAN"))
sapInfo <- read.csv(paste0(dirData[userNumber],"/campbell/",sversion,"/Sapflow_TableTC.dat"),
                   header=FALSE,skip=4,na.strings=c("NAN"))

#weather data
weather <- read.csv(paste0(dirWeather[userNumber],"/12_z6-10463 12Oct21.csv"),
                    skip=3, header=FALSE)
weatherNames <- read.csv(paste0(dirWeather[userNumber],"/12_z6-10463 12Oct21.csv"),
                         nrows=3, header=FALSE)
#green ash allometry
greenwood <- read.csv(paste0(dirData[userNumber],"/green ash olson paper measurements.csv"))
#sapwood allometry
buckthornSW <- read.csv(paste0(dirData[userNumber],"/buckthorn_allometry_info.csv"))
#SLA
buckthornSLA <- read.csv(paste0(dirData[userNumber],"/leaf area.csv"))
#buchthorn dbh and leaf allom
buckthornLA <- read.csv(paste0(dirData[userNumber],"/buckthorn_leaf_allom.csv"))
#list of buckthorn removed with dbh
buckthornRemove <- read.csv(paste0(dirData[userNumber],"/buckthorn_dbh.csv"))
#read sensor info
sensors <- read.csv(paste0(dirData[userNumber],"/sensors_meta.csv"))

#### organize sap flow ---
heaterv <- data.frame(date =  ymd_hms(sapInfo[,1]),
                      ht1 = sapInfo[,165],
                      ht2 = sapInfo[,166])
heaterv$year <- year(heaterv$date)
heaterv$doy <- yday(heaterv$date)
heaterv$hour <- hour(heaterv$date)+(minute(heaterv$date)/60)

#remove unused sensor locations
datSap <- sapRaw[,1:18]
#rename columns
colnames(datSap ) <- c("date","record",paste0("dT",seq(1,16)))
#parse date
datSap$dateF <- ymd_hms(datSap$date)
datSap$year <- year(datSap$dateF)
datSap$doy <- yday(datSap$dateF)
datSap$hour <- hour(datSap$dateF)+(minute(datSap$dateF)/60)
datSap$DD <- datSap$doy + (datSap$hour/24)



#### initial plots ----
#buckthorn sapwood allometry
#dbh vs sapwood
plot(buckthornSW$DBH.cm, buckthornSW$Sapwood.mm/10, pch=19)
#linear reg sap
bsap.lm<-lm(buckthornSW$Sapwood.mm/10~buckthornSW$DBH.cm)
summary(bsap.lm)
bsap.calc<-mean(buckthornSW$Sapwood.mm/10)
#not significant relationship

#dbh vs bark thickness
plot(buckthornSW$DBH.cm, buckthornSW$bark.mm, pch=19)
bbark.lm<-lm(buckthornSW$bark.mm/10~buckthornSW$DBH.cm)
summary(bbark.lm)
#bark relationship not significant, assume mean
bbark.calc<-mean(buckthornSW$bark.mm/10)


#add sapwood to sensors- used a lot
#ash allometry from Zeima Kassahun, Heidi J. Renninger 2021 Ag & Forest Met
sensors$sd.cm <- ifelse(sensors$Type == "Ash", #if sensors is ash
                        -36.33 + (44.28*(1-exp(-0.1306*sensors$DBH.cm))),#allometry
                        bsap.calc)#if buckthorn fill place with 1 cm placeholder until allometry is fully measured
#allometry from greenwood
greenwood
#organize data for easier calculations
tabledt <- datSap


dtAll <- data.frame(date= rep(tabledt$date, times = 16), 
                    doy = rep(tabledt$doy, times = 16),
                    hour = rep(tabledt$hour, times = 16),
                    DD = rep(tabledt$DD, times = 16),
                    sensor = rep(seq(1,16), each = nrow(tabledt)), 
                    dT = c(tabledt[,3],
                           tabledt[,4],
                           tabledt[,5],
                           tabledt[,6],
                           tabledt[,7],
                           tabledt[,8],
                           tabledt[,9],
                           tabledt[,10],
                           tabledt[,11],
                           tabledt[,12],
                           tabledt[,13],
                           tabledt[,14],
                           tabledt[,15],
                           tabledt[,16],
                           tabledt[,17],
                           tabledt[,18]))

#filter out unreliable data due to voltage regulator issues

#moved heaters to more reliable first regulator on july 2 10-10:30 and replaced reculator on July 6 10 am
ggplot(heaterv, aes(x=date,y=ht1))+ 
  geom_point()+
  geom_line()

ggplot(heaterv, aes(x=date,y=ht2))+ 
  geom_point()+
  geom_line()

#indicate which heater
dtAll$htrN <- ifelse(dtAll$sensor <= 8,1,
                     ifelse(dtAll$doy < 183 | dtAll$doy > 187, 2,1))
heaterv[which(heaterv$ht1 == 0),]

#calculate daily heater sd
heater1sd <- aggregate(heaterv$ht1, by=list(doy=heaterv$doy),FUN="sd")
heater2sd <- aggregate(heaterv$ht2, by=list(doy=heaterv$doy),FUN="sd")
heater1min <- aggregate(heaterv$ht1, by=list(doy=heaterv$doy),FUN="min")
heater2min <- aggregate(heaterv$ht2, by=list(doy=heaterv$doy),FUN="min")

heatersAll <- data.frame(doy= c(heater1sd$doy,heater2sd$doy),
                         htrN=c(rep(1,nrow(heater1sd)),rep(2,nrow(heater2sd))),
                         sd = c(heater1sd$x,heater2sd$x),
                         min=c(heater1min$x,heater2min$x))

ggplot(heatersAll, aes(doy, sd, col=htrN))+
  geom_point()


#join heater info back into dt
#56448
dtAll <- left_join(dtAll,heatersAll, by=c("doy","htrN"))


#################
#filter out days when voltage regulator was unreliable
#either too variable or heaters turned off at any point
dtAll <- dtAll[dtAll$sd <= 0.05 & dtAll$min >0,]


#################
#check for dt outliers
quantile(dtAll$dT, prob=seq(0,1,by=0.001))
#definitely few outliers. 99.5% and above are unusually high
dtAll <- dtAll[dtAll$dT <= quantile(dtAll$dT, prob=0.995),]

#join sensor info into table dt
#make a doy that contains the same night
#so new day actually starts at 5 am not midnight
dtAll$doy5 <- ifelse(dtAll$hour < 5, dtAll$doy-1,dtAll$doy)

night <- dtAll[dtAll$hour < 5|dtAll$hour >= 22,]

#filter night so maximum in day and sensor is provided
maxnight1 <- night %>% 
  group_by(sensor, doy5) %>%
  filter(dT == max(dT),na.rm=TRUE)
#remove duplicate maximums that occur for longer than 15 min
#just take earliest measurement
maxnight <- maxnight1  %>% 
  group_by(sensor, doy5) %>%
  filter(hour == min(hour),na.rm=TRUE)

ggplot(maxnight, aes(doy5,dT, color=sensor))+
  geom_point()
#isolate max and join back into table
maxJoin <- data.frame(sensor=maxnight$sensor, 
                      doy5=maxnight$doy5,
                      maxDT = maxnight$dT)

#join backinto tabledt
dtCalct1 <- left_join(dtAll, maxJoin, by=c("sensor","doy5"))
#join sensor info
dtCalc <- left_join(dtCalct1 , sensors, by=c("sensor"="SensorID"))

#from clearwater

#sap velocity m s-1 (v)
#v = 0.000119*k^1.231
#flow is F (L s-1) = v* A (m2, sapwood area)

#K= (dTmax - dT)/dT if sensor is fully within sapwood

#otherwise correction is:
#dt sap = (dT - b* Dtmax)/a

#a = proportion of probe in sapwood and b=1-a

dtCalc$a <- ifelse(dtCalc$sd.cm >= 3,1,
                   dtCalc$sd.cm/3)

dtCalc$b <- 1 - dtCalc$a 

dtCalc$dTCor <- (dtCalc$dT - (dtCalc$b * dtCalc$maxDT))/dtCalc$a
dtCalc$K <- (dtCalc$maxDT - dtCalc$dTCor)/dtCalc$dTCor
dtCalc$velo <- 0.000119*(dtCalc$K^1.231)


#seperate types
ash <- dtCalc[dtCalc$Type == "Ash",]
buckthorn <- dtCalc[dtCalc$Type == "Buckthorn",]


#############
#compare N & S sensors for ash
sens3 <- data.frame(date = ash$date[ash$sensor == 3],
                    veloN = ash$velo[ash$sensor == 3])
                     
sens4 <- data.frame(date = ash$date[ash$sensor == 4],
                    veloS = ash$velo[ash$sensor == 4])

treeD1 <- inner_join(sens3,sens4, by="date")

#compare N & S sensors for ash
sens12 <- data.frame(date = ash$date[ash$sensor == 12],
                    veloN = ash$velo[ash$sensor == 12])

sens11 <- data.frame(date = ash$date[ash$sensor == 11],
                    veloS = ash$velo[ash$sensor == 11])

treeD2 <- inner_join(sens12,sens11, by="date")

sens15 <- data.frame(date = ash$date[ash$sensor == 15],
                     veloN = ash$velo[ash$sensor == 15])

sens16 <- data.frame(date = ash$date[ash$sensor == 16],
                     veloS = ash$velo[ash$sensor == 16])

treeD3 <- inner_join(sens15,sens16, by="date")

treeDir <- rbind(treeD1,treeD2,treeD3)
#check relationship
azim.rel <- lm(treeDir$veloS ~ treeDir$veloN)
summary(azim.rel)

ggplot(treeDir, aes(veloN,veloS))+
  geom_point()+
  geom_abline()

#regression does not differ significantly from S=0 + 1*N

ash.tree <- ash[ash$Direction == "N", ]



###############
#Ash allometry from literature
greenwood$sap.area <- greenwood$Sapwood.Volume.ft3/greenwood$Height.feet
#30.48 cm in 1 foot
greenwood$sap.area.cm <- 30.48*30.48*greenwood$sap.area 
greenwood$dbh.cm <- (greenwood$DBH.in*2.54)
greenwood$treeArea <- ((greenwood$dbh.cm /2)^2)*pi

plot(greenwood$dbh.cm,greenwood$sap.area.cm)

saparea.reg <- lm(greenwood$sap.area.cm ~ greenwood$dbh.cm)
summary(saparea.reg)
#sap cm2 = -9.6 + 8.854*DBH cm
#meadows paper
#LA (m2) = -66.185 +  6.579*DBH in cm

#### tree calculations ----

ash.tree$sap.areacm2 <- -9.6 + 8.854*ash.tree$DBH.cm
#convert sap area to m2
ash.tree$sap.aream2 <- 0.0001*ash.tree$sap.areacm2

ash.tree$LA.m2 <- -66.185 +  6.579*ash.tree$DBH.cm
range(ash.tree$LA.m2)

#flow rate according to clearwater
#F(L s-1) =  v(m s-1)* A (m2)

ash.tree$Flow.m3.s <- ash.tree$velo * ash.tree$sap.aream2

ash.tree$Flow.L.s <- ash.tree$Flow.m3.s * 1000

ash.tree$Flow.L.m2.s <- ash.tree$Flow.L.s /ash.tree$LA.m2 

#summarize total per day for each tree
#remove NA
ash.treeNN <- ash.tree[is.na(ash.tree$Flow.L.s)==FALSE,]
#calculate total water use by each tree in a day
#total liters used in 15 min period
ash.treeNN$L.p <- ash.treeNN$Flow.L.s* 60 *15
#normalized by leaf area

#sum up for each tree and day
ash.L.day <- ash.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p), n=length(L.p))

ash.treeNN$L.p.m2  <- ash.treeNN$L.p/ash.treeNN$LA.m2 

ash.L.m.day <- ash.treeNN %>%
  group_by(doy, Removal, sensor) %>%
  summarise(sum = sum(L.p.m2 ), n=length(L.p.m2))

#summary table
#flow L s every 15 min by treatment
Flow.exp <- ash.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.s),sd=sd(Flow.L.s), n=length(Flow.L.s))
#flow L m-2 leaf s-1 by 15min
Flow.m2.exp <- ash.treeNN %>%
  group_by(doy, hour, DD, Removal) %>%
  summarise(mean = mean(Flow.L.m2.s),sd=sd(Flow.L.m2.s), n=length(Flow.L.m2.s))
#only use time points with at least 3 trees
Flow.exp <- Flow.exp[ Flow.exp$n >=3,]

#total liters per day used by the tree
ash.L.day <- ash.L.day[ ash.L.day$n == 96,]

L.day.exp <- ash.L.day %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
L.day.exp <- L.day.exp[L.day.exp$n >= 3,]
#total liters per day per m2 leaf area
ash.L.m.day <- ash.L.m.day[ ash.L.m.day$n == 96,]

L.day.m2.exp <- ash.L.m.day %>%
  group_by(doy, Removal) %>%
  summarise(mean = mean(sum),sd=sd(sum), n=length(sum))
L.day.m2.exp <- L.day.m2.exp[L.day.m2.exp$n >= 3,]



ggplot(Flow.exp, aes(DD,mean, col=Removal))+
  geom_line()+
  geom_point()

ggplot(Flow.m2.exp[Flow.m2.exp$doy >= 187,], aes(DD,mean, col=Removal))+
  geom_line(alpha=0.25)+
  geom_point(alpha=0.25)+
  theme_classic()+
  geom_errorbar(aes(ymin=mean-((sd/sqrt(n))*2), ymax=mean+((sd/sqrt(n))*2)), alpha=0.5) 
  


ggplot(L.day.m2.exp, aes(doy,mean, col=Removal))+
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) 



ggplot(L.day.exp, aes(doy,mean, col=Removal))+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) 

################
#To do

#need sapwood area allometry from paper
#calculate sapflow in volume per time
#filter out days with voltage regulator down


ggplot(dtCalc[dtCalc$sensor ==1,], aes(x=DD,y=velo))+ 
  geom_point()

ggplot(dtCalc[dtCalc$sensor ==2,], aes(x=DD,y=velo))+ 
  geom_point()

ggplot(dtCalc[dtCalc$sensor ==3,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==4,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==5,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==6,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==7,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==8,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==9,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==10,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==11,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==12,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==13,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==14,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==15,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==16,], aes(x=DD,y=velo))+ 
  geom_point()+
  geom_line()




ggplot(dtCalc[dtCalc$sensor ==1,], aes(x=DD,y=dT))+ 
  geom_point()

ggplot(dtCalc[dtCalc$sensor ==2,], aes(x=DD,y=dT))+ 
  geom_point()

ggplot(dtCalc[dtCalc$sensor ==3,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==4,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==5,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==6,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==7,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==8,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==9,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==10,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==11,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==12,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==13,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==14,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==15,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()
ggplot(dtCalc[dtCalc$sensor ==16,], aes(x=DD,y=dT))+ 
  geom_point()+
  geom_line()

