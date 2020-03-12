library(PerformanceAnalytics)
library(magrittr)
library(dplyr)
rm(list = ls())

####Chick Growth plot by year############################################################################################################################
#########Merge chick growth files#############################################################################################################################
#old chick growth files
#cg2010 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2010.csv")
#cg2011 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2011.csv")
#cg2012 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2012.csv")
#cg2013 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2013.csv")
#cg2014 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2014.csv")
#cg2015 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2015.csv")
#cg2016 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2016.csv")
#cg2017 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2017.csv")
#cg2018 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_ChickGrowth_2018.csv")

band2010 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2010.csv")
band2011 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2011.csv")
band2012 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2012.csv")
band2013 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2013.csv")
band2014 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2014.csv")
band2015 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2015.csv")
band2016 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2016.csv")
band2017 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2017.csv")
band2018 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2018.csv")
band2019 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2019.csv")

data.allband <- rbind(band2010, band2011, band2012, band2013, band2014,  band2015, band2016, band2017, band2018, band2019)

data.allband$Disposition <- as.factor(data.allband$Disposition)
data.allband$MetalOut <- as.factor(data.allband$MetalOut)
data.allband$AgeID <- as.factor(data.allband$AgeID)

data.juv <- as.data.frame(data.allband %>%
  filter(data.allband$AgeID == "L" | data.allband$AgeID == "HY"))

nls.Gompertz.growth.fit = nls(Weight ~ A*exp(-exp(-K*(ChickAge-T))), data.juv, start=list(A=90, T=10, K=0.2))
#A	=	coef(nls.Gompertz.growth.fit)[1]
#T	=	coef(nls.Gompertz.growth.fit)[2]
#K	=	coef(nls.Gompertz.growth.fit)[3]
#predict.age=function(P10.total) -(1/K)*log(-log(P10.total/A))+T
plot(Weight~ChickAge, data.juv, type='n', xlab='Predicted Age (d)', ylab='Body Mass (g)', xlim=c(0,30))
#points(Weight~Age, subset(data.juvs, Year==2010), col=1)
#points(Weight~Age, subset(data.juvs, Year==2011), col=2)
points(Weight~ChickAge, subset(data.juv, Year==2012), col=1, bg=3, pch=21)
points(Weight~ChickAge, subset(data.juv, Year==2014), col=1, bg=4, pch=21)
points(Weight~ChickAge, subset(data.juv, Year==2015), col=1, bg=5, pch=21)
points(Weight~ChickAge, subset(data.juv, Year==2016), col=1, bg=6, pch=21)
points(Weight~ChickAge, subset(data.juv, Year==2017), col=1, bg=9, pch=21)
points(Weight~ChickAge, subset(data.juv, Year==2018), col=1, bg=2, pch=21)
points(Weight~ChickAge, subset(data.juv, Year==2019), col=1, bg=7, pch=21)
legend('topleft', pch=c(21,21,21,21), col=c(1,1,1,1), pt.bg=c(3,4,5,6,9,2,7), c('2012','2014','2015','2016',"2017", "2018", "2019"))

#fit year-specific logistic growth curves:
#nls.growth.fit.2010=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juvs, Year==2010))
#nls.growth.fit.2011=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juvs, Year==2011))
nls.growth.fit.2012=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juv, Year==2012))
nls.growth.fit.2014=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juv, Year==2014))
nls.growth.fit.2015=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juv, Year==2015))
nls.growth.fit.2016=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juv, Year==2016))
nls.growth.fit.2017=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juv, Year==2017))
nls.growth.fit.2018=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juv, Year==2018))
nls.growth.fit.2019=nls(Weight ~ SSlogis(ChickAge, Asym, xmid, scal), subset(data.juv, Year==2019))
#plot the fitted growth curve:
x=seq(0,35)
#y.2010=predict(nls.growth.fit.2010,list(ChickAge=x))
#y.2011=predict(nls.growth.fit.2011,list(ChickAge=x))
y.2012=predict(nls.growth.fit.2012,list(ChickAge=x))
y.2014=predict(nls.growth.fit.2014,list(ChickAge=x))
y.2015=predict(nls.growth.fit.2015,list(ChickAge=x))
y.2016=predict(nls.growth.fit.2016,list(ChickAge=x))
y.2017=predict(nls.growth.fit.2017,list(ChickAge=x))
y.2018=predict(nls.growth.fit.2018,list(ChickAge=x))
y.2019=predict(nls.growth.fit.2019,list(ChickAge=x))
#lines(y.2010~x, col=1)
#lines(y.2011~x, col=2)
lines(y.2012~x, col=3, lwd=2)
lines(y.2014~x, col=4, lwd=2)
lines(y.2015~x, col=5, lwd=2)
lines(y.2016~x, col=6, lwd=2)
lines(y.2017~x, col=9, lwd=2)
lines(y.2018~x, col=2, lwd=2)
lines(y.2019~x, col=7, lwd=2)
#####Mass at 25 Days and Median hatch Date plot################################################################################################################
given.age=25
mass.at.given.age.2018=predict(nls.growth.fit.2018,list(Age=given.age))
mass.at.given.age.2017=predict(nls.growth.fit.2017,list(Age=given.age))
mass.at.given.age.2016=predict(nls.growth.fit.2016,list(Age=given.age))
mass.at.given.age.2015=predict(nls.growth.fit.2015,list(Age=given.age))
mass.at.given.age.2014=predict(nls.growth.fit.2014,list(Age=given.age))
mass.at.given.age.2012=predict(nls.growth.fit.2012,list(Age=given.age))
ymhw <- read.csv("ymhw.csv")
plot(x = ymhw$median,y = ymhw$mass, type='n', xlim=c(12,27), ylim=c(92,107), xlab='Median Hatch Date', ylab='Chick Body Mass (g) at Age = 25 days')
points(x = ymhw$median, y = ymhw$mass, cex=7, pch=21, bg= ifelse(ymhw$mass > 100, "#6495ED", "#FF7F50"))
text(x = ymhw$median, y = ymhw$mass, labels=ymhw$year)
legend('topleft', pch=c(21,21), col=c(1,1), cex = 1.4, pt.bg=c("#6495ED","#FF7F50"), c("Late Year", "Early Year"))
#######Write csv
setwd("T:/Pohlen/Red_Kont/REKN_Datafiles/Test_Files")
write.csv(data.juv, "Allchickgrowth8.1.2019.csv", na = "")


#######Merge all years banding data#########################################################################################################################
setwd("T:/Landbirds/Red_Knot/Breeding/Data")

band2010 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2010.csv")
band2011 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2011.csv")
band2012 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2012.csv")
band2013 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2013.csv")
band2014 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2014.csv")
band2015 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2015.csv")
band2016 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2016.csv")
band2017 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2017.csv")
band2018 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2018.csv")
band2019 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2019.csv")

allband <- rbind(band2010, band2011, band2012, band2013, band2014,  band2015, band2016, band2017, band2018, band2019)
allband$Disposition <- as.factor(allband$Disposition)
allband$MetalOut <- as.factor(allband$MetalOut)
allband$AgeID <- as.factor(allband$AgeID)
adultsperyear <- allband %>%
  filter(AgeID == "AHY" |
           AgeID == "ASY",
         Disposition == "1",
         Location == "NOME")
newadults <- adultsperyear %>%
  group_by(Year) %>%
  summarise(total_newadults = n_distinct(FlagCodeOut))
juvsperyear <- allband %>%
  filter(AgeID == "L" |
           AgeID == "HY",
         Disposition == "1",
         Location == "NOME")
newjuvs <- juvsperyear %>%
  group_by(Year) %>%
  summarise(total_newjuvs = n_distinct(MetalOut))
newadults
newjuvs
mean(newjuvs$total_newjuvs)
mean(newadults$total_newadults)
#######Write csv
setwd("T:/Pohlen/Red_Kont/REKN_Datafiles/Test_Files")
write.csv(allband, "Allbanding8.1.2019.csv", na = "")


#####Merge Encounter files ################################################################################################################################
setwd("T:/Landbirds/Red_Knot/Breeding/Data")

enc2010 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2010.csv")
enc2011 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2011.csv")
enc2012 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2012.csv")
enc2013 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2013.csv")
enc2014 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2014.csv")
enc2015 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2015.csv")
enc2016 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2016.csv")
enc2017 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2017.csv")
enc2018 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2018.csv")
enc2019 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Encounters_2019.csv")

allenc <- rbind(enc2010, enc2011, enc2012, enc2013, enc2014, enc2015, enc2016, enc2017, enc2018, enc2019)
encsummary <- allenc%>%
  group_by(Year) %>%
  filter(PlotID == "34 MI" |PlotID == "37 MI" |PlotID == "38 MI" |PlotID == "9 MI" |
           PlotID == "Barrel" |PlotID == "Feather" |PlotID == "Feather East" |PlotID == "Feather Steven" |
           PlotID == "Feather West" |PlotID == "Knob" |PlotID == "Safety" |PlotID == "Steven" |
           PlotID == "Tombstone") %>%
  summarise(unique_encounters = n_distinct(CurrentFlagCode))
encsummary
#######Write csv
setwd("T:/Pohlen/Red_Kont/REKN_Datafiles/Test_Files")
write.csv(allenc, "Allencounters8.1.2019.csv", na = "")


#####Merge Brood files####################################################################################################
setwd("T:/Landbirds/Red_Knot/Breeding/Data")

#bro2010 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2010.csv")
bro2011 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2011.csv")
bro2012 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2012.csv")
bro2013 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2013.csv")
bro2014 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2014.csv")
bro2015 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2015.csv")
bro2016 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2016.csv")
bro2017 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2017.csv")
bro2018 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2018.csv")
bro2019 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Brood_2019.csv")

allbrood <- rbind(bro2011, bro2012, bro2013, bro2014, bro2015, bro2016, bro2017, bro2018, bro2019)
broodsperyear <- allbrood %>%
  group_by(Year) %>%
  summarise(total_broods = n_distinct(BroodID))
broodsperyear
setwd("T:/Pohlen/Red_Kont/REKN_Datafiles/Test_Files")
write.csv(allbrood, "Allbroods8.1.2019.csv", na = "")
