

#######################################################################################################################################
####################         LOAD DATA                                                                 ################################
#######################################################################################################################################


##read in banding data from Nome in each year file
band2010 <- read.csv("data/R7MBMlb_REKN_Banding_2010.csv", stringsAsFactors = FALSE)
band2011 <- read.csv("data/R7MBMlb_REKN_Banding_2011.csv", stringsAsFactors = FALSE)
band2012 <- read.csv("data/R7MBMlb_REKN_Banding_2012.csv", stringsAsFactors = FALSE)
band2013 <- read.csv("data/R7MBMlb_REKN_Banding_2013.csv", stringsAsFactors = FALSE)
band2014 <- read.csv("data/R7MBMlb_REKN_Banding_2014.csv", stringsAsFactors = FALSE)
band2015 <- read.csv("data/R7MBMlb_REKN_Banding_2015.csv", stringsAsFactors = FALSE)
band2016 <- read.csv("data/R7MBMlb_REKN_Banding_2016.csv", stringsAsFactors = FALSE)
band2017 <- read.csv("data/R7MBMlb_REKN_Banding_2017.csv", stringsAsFactors = FALSE)
band2018 <- read.csv("data/R7MBMlb_REKN_Banding_2018.csv", stringsAsFactors = FALSE)
band2019 <- read.csv("data/R7MBMlb_REKN_Banding_2019.csv", stringsAsFactors = FALSE)
##combine all banding files from Nome
allband <- rbind(band2019, band2018, band2017, band2016, band2015, band2014, band2013, band2012, band2011, band2010)

##read in banding data from Gray's Harbor for 2017 and 2018
GHband <- read.csv("data/R7MBMlb_GH_banding.csv", stringsAsFactors = TRUE)

##read in banding data from Wrangel Island 
dfWR <- read.csv("data/Wrangel_REKN.csv", stringsAsFactors = TRUE)


#######################################################################################################################################
####################         FILTER & SUBSET                                                           ################################
#######################################################################################################################################


library(dplyr)

##NOME, ALASKA


##filter out adults and REKN in Nome
allbandadults <- allband %>% filter(
  AgeID == "AHY" | AgeID == "ASY" | AgeID == "ATY",
  Species == "REKN") %>%
  droplevels
##filter out adult REKN from Nome that have been molecularly sexed
all.sexed.adults <- allbandadults %>% filter(
  CHDSex == "M" | CHDSex == "F") %>%
  droplevels

##remove unnesecary columns and variables we did not regularly collect
df <- dplyr::select(all.sexed.adults, CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing)
##remove individuals that did not have all 4 morphometric variables measured
df <- na.omit(df)
df$CHDSex <- as.factor(df$CHDSex)

##select males from Nome and add a column for breeding site - Alaska
df.m <- df %>% filter(CHDSex == "M")
df.m$site <- "Alaska"

##select females from Nome and add a column for breeding site - Alaska
df.f <- df %>% filter(CHDSex == "F")
df.f$site <- "Alaska"


##GRAYS HARBOR, WASHINGTON


##filter out birds from Grays Harbor (all adults and all REKN) that were molecularly sexed
dfGH<- GHband %>% filter(
  CHDSex == "M" | CHDSex == "F") %>%
  droplevels
dfGH<- dplyr::select(dfGH, CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing, site.sex)
dfGH<- na.omit(dfGH)
dfGH$CHDSex <- as.factor(dfGH$CHDSex)

##select males tracked to Wrangel Island, Russia from Grays Harbor
#select universal variables 
#identify breeding site - Wrangel
dfGH.WR.m <- dfGH %>% filter(
  site.sex == "WM") %>%
  droplevels
dfGH.WR.m$site <- "T_Wrangel"

##select females tracked to Wrangel Island, Russia from Grays Harbor
#select universal variables 
#identify breeding site - Wrangel
dfGH.WR.f <- dfGH %>% filter(
  site.sex == "WF") %>%
  droplevels
dfGH.WR.f$site <- "T_Wrangel"

##select males tracked to Alaska from Grays Harbor
#select universal variables 
#identify breeding site - Alaska
dfGH.AK.m <- dfGH %>% filter(
  site.sex == "AM") %>%
  droplevels
dfGH.AK.m$site <- "T_Alaska"

##select males tracked to Alaska from Grays Harbor
#select universal variables 
#identify breeding site - Alaska
dfGH.AK.f <- dfGH %>% filter(
  site.sex == "AF") %>%
  droplevels
dfGH.AK.f$site <- "T_Alaska"


##Wrangel Island, Russia


##filter out adult REKN from Wrangel Island that have been molecularly sexed (all adults)
#select universal variables 
dfWR <- dfWR %>% filter(
  CHDSex == "M" | CHDSex == "F") %>%
  droplevels
dfWR <- dplyr::select(dfWR,CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing)
dfWR<- na.omit(dfWR)
dfWR$CHDSex <- as.factor(dfWR$CHDSex)

##select females from Wrangel Island and add breeding site - Wrangel 
dfWR.f <- dfWR %>% filter(CHDSex == "F")
dfWR.f$site <- "Wrangel"

##select males from Wrangel Island and add breeding site - Wrangel
dfWR.m <- dfWR %>% filter(CHDSex == "M")
dfWR.m$site <- "Wrangel"


##breeding site & sex combinations


##combine all birds from all three sites
dfGH <- dplyr::select(dfGH,CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing)
all.rekn <- rbind(df,dfGH, dfWR)

##all males labeled by breeding site
dfGH.WR.f <- dplyr::select(dfGH.WR.f,CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing, site)
dfGH.WR.m <- dplyr::select(dfGH.WR.m,CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing, site)
dfGH.AK.m <- dplyr::select(dfGH.AK.m,CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing, site)
dfGH.AK.f <- dplyr::select(dfGH.AK.f,CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing, site)
all.m <- rbind(dfGH.WR.m, df.m, dfWR.m, dfGH.AK.m)
##all females labeled by breeding site
all.f <- rbind(dfGH.WR.f, df.f, dfWR.f, dfGH.AK.f)

##all males breeding in Alaska
all.AK.m <- rbind(df.m, dfGH.AK.m)
all.AK.m$loc <- "AK"
##all males breeding on Wrangel Island
all.WR.m <- rbind(dfWR.m, dfGH.WR.m)
all.WR.m$loc <- "WR"

##all females breeding in Alaska
all.AK.f <- rbind(df.f, dfGH.AK.f)
all.AK.f$loc <- "AK"
##all females breeding on Wrangel Island
all.WR.f <- rbind(dfWR.f, dfGH.WR.f)
all.WR.f$loc <- "WR"


#######################################################################################################################################
####################                   CORRELATIONS                                                    ################################
#######################################################################################################################################

library(corrplot)

##define fultiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##filter Grays Harbor banding data to include mass
GHmass<- GHband %>% filter(
  CHDSex == "M" | CHDSex == "F") %>%
  droplevels
GHmass <- dplyr::select(GHmass, CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing, mass)
GHmass <- na.omit(GHmass)

##test correlation between mass and other variables
##by selecting heavier birds, where we selecting larger variables? - YES
correlationsMASS <- cor(GHmass[,2:6])
corrplot(correlationsMASS, method="circle")
correlationsMASS

##correlations of all Grays Harbor, Nome, and Wrangel Island birds
correlationsALL <- cor(all.rekn[,2:5])
corrplot(correlationsALL, method="circle")
correlationsALL

##correlations of only Grays Harbor birds
correlationsGH <- cor(dfGH[,2:5])
corrplot(correlationsGH, method="circle")
correlationsGH

##correlations of only Nome birds
correlationsNOME <- cor(df[,2:5])
corrplot(correlationsNOME, method="circle")
correlationsNOME

##correlations of Wrangel Island birds
correlationsWR <- cor(dfWR[,2:5])
corrplot(correlationsWR, method="circle")
correlationsWR


#######################################################################################################################################
####################                   MEASUREMENT COMPARISON BY SEX  & SITE                           ################################
#######################################################################################################################################


library(ggplot2)

##COMPARISONS BY SEX

##density plot all REKN from all sites
p1 <- ggplot(all.rekn,aes(x=Culmen, fill=CHDSex)) + 
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("All Red Knots from Nome, Grays Harbor, and Wrangel Island") +
                    theme(plot.title = element_text(size=11))
p2 <- ggplot(all.rekn,aes(x=TotalHead, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) +
                    ggtitle("All Red Knots from Nome, Grays Harbor, and Wrangel Island") +
                    theme(plot.title = element_text(size=11))
p3 <- ggplot(all.rekn,aes(x=TarsusDiagonal, fill=CHDSex)) + 
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("All Red Knots from Nome, Grays Harbor, and Wrangel Island") +
                    theme(plot.title = element_text(size=11))
p4 <- ggplot(all.rekn,aes(x=Wing, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) +
                    ggtitle("All Red Knots from Nome, Grays Harbor, and Wrangel Island") +
                    theme(plot.title = element_text(size=11))
multiplot(p1, p2, p3, p4, cols = 2)
##density plot only REKN from Nome
q1 <- ggplot(df,aes(x=Culmen, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Nome") +
                    theme(plot.title = element_text(size=11))
q2 <- ggplot(df,aes(x=TotalHead, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5)+ 
                    ggtitle("Only Red Knots from Nome") +
                    theme(plot.title = element_text(size=11))
q3 <- ggplot(df,aes(x=TarsusDiagonal, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5)+ 
                    ggtitle("Only Red Knots from Nome") +
                    theme(plot.title = element_text(size=11))
q4 <- ggplot(df,aes(x=Wing, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5)+ 
                    ggtitle("Only Red Knots from Nome") +
                    theme(plot.title = element_text(size=11))
multiplot(q1, q2, q3, q4, cols = 2)
##density plot only REKN from Grays Harbor
r1 <- ggplot(dfGH,aes(x=Culmen, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Grays Harbor") +
                    theme(plot.title = element_text(size=11))
r2 <- ggplot(dfGH,aes(x=TotalHead, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Grays Harbor") +
                    theme(plot.title = element_text(size=11))
r3 <- ggplot(dfGH,aes(x=TarsusDiagonal, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Grays Harbor") +
                    theme(plot.title = element_text(size=11))
r4 <- ggplot(dfGH,aes(x=Wing, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Grays Harbor") +
                    theme(plot.title = element_text(size=11))
multiplot(r1, r2, r3, r4, cols = 2)
##density plot only REKN from Wrangel Island
s1 <- ggplot(dfGH,aes(x=Culmen, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Wrangel Island") +
                    theme(plot.title = element_text(size=11))
s2 <- ggplot(dfGH,aes(x=TotalHead, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Wrangel Island") +
                    theme(plot.title = element_text(size=11))
s3 <- ggplot(dfGH,aes(x=TarsusDiagonal, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Wrangel Island") +
                    theme(plot.title = element_text(size=11))
s4 <- ggplot(dfGH,aes(x=Wing, fill=CHDSex)) +
                    geom_density(alpha=0.25, adjust = 2.5) + 
                    ggtitle("Only Red Knots from Wrangel Island") +
                    theme(plot.title = element_text(size=11))
multiplot(s1, s2, s3, s4, cols = 2)

##box plots of all individuals combined from all sites comparing male and female measurements
p1all.rekn <- ggplot(all.rekn,aes(y=Culmen, x=CHDSex, fill=CHDSex)) +
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Culmen (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none")
p2all.rekn <- ggplot(all.rekn,aes(y=TotalHead, x=CHDSex, fill=CHDSex)) +
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() +
                    labs(x="Sex", y="Total Head (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none")
p3all.rekn <- ggplot(all.rekn,aes(y=TarsusDiagonal, x=CHDSex, fill=CHDSex)) + 
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() +
                    labs(x=element_blank(),y="Tarsus (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none")
p4all.rekn <- ggplot(all.rekn,aes(y=Wing, x=CHDSex, fill=CHDSex)) +
                    geom_boxplot(alpha=0.25) +
                    scale_fill_grey() +
                    labs(x="Sex", y="Wing (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none")
multiplot(p1all.rekn, p2all.rekn, p3all.rekn, p4all.rekn, cols = 2)

##COMPARISONS OF SEX & SITE

##compare male measurements from Alaska, Wrangel, and birds tracked from Grays Harbor to their breeding site
#tracked birds were non-randomly selected
a1 <- ggplot(all.m, aes(y=Culmen, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Culmen (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Male Red Knot - Culmen")
a2 <- ggplot(all.m, aes(y=TotalHead, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Total Head (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Male Red Knot - Total Head")
a3 <- ggplot(all.m, aes(y=Wing, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Wing (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Male Red Knot - Wing")
a4 <- ggplot(all.m, aes(y=TarsusDiagonal, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Tarsus Diagonal (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Male Red Knot - Tarsus")
multiplot(a1, a2, a3, a4, cols = 2)


##compare male measurements from Alaska, Wrangel, and birds tracked from Grays Harbor to their breeding site
#tracked birds were non-randomly selected
a5 <- ggplot(all.f, aes(y=Culmen, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Culmen (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Female Red Knot - Culmen")
a6 <- ggplot(all.f, aes(y=TotalHead, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Total Head (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Female Red Knot - Total Head")
a7 <- ggplot(all.f, aes(y=Wing, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Wing (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Female Red Knot - Wing")
a8 <- ggplot(all.f, aes(y=TarsusDiagonal, x=site, fill = site))+
                    geom_boxplot(alpha=0.25) + 
                    scale_fill_grey() + 
                    labs(x=element_blank(), y="Tarsus Diagonal (mm)") +
                    theme(panel.background = element_blank(), axis.line = element_line("black"), legend.position = "none") +
                    stat_summary(fun=mean, geom="point", shape=20, size=2, color="black") +
                    ggtitle("Female Red Knot - Tarsus")
multiplot(a5, a6, a7, a8, cols = 2)


#######################################################################################################################################
####################                   TEST DIFFFERENCES BETWEEN SITES & SEX                           ################################
#######################################################################################################################################



##test differences between all males and all females
t.test(all.m$Culmen, all.f$Culmen, var.equal = TRUE, paired = FALSE)
t.test(all.m$TotalHead, all.f$TotalHead, var.equal = TRUE, paired = FALSE)
t.test(all.m$Wing, all.f$Wing, var.equal = TRUE, paired = FALSE)
t.test(all.m$TarsusDiagonal, all.f$TarsusDiagonal, var.equal = TRUE, paired = FALSE)

##test differences between males breeding in Alaska & Wrangel
#no tracked birds included (non-random sample)
t.test(df.m$Culmen, dfWR.m$Culmen, var.equal = TRUE, paired = FALSE)
t.test(df.m$TotalHead, dfWR.m$TotalHead, var.equal = TRUE, paired = FALSE)
t.test(df.m$Wing, dfWR.m$Wing, var.equal = TRUE, paired = FALSE)
t.test(df.m$TarsusDiagonal, dfWR.m$TarsusDiagonal, var.equal = TRUE, paired = FALSE)

##test differences between females breeding in Alaska & Wrangel
#no tracked birds included (non-random sample)
t.test(df.f$Culmen, dfWR.f$Culmen, var.equal = TRUE, paired = FALSE)
t.test(df.f$TotalHead, dfWR.f$TotalHead, var.equal = TRUE, paired = FALSE)
t.test(df.f$Wing, dfWR.f$Wing, var.equal = TRUE, paired = FALSE)
t.test(df.f$TarsusDiagonal, dfWR.f$TarsusDiagonal, var.equal = TRUE, paired = FALSE)


#######################################################################################################################################
####################                   TEST ASSUMPTIONS                                                ################################
#######################################################################################################################################


library(ggpubr)

##look for highly correlated morphometric measurements within males
correlationsM <- cor(all.m[,2:5])
corrplot(correlationsALL, method="circle")

##look for highly correlated morphometric measurements within females
correlationsF <- cor(all.f[,2:5])
corrplot(correlationsALL, method="circle")

##eye-test normality in male measurements with Q-Q plots
nm1 <- ggqqplot(all.m$Culmen)
nm2 <- ggqqplot(all.m$TotalHead)
nm3 <- ggqqplot(all.m$TarsusDiagonal)
nm4 <- ggqqplot(all.m$Wing)
multiplot(nm1, nm2, nm3, nm4, cols = 2)

##eye-test normality in female measurements with Q-Q plots
nf1 <- ggqqplot(all.f$Culmen)
nf2 <- ggqqplot(all.f$TotalHead)
nf3 <- ggqqplot(all.f$TarsusDiagonal)
nf4 <- ggqqplot(all.f$Wing)
multiplot(nf1, nf2, nf3, nf4, cols = 2)

##test for homogeneity of covariance
library(heplots)
boxM(all.rekn[2:5],all.rekn$CHDSex)


#######################################################################################################################################
####################                   STEPWISE VARIABLE SELECTION FOR LDA                             ################################
#######################################################################################################################################


##stepwise variable selection for lda with stop criterion improvement less than 1%.
library(klaR)
modelstepL <- stepclass(CHDSex ~ ., "lda", direction = "both", data = all.rekn, improvement = 0.01, fold = 189)

modelstepL


#######################################################################################################################################
####################                   BIOLOGICALLY RELEVENT MANUAL VARIABLE SELECTION FOR LDA        #################################
#######################################################################################################################################


library(MASS)

##cross-validated accuracey for LDA using different biologically relevent variable combinations
jacknife1 <- lda(CHDSex~.,data = all.rekn, CV = TRUE)
jacknife2 <- lda(CHDSex~ Culmen + Wing + TarsusDiagonal, data = all.rekn, CV = TRUE)
jacknife3 <- lda(CHDSex~ TotalHead + Wing + TarsusDiagonal, data = all.rekn, CV = TRUE)
jacknife4 <- lda(CHDSex~ TotalHead + Wing, data = all.rekn, CV = TRUE)
jacknife5 <- lda(CHDSex~ TotalHead + TarsusDiagonal, data = all.rekn, CV = TRUE)
jacknife6 <- lda(CHDSex~ Culmen + TarsusDiagonal, data = all.rekn, CV = TRUE)
jacknife7 <- lda(CHDSex~ TarsusDiagonal + Wing, data = all.rekn, CV = TRUE)
jacknife8 <- lda(CHDSex~ TarsusDiagonal, data = all.rekn, CV = TRUE)
jacknife9 <- lda(CHDSex~ TotalHead, data = all.rekn, CV = TRUE)
jacknife10 <- lda(CHDSex~ Wing, data = all.rekn, CV = TRUE)
jacknife11 <- lda(CHDSex~ Culmen, data = all.rekn, CV = TRUE)

##format lda outputs as confusion matrix
jacknife1.acc <- table(all.rekn$CHDSex, 
                       jacknife1$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife2.acc <- table(all.rekn$CHDSex, 
                       jacknife2$class, 
                       dnn = c("Actual Group", "Predicted Group"))
jacknife3.acc <- table(all.rekn$CHDSex, 
                       jacknife3$class, 
                       dnn = c("Actual Group", "Predicted Group"))
jacknife4.acc <- table(all.rekn$CHDSex, 
                       jacknife4$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife5.acc <- table(all.rekn$CHDSex, 
                       jacknife5$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife6.acc <- table(all.rekn$CHDSex, 
                       jacknife6$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife7.acc <- table(all.rekn$CHDSex, 
                       jacknife7$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife8.acc <- table(all.rekn$CHDSex, 
                       jacknife8$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife9.acc <- table(all.rekn$CHDSex, 
                       jacknife9$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife10.acc <- table(all.rekn$CHDSex, 
                       jacknife10$class,
                       dnn = c("Actual Group", "Predicted Group"))
jacknife11.acc <- table(all.rekn$CHDSex, 
                       jacknife11$class,
                       dnn = c("Actual Group", "Predicted Group"))

##evaluate accuracey of each LDA model 
library(caret)

cm1 <- caret::confusionMatrix(jacknife1.acc)
cm1
cm2 <- caret::confusionMatrix(jacknife2.acc)
cm2
cm3 <- caret::confusionMatrix(jacknife3.acc)
cm3
cm4 <- caret::confusionMatrix(jacknife4.acc)
cm4
cm5 <- caret::confusionMatrix(jacknife5.acc)
cm5
cm6 <- caret::confusionMatrix(jacknife6.acc)
cm6
cm7 <- caret::confusionMatrix(jacknife7.acc)
cm7
cm8 <- caret::confusionMatrix(jacknife8.acc)
cm8
cm9 <- caret::confusionMatrix(jacknife9.acc)
cm9
cm10 <- caret::confusionMatrix(jacknife10.acc)
cm10
cm11 <- caret::confusionMatrix(jacknife11.acc)
cm11

##the most accurate model was Total Head and Wing (cm4) with a classification accuracy of 85.86%
cm4
cm4$table
print(cm4$overall)

##re-run the lda with Total Head, Wing as variables without cross-validation (CV=TRUE) to get LD1 values to plot
lda4 <- lda(CHDSex~ TotalHead + Wing, data = all.rekn)
lda4
##use the lda to predict the sex of all REKN from the whole data.set
predict4 <- predict(lda4, newdata = all.rekn)
##combine individual LD1 values and posterior probabilities to dataset for graphing
lda.results <- data.frame(all.rekn, predict4$x, predict4$posterior)

names(lda.results)[names(lda.results) == "F"] <- "Prob.F"
##provide some small random error to the probabilities as they form a perfect curve, and I couldn't fit a curve to it
lda.results$Prob.F.jitter <- lda.results$Prob.F + rnorm(length(lda.results$Prob.F), sd = 0.000000005)

##fit a published equation to the curve 
fitmodel <- nls( Prob.F.jitter ~ a/(1 + exp(b * (LD1 + c))), data = lda.results, start=list(a = 1,b = 1.5,c = 0.1))

##coefficients in the equation
coef(fitmodel)

x <- seq(-4,4,.001)
y <- (coef(fitmodel)[1] / (1 + exp(coef(fitmodel)[2] * (x + coef(fitmodel)[3]))))

##solve for y in input LD1 (x) value with 80% probability female (0.8)
z1 <- (log((coef(fitmodel)[1]/0.8)-1)/coef(fitmodel)[2])-coef(fitmodel)[3]
##solve for y in input LD1 (x) value with 80% probability male (20% female, 0.2)
z2 <- (log((coef(fitmodel)[1]/0.2)-1)/coef(fitmodel)[2])-coef(fitmodel)[3]

testdf <- as.data.frame(cbind(x,y))
      
cutoffF <- lda.results %>% filter(Prob.F > 0.8)
cutoffM <- lda.results %>% filter(Prob.F < 0.2)

##plot a graph of the probability of being female by discriminate score with restricted classification cutoff scores
ggplot() +
  geom_point(data = lda.results, aes(x=LD1, y=Prob.F, shape = CHDSex, color = CHDSex), size = 3) +
  scale_color_manual(values=c('#A0A0A0','#303030'), labels = c("Known Female (n = 62)", "Known Male (n = 136)")) +
  scale_shape_manual(values=c(16, 17), labels = c("Known Female (n = 62)", "Known Male (n = 136)")) +
  geom_line(data = testdf, aes(x=x, y=y)) +
  labs(x="Descrimintate Score", y="Probability of being female") +
  theme(panel.background = element_blank(), 
        axis.line = element_line("black"), 
        legend.position = c(.75,.8), 
        legend.title=element_blank()) +
  geom_segment(aes(x = -4.05, y = .8, xend = z1, yend = 0.8)) +
  geom_segment(aes(x = -4.05, y = .5, xend = -0.78, yend = 0.5)) +
  geom_segment(aes(x = -4.05, y = .2, xend = z2, yend = 0.2)) +
  geom_segment(aes(x = z1, y = 0.8, xend = z1, yend = -0.05), arrow = arrow(type = "closed", angle = 20, length = unit(.1,"inches"))) +
  geom_segment(aes(x = -0.78, y = 0.5, xend = -0.78, yend = -0.05), arrow = arrow(type = "closed", angle = 20, length = unit(.1,"inches"))) +
  geom_segment(aes(x = z2, y = 0.2, xend = z2, yend = -0.05), arrow = arrow(type = "closed", angle = 20, length = unit(.1,"inches"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.05, 1.05)) +
  scale_x_continuous(expand = c(0, 0), limits = c(-4.05, 4.05))




