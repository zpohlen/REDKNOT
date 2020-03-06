#setwd("T:/Pohlen/R")
library(psych)
library(MASS)
library(dplyr)
library(MASS)
library(caret)
library(magrittr)
library(recipes)
library(dplyr)
library(rgl)
library(aod)
library(ggplot2)
library(corrplot)
library(heplots)
library(ROCR)

band2010 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2010.csv", stringsAsFactors = FALSE)
band2011 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2011.csv", stringsAsFactors = FALSE)
band2012 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2012.csv", stringsAsFactors = FALSE)
band2013 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2013.csv", stringsAsFactors = FALSE)
band2014 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2014.csv", stringsAsFactors = FALSE)
band2015 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2015.csv", stringsAsFactors = FALSE)
band2016 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2016.csv", stringsAsFactors = FALSE)
band2017 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2017.csv", stringsAsFactors = FALSE)
band2018 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2018.csv", stringsAsFactors = FALSE)
band2019 <- read.csv("T:/Landbirds/Red_Knot/Breeding/Data/R7MBMlb_REKN_Banding_2019.csv", stringsAsFactors = FALSE)

allband <- rbind(band2019, band2018, band2017, band2016, band2015, band2014, band2013, band2012, band2011, band2010)

#filter by adults and REKN in Nome
allbandadults <- allband %>% filter(
                                      AgeID == "AHY" | AgeID == "ASY" | AgeID == "ATY",
                                      Species == "REKN") %>%
                                      droplevels

#write csv if needed
#write.csv(allband, "Allband.csv", na = "")

all.sexed.adults <- allbandadults %>% filter(
  CHDSex == "M" | CHDSex == "F") %>%
  droplevels

#pull select fields with minimal NAs
df <- dplyr::select(all.sexed.adults, CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing)
df <- na.omit(df)

df$CHDSex <- as.factor(df$CHDSex)

summary(df)

#######################################################################################################################
##Logistic Regression##################################################################################################
#######################################################################################################################

##Multiplot function
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

correlations <- cor(df[,2:5])
corrplot(correlations, method="circle")

p1 <- ggplot(df,aes(x=Culmen, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p2 <- ggplot(df,aes(x=TotalHead, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p3 <- ggplot(df,aes(x=TarsusDiagonal, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p4 <- ggplot(df,aes(x=Wing, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
multiplot(p1, p2, p3, p4, cols = 2)

df.logit <- glm(CHDSex ~ Culmen + TotalHead + Wing, data = df, family = "binomial")

summary(df.logit)
confint.default(df.logit)
plot.glm

exp(cbind(OR = coef(df.logit), confint(df.logit)))

ggplot(df, aes(x= TotalHead, y = Wing, color = factor(CHDSex))) +
  geom_point() +
  stat_ellipse()
  

#######################################################################################################################
##Linear Discriminate Analysis#########################################################################################
#######################################################################################################################

#Test for homoscedasticity

boxM(df[2:5],df$CHDSex)

#Mahalanobis outliers

percentage.to.remove <- 2 # Remove % of points
number.to.remove <- trunc(nrow(df) * percentage.to.remove / 100)

#Look for outliers using all criteria
m.dist <- mahalanobis(df[, 2:5], colMeans(df[, 2:5]), cov(df[, 2:5]))
m.dist.order <- order(m.dist, decreasing=TRUE)
rows.to.keep.index <- m.dist.order[(number.to.remove+1):nrow(df[, 2:5])]
#remove the top 5% of outliers
df.outliers.out <- df[rows.to.keep.index,]

df.nome.m <- df.outliers.out %>% filter ( CHDSex == "M")
df.nome.f <- df.outliers.out %>% filter ( CHDSex == "F")

mean(df.nome.m$Culmen)
sd(df.nome.m$Culmen)
max(df.nome.m$Culmen)
min(df.nome.m$Culmen)

mean(df.nome.f$Culmen)
sd(df.nome.f$Culmen)
max(df.nome.f$Culmen)
min(df.nome.f$Culmen)

var.test(df.nome.m$Culmen, df.nome.f$Culmen)
t.test(df.nome.m$Culmen, df.nome.f$Culmen, var.equal = TRUE, paired = FALSE)
((mean(df.nome.f$Culmen) - mean(df.nome.m$Culmen))/(mean(df.nome.f$Culmen) + mean(df.nome.m$Culmen))*0.5)*100


mean(df.nome.m$TotalHead)
sd(df.nome.m$TotalHead)
max(df.nome.m$TotalHead)
min(df.nome.m$TotalHead)
max(df.nome.m$TotalHead)

mean(df.nome.f$TotalHead)
sd(df.nome.f$TotalHead)
max(df.nome.f$TotalHead)
min(df.nome.f$TotalHead)
max(df.nome.f$TotalHead)

var.test(df.nome.m$TotalHead, df.nome.f$TotalHead)
t.test(df.nome.m$TotalHead, df.nome.f$TotalHead, var.equal = TRUE, paired = FALSE)
((mean(df.nome.f$TotalHead) - mean(df.nome.m$TotalHead))/(mean(df.nome.f$TotalHead) + mean(df.nome.m$TotalHead))*0.5)*100

mean(df.nome.m$TarsusDiagonal)
sd(df.nome.m$TarsusDiagonal)
max(df.nome.m$TarsusDiagonal)
min(df.nome.m$TarsusDiagonal)
max(df.nome.m$TarsusDiagonal)

mean(df.nome.f$TarsusDiagonal)
sd(df.nome.f$TarsusDiagonal)
max(df.nome.f$TarsusDiagonal)
min(df.nome.f$TarsusDiagonal)
max(df.nome.f$TarsusDiagonal)

var.test(df.nome.m$TarsusDiagonal, df.nome.f$TarsusDiagonal)
t.test(df.nome.m$TarsusDiagonal, df.nome.f$TarsusDiagonal, var.equal = TRUE, paired = FALSE)
((mean(df.nome.f$TarsusDiagonal) - mean(df.nome.m$TarsusDiagonal))/(mean(df.nome.f$TarsusDiagonal) + mean(df.nome.m$TarsusDiagonal))*0.5)*100


mean(df.nome.m$Wing)
sd(df.nome.m$Wing)
max(df.nome.m$Wing)
min(df.nome.m$Wing)
max(df.nome.m$Wing)

mean(df.nome.f$Wing)
sd(df.nome.f$Wing)
max(df.nome.f$Wing)
min(df.nome.f$Wing)
max(df.nome.f$Wing)

var.test(df.nome.m$Wing, df.nome.f$Wing)
t.test(df.nome.m$Wing, df.nome.f$Wing, var.equal = TRUE, paired = FALSE)
((mean(df.nome.f$Wing) - mean(df.nome.m$Wing))/(mean(df.nome.f$Wing) + mean(df.nome.m$Wing))*0.5)*100

#Create LDA using three methods##################################################################################
#Linear Discriminate Function using cross-validation (jacknife)
df.jacknife1 <- lda(CHDSex~.,data = df.outliers.out, CV = TRUE)
df.jacknife2 <- lda(CHDSex~ Culmen + Wing + TarsusDiagonal, data = df.outliers.out, CV = TRUE)
df.jacknife3 <- lda(CHDSex~ Culmen + Wing + TotalHead, data = df.outliers.out, CV = TRUE)
df.jacknife4 <- lda(CHDSex~ TotalHead + Wing, data = df.outliers.out, CV = TRUE)

#Linear Discriminate Function using resubstitution
df.resub1 <- lda(CHDSex~.,data = df.outliers.out)
df.resub2 <- lda(CHDSex~ Culmen + Wing + TarsusDiagonal, data = df.outliers.out)
df.resub3 <- lda(CHDSex~ Culmen + Wing + TotalHead, data = df.outliers.out)
df.resub4 <- lda(CHDSex~ TotalHead + Wing, data = df.outliers.out)

mean(subset(df.outliers.out, CHDSex == "M")$v)
mean(subset(df.outliers.out, CHDSex == "F")$v)  

(mean(subset(df.outliers.out, CHDSex == "M")$v) +mean(subset(df.outliers.out, CHDSex == "F")$v))/2

df.outliers.out$v <- df.outliers.out$TotalHead*-0.5402185 + df.outliers.out$Wing*-0.13662
ggplot(df.outliers.out, aes(x=df.outliers.out$v, fill = df.outliers.out$CHDSex))+
  geom_histogram(binwidth = .02, alpha = 0.5)

ggplot(df.outliers.out, aes(v, fill = CHDSex)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .22) +
  geom_vline(xintercept = (mean(subset(df.outliers.out, CHDSex == "M")$v)+mean(subset(df.outliers.out, CHDSex == "F")$v))/2) +
geom_vline(xintercept = mean(subset(df.outliers.out, CHDSex == "M")$v), color = "blue") +
geom_vline(xintercept = mean(subset(df.outliers.out, CHDSex == "F")$v), color = "pink") 
           
#Linear Discriminate Function using sample splitting
#Create train and test data.frames
set.seed(1337)
smp_size <- floor(0.66 * nrow(df))

train_ind <- sample(seq_len(nrow(df.outliers.out)), size = smp_size)

train <- df.outliers.out[train_ind,]
test <- df.outliers.out[-train_ind,]

df.samplesplit1 <- lda(CHDSex~.,data = train)
df.samplesplit2 <- lda(CHDSex~ Culmen + Wing + TarsusDiagonal, data = train)
df.samplesplit3 <- lda(CHDSex~ Culmen + Wing + TotalHead, data = train)
df.samplesplit4 <- lda(CHDSex~ TotalHead + Wing, data = train)

#Use the LDA to predict the sex in a confusion matrix######################################################
#Cross-validation
df.jacknife1.acc <- table(df.outliers.out$CHDSex, df.jacknife1$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife2.acc <- table(df.outliers.out$CHDSex, df.jacknife2$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife3.acc <- table(df.outliers.out$CHDSex, df.jacknife3$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife4.acc <- table(df.outliers.out$CHDSex, df.jacknife4$class, dnn = c("Actual Group", "Predicted Group"))

#Resubstitution
df.resub1.predict <- predict(df.resub1, newdata = df.outliers.out)
df.resub2.predict <- predict(df.resub2, newdata = df.outliers.out)
df.resub3.predict <- predict(df.resub3, newdata = df.outliers.out)
df.resub4.predict <- predict(df.resub4, newdata = df.outliers.out)


df.resub1.acc <- table(df.outliers.out$CHDSex, df.resub1.predict$class, dnn = c("Actual Group", "Predicted Group"))
df.resub2.acc <- table(df.outliers.out$CHDSex, df.resub2.predict$class, dnn = c("Actual Group", "Predicted Group"))
df.resub3.acc <- table(df.outliers.out$CHDSex, df.resub3.predict$class, dnn = c("Actual Group", "Predicted Group"))
df.resub4.acc <- table(df.outliers.out$CHDSex, df.resub4.predict$class, dnn = c("Actual Group", "Predicted Group"))

#Sample splitting
df.samplesplit1.predict <- predict(df.samplesplit1, newdata = test)
df.samplesplit2.predict <- predict(df.samplesplit2, newdata = test)
df.samplesplit3.predict <- predict(df.samplesplit3, newdata = test)
df.samplesplit4.predict <- predict(df.samplesplit4, newdata = test)

df.samplesplit1.acc <- table(test$CHDSex, df.samplesplit1.predict$class)
df.samplesplit2.acc <- table(test$CHDSex, df.samplesplit2.predict$class)
df.samplesplit3.acc <- table(test$CHDSex, df.samplesplit3.predict$class)
df.samplesplit4.acc <- table(test$CHDSex, df.samplesplit4.predict$class)

#Calculate the accuracy & CI  of each model##############################################################################
#Cross-validation
caret::confusionMatrix(df.jacknife1.acc)
caret::confusionMatrix(df.jacknife2.acc)
caret::confusionMatrix(df.jacknife3.acc)
caret::confusionMatrix(df.jacknife4.acc)

#resubstitution
caret::confusionMatrix(df.resub1.acc)
caret::confusionMatrix(df.resub2.acc)
caret::confusionMatrix(df.resub3.acc)
caret::confusionMatrix(df.resub4.acc)

#sample splitting
caret::confusionMatrix(df.samplesplit1.acc)
caret::confusionMatrix(df.samplesplit2.acc)
caret::confusionMatrix(df.samplesplit3.acc)
caret::confusionMatrix(df.samplesplit4.acc)

##Accuracy calculation simple form###################################################################################
#Jacknife
sum(df.jacknife1.acc[row(df.jacknife1.acc) == col(df.jacknife1.acc)]) / sum(df.jacknife1.acc)
sum(df.jacknife2.acc[row(df.jacknife2.acc) == col(df.jacknife2.acc)]) / sum(df.jacknife2.acc)
sum(df.jacknife3.acc[row(df.jacknife3.acc) == col(df.jacknife3.acc)]) / sum(df.jacknife3.acc)
sum(df.jacknife4.acc[row(df.jacknife4.acc) == col(df.jacknife4.acc)]) / sum(df.jacknife4.acc)

#Resubstitution
sum(df.resub1.acc[row(df.resub1.acc) == col(df.resub1.acc)]) / sum(df.resub1.acc)
sum(df.resub2.acc[row(df.resub2.acc) == col(df.resub2.acc)]) / sum(df.resub2.acc)
sum(df.resub3.acc[row(df.resub3.acc) == col(df.resub3.acc)]) / sum(df.resub3.acc)
sum(df.resub4.acc[row(df.resub4.acc) == col(df.resub4.acc)]) / sum(df.resub4.acc)

#Sample splitting
sum(df.samplesplit1.acc[row(df.samplesplit1.acc) == col(df.samplesplit1.acc)]) / sum(df.samplesplit1.acc)
sum(df.samplesplit2.acc[row(df.samplesplit2.acc) == col(df.samplesplit2.acc)]) / sum(df.samplesplit2.acc)
sum(df.samplesplit3.acc[row(df.samplesplit3.acc) == col(df.samplesplit3.acc)]) / sum(df.samplesplit3.acc)
sum(df.samplesplit4.acc[row(df.samplesplit4.acc) == col(df.samplesplit4.acc)]) / sum(df.samplesplit4.acc)

#Create dataframes for graphing####################################################################################
#jacknife
df.jacknife1.g <- data.frame(type = df.outliers.out[,1], lda = df.jacknife1$x)

#resubstitution
df.resub1.g <- data.frame(type = df.outliers.out[,1], lda = df.resub1.predict$x)
df.resub2.g <- data.frame(type = df.outliers.out[,1], lda = df.resub2.predict$x)
df.resub3.g <- data.frame(type = df.outliers.out[,1], lda = df.resub3.predict$x)
df.resub4.g <- data.frame(type = df.outliers.out[,1], lda = df.resub4.predict$x)

#sample splitting
df.samplesplit1.g <- data.frame(type = test[,1], lda = df.samplesplit1.predict$x)
df.samplesplit2.g <- data.frame(type = test[,1], lda = df.samplesplit2.predict$x)
df.samplesplit3.g <- data.frame(type = test[,1], lda = df.samplesplit3.predict$x)
df.samplesplit4.g <- data.frame(type = test[,1], lda = df.samplesplit4.predict$x)

#Histograms#######################################################################################################
#resubstitute
ggplot(df.resub1.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .5)

ggplot(df.resub2.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .5)

ggplot(df.resub3.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .5)

ggplot(df.resub4.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .22)

#sample splitting
ggplot(df.samplesplit1.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .5)

ggplot(df.samplesplit2.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .5)

ggplot(df.samplesplit3.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .5)

ggplot(df.samplesplit4.g, aes(LD1, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = .5)

#Density Graph####################################################################################################
#resubstitute
ggplot(df.resub1.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)

ggplot(df.resub2.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)

ggplot(df.resub3.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)

ggplot(df.resub4.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)

#Sample splitting
ggplot(df.samplesplit1.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)

ggplot(df.samplesplit2.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)

ggplot(df.samplesplit3.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)

ggplot(df.samplesplit4.g, aes(LD1, fill = type)) +
  geom_density(adjust = 1.35, alpha = 0.5)


##Grays Harbor known sex and breeding site#####################################################################

#Red GH banding file
GHband <- read.csv("R7MBMlb_GH_banding.csv", stringsAsFactors = FALSE)

summary(GHband)

dfGH.nosite <- GHband %>% filter(
  CHDSex == "M" | CHDSex == "F") %>%
  droplevels

#remove unneccesary columns
dfGH.nosite <- dplyr::select(dfGH.nosite, CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing)
dfGH.nosite <- na.omit(dfGH.nosite)

dfGH.nosite$CHDSex <- as.factor(dfGH.nosite$CHDSex)

correlationsGH.nosite <- cor(dfGH.nosite[,2:5])
corrplot(correlationsGH.nosite, method="circle")


#Test for homoscedasticity

boxM(df[2:5],dfGH.nosite$CHDSex)


percentage.to.remove <- 2 # Remove % of points
number.to.remove <- trunc(nrow(dfGH.nosite) * percentage.to.remove / 100)

#Look for outliers using all criteria
m.dist.ns <- mahalanobis(dfGH.nosite[, 2:5], colMeans(dfGH.nosite[, 2:5]), cov(dfGH.nosite[, 2:5]))
m.dist.order.ns <- order(m.dist.ns, decreasing=TRUE)
rows.to.keep.index.ns <- m.dist.order.ns[(number.to.remove+1):nrow(dfGH.nosite[, 2:5])]
#remove the top 5% of outliers
dfGH.outliers.out.ns <- dfGH.nosite[rows.to.keep.index.ns,]

p1GH.ns <- ggplot(dfGH.outliers.out.ns,aes(x=Culmen, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p2GH.ns <- ggplot(dfGH.outliers.out.ns,aes(x=TotalHead, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p3GH.ns <- ggplot(dfGH.outliers.out.ns,aes(x=TarsusDiagonal, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p4GH.ns <- ggplot(dfGH.outliers.out.ns,aes(x=Wing, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
multiplot(p1GH.ns, p2GH.ns, p3GH.ns, p4GH.ns, cols = 2)


#Use the LDA to predict the sex in a confusion matrix######################################################
#Cross-validation
df.jacknife1GH.ns <- lda(CHDSex~.,data = dfGH.outliers.out.ns, CV = TRUE)
df.jacknife2GH.ns <- lda(CHDSex~ Culmen + Wing + TarsusDiagonal, data = dfGH.outliers.out.ns, CV = TRUE)
df.jacknife3GH.ns <- lda(CHDSex~ Culmen + Wing + TotalHead, data = dfGH.outliers.out.ns, CV = TRUE)
df.jacknife4GH.ns <- lda(CHDSex~ TotalHead + Wing, data = dfGH.outliers.out.ns, CV = TRUE)

df.jacknife1GH.acc.ns <- table(dfGH.outliers.out.ns$CHDSex, df.jacknife1GH.ns$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife2GH.acc.ns <- table(dfGH.outliers.out.ns$CHDSex, df.jacknife2GH.ns$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife3GH.acc.ns <- table(dfGH.outliers.out.ns$CHDSex, df.jacknife3GH.ns$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife4GH.acc.ns <- table(dfGH.outliers.out.ns$CHDSex, df.jacknife4GH.ns$class, dnn = c("Actual Group", "Predicted Group"))

caret::confusionMatrix(df.jacknife1GH.acc.ns)
caret::confusionMatrix(df.jacknife2GH.acc.ns)
caret::confusionMatrix(df.jacknife3GH.acc.ns)
caret::confusionMatrix(df.jacknife4GH.acc.ns)

#Filter GH data for only sexed birds of known breeding location#############################################
all.sexed.GH <- GHband %>% filter(
  site.sex == "AF" | site.sex == "AM" | site.sex == "WF" | site.sex == "WM") %>%
  droplevels

#remove unneccesary columns
dfGH <- dplyr::select(all.sexed.GH, CHDSex, Culmen, TotalHead, TarsusDiagonal, Wing, site.sex)
dfGH <- na.omit(dfGH)

dfGH$CHDSex <- as.factor(dfGH$CHDSex)
dfGH$site.sex <- as.factor(dfGH$site.sex)

summary(dfGH)

correlationsGH <- cor(dfGH[,2:5])
corrplot(correlationsGH, method="circle")

p1GH <- ggplot(dfGH,aes(x=Culmen, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p2GH <- ggplot(dfGH,aes(x=TotalHead, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p3GH <- ggplot(dfGH,aes(x=TarsusDiagonal, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p4GH <- ggplot(dfGH,aes(x=Wing, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
multiplot(p1GH, p2GH, p3GH, p4GH, cols = 2)

#Test for homoscedasticity

boxM(df[2:5],dfGH$site.sex)

#Mahalanobis outliers

percentage.to.remove <- 2 # Remove % of points
number.to.remove <- trunc(nrow(dfGH) * percentage.to.remove / 100)

#Look for outliers using all criteria
m.dist <- mahalanobis(dfGH[, 2:5], colMeans(dfGH[, 2:5]), cov(dfGH[, 2:5]))
m.dist.order <- order(m.dist, decreasing=TRUE)
rows.to.keep.index <- m.dist.order[(number.to.remove+1):nrow(dfGH[, 2:5])]
#remove the top 5% of outliers
dfGH.outliers.out <- dfGH[rows.to.keep.index,]

#Use the LDA to predict the sex in a confusion matrix######################################################
#Cross-validation
df.jacknife1GH <- lda(site.sex~.,data = dfGH.outliers.out[,2:6], CV = TRUE)
df.jacknife2GH <- lda(site.sex~ Culmen + Wing + TarsusDiagonal, data = dfGH.outliers.out, CV = TRUE)
df.jacknife3GH <- lda(site.sex~ Culmen + Wing + TotalHead, data = dfGH.outliers.out, CV = TRUE)
df.jacknife4GH <- lda(site.sex~ TotalHead + Wing, data = dfGH.outliers.out, CV = TRUE)

df.jacknife1GH.acc <- table(dfGH.outliers.out$site.sex, df.jacknife1GH$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife2GH.acc <- table(dfGH.outliers.out$site.sex, df.jacknife2GH$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife3GH.acc <- table(dfGH.outliers.out$site.sex, df.jacknife3GH$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife4GH.acc <- table(dfGH.outliers.out$site.sex, df.jacknife4GH$class, dnn = c("Actual Group", "Predicted Group"))

caret::confusionMatrix(df.jacknife1GH.acc)
caret::confusionMatrix(df.jacknife2GH.acc)
caret::confusionMatrix(df.jacknife3GH.acc)
caret::confusionMatrix(df.jacknife4GH.acc)

#known sex predict breeding site################################################################################
#select males from GH
dfGHM <- dfGH %>% filter(CHDSex == "M") %>% droplevels
dfGHF <- dfGH %>% filter(CHDSex == "F") %>% droplevels

ggplot(dfGHM, aes(x= TotalHead, y = Wing, color = factor(site.sex))) +
  geom_point() 

ggplot(dfGHF, aes(x= TotalHead, y = Wing, color = factor(site.sex))) +
  geom_point() 

##only females#####################################################
correlationsGHF <- cor(dfGHF[,2:5])
corrplot(correlationsGHF, method="circle")

p1GHF <- ggplot(dfGHF,aes(x=Culmen, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p2GHF <- ggplot(dfGHF,aes(x=TotalHead, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p3GHF <- ggplot(dfGHF,aes(x=TarsusDiagonal, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p4GHF <- ggplot(dfGHF,aes(x=Wing, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
multiplot(p1GHF, p2GHF, p3GHF, p4GHF, cols = 2)

#Test for homoscedasticity

boxM(df[2:5],dfGHF$site.sex)

#Mahalanobis outliers

percentage.to.remove <- 0 # Remove % of points
number.to.remove <- trunc(nrow(dfGHF) * percentage.to.remove / 100)

#Look for outliers using all criteria
m.dist <- mahalanobis(dfGHF[, 2:5], colMeans(dfGHF[, 2:5]), cov(dfGHF[, 2:5]))
m.dist.order <- order(m.dist, decreasing=TRUE)
rows.to.keep.index <- m.dist.order[(number.to.remove+1):nrow(dfGHF[, 2:5])]
#remove the top 5% of outliers
dfGHF.outliers.out <- dfGHF[rows.to.keep.index,]


#Use the LDA to predict the sex in a confusion matrix######################################################
#Cross-validation
df.jacknife1GHF <- lda(site.sex~.,data = dfGHF.outliers.out[,2:6], CV = TRUE)
df.jacknife2GHF <- lda(site.sex~ Culmen + Wing + TarsusDiagonal, data = dfGHF.outliers.out, CV = TRUE)
df.jacknife3GHF <- lda(site.sex~ Culmen + Wing + TotalHead, data = dfGHF.outliers.out, CV = TRUE)
df.jacknife4GHF <- lda(site.sex~ TotalHead + Wing, data = dfGHF.outliers.out, CV = TRUE)

df.jacknife1GHF.acc <- table(dfGHF.outliers.out$site.sex, df.jacknife1GHF$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife2GHF.acc <- table(dfGHF.outliers.out$site.sex, df.jacknife2GHF$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife3GHF.acc <- table(dfGHF.outliers.out$site.sex, df.jacknife3GHF$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife4GHF.acc <- table(dfGHF.outliers.out$site.sex, df.jacknife4GHF$class, dnn = c("Actual Group", "Predicted Group"))

caret::confusionMatrix(df.jacknife1GHF.acc)
caret::confusionMatrix(df.jacknife2GHF.acc)
caret::confusionMatrix(df.jacknife3GHF.acc)
caret::confusionMatrix(df.jacknife4GHF.acc)

##only males#####################################################
correlationsGHM <- cor(dfGHM[,2:5])
corrplot(correlationsGHM, method="circle")

p1GHM <- ggplot(dfGHM,aes(x=Culmen, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p2GHM <- ggplot(dfGHM,aes(x=TotalHead, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p3GHM <- ggplot(dfGHM,aes(x=TarsusDiagonal, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
p4GHM <- ggplot(dfGHM,aes(x=Wing, fill=site.sex)) + geom_density(alpha=0.25, adjust = 2.5)
multiplot(p1GHM, p2GHM, p3GHM, p4GHM, cols = 2)

#Test for homoscedasticity

boxM(df[2:5],dfGHM$site.sex)

#Mahalanobis outliers

percentage.to.remove <- 2 # Remove % of points
number.to.remove <- trunc(nrow(dfGHM) * percentage.to.remove / 100)

#Look for outliers using all criteria
m.dist <- mahalanobis(dfGHM[, 2:5], colMeans(dfGHM[, 2:5]), cov(dfGHM[, 2:5]))
m.dist.order <- order(m.dist, decreasing=TRUE)
rows.to.keep.index <- m.dist.order[(number.to.remove+1):nrow(dfGHM[, 2:5])]
#remove the top 5% of outliers
dfGHM.outliers.out <- dfGHM[rows.to.keep.index,]


#Use the LDA to predict the sex in a confusion matrix######################################################
#Cross-validation
df.jacknife1GHM <- lda(site.sex~.,data = dfGHM.outliers.out[,2:6], CV = TRUE)
df.jacknife2GHM <- lda(site.sex~ Culmen + Wing + TarsusDiagonal, data = dfGHM.outliers.out, CV = TRUE)
df.jacknife3GHM <- lda(site.sex~ Culmen + Wing + TotalHead, data = dfGHM.outliers.out, CV = TRUE)
df.jacknife4GHM <- lda(site.sex~ TotalHead + Wing, data = dfGHM.outliers.out, CV = TRUE)

df.jacknife1GHM.acc <- table(dfGHM.outliers.out$site.sex, df.jacknife1GHM$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife2GHM.acc <- table(dfGHM.outliers.out$site.sex, df.jacknife2GHM$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife3GHM.acc <- table(dfGHM.outliers.out$site.sex, df.jacknife3GHM$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife4GHM.acc <- table(dfGHM.outliers.out$site.sex, df.jacknife4GHM$class, dnn = c("Actual Group", "Predicted Group"))

caret::confusionMatrix(df.jacknife1GHM.acc)
caret::confusionMatrix(df.jacknife2GHM.acc)
caret::confusionMatrix(df.jacknife3GHM.acc)
caret::confusionMatrix(df.jacknife4GHM.acc)


#############################################################################################################
#combine GH and Nome


ASN <- rbind(df,dfGH.nosite)

summary(ASN)

correlationsASN <- cor(ASN[,2:5])
corrplot(correlationsASN, method="circle")

p1ASN <- ggplot(ASN,aes(x=Culmen, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p2ASN <- ggplot(ASN,aes(x=TotalHead, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p3ASN <- ggplot(ASN,aes(x=TarsusDiagonal, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
p4ASN <- ggplot(ASN,aes(x=Wing, fill=CHDSex)) + geom_density(alpha=0.25, adjust = 2.5)
multiplot(p1ASN, p2ASN, p3ASN, p4ASN, cols = 2)


df.jacknife1ASN <- lda(CHDSex~.,data = ASN, CV = TRUE)
df.jacknife2ASN <- lda(CHDSex~ Culmen + Wing + TarsusDiagonal, data = ASN, CV = TRUE)
df.jacknife3ASN <- lda(CHDSex~ Culmen + Wing + TotalHead, data = ASN, CV = TRUE)
df.jacknife4ASN <- lda(CHDSex~ TotalHead + Wing, data = ASN, CV = TRUE)

df.jacknife1ASN.acc <- table(ASN$CHDSex, df.jacknife1ASN$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife2ASN.acc <- table(ASN$CHDSex, df.jacknife2ASN$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife3ASN.acc <- table(ASN$CHDSex, df.jacknife3ASN$class, dnn = c("Actual Group", "Predicted Group"))
df.jacknife4ASN.acc <- table(ASN$CHDSex, df.jacknife4ASN$class, dnn = c("Actual Group", "Predicted Group"))

caret::confusionMatrix(df.jacknife1ASN.acc)
caret::confusionMatrix(df.jacknife2ASN.acc)
caret::confusionMatrix(df.jacknife3ASN.acc)
caret::confusionMatrix(df.jacknife4ASN.acc)
