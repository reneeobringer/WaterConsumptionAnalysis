# Project: Water Consumption Analysis - Indianapolis, Indiana
# Code By: Renee Obringer
# Last Updated: 30 September 2021

rm(list=ls())

# libraries  
library(rgdal)    # for readOGR 
library(sp)       # for spatial objects
library(dplyr)
library(ggplot2)
library(randomForest)
library(zoo)
library(maptools) # for ggplot map plotting
library(rgeos)    # for maptool
library(corrplot)
library(cowplot)

# set directories
datadir1 <- '/Users/robringer/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/Rcode/projects/indywaterconsumption/git/censustracts' # shape file directory
datadir2 <- '/Users/robringer/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/Rcode/projects/indywaterconsumption/git'   # input data directory
outputdir <- '/Users/robringer/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research/Rcode/projects/indywaterconsumption/git'  # output directory


######## SHAPE FILE PRE-PROCESSING ############

# load shapefile 
setwd(datadir1)
tract <- readOGR(dsn=".", layer = "tl_2017_18_tract")
tract@data$GEOID <- as.character(tract@data$GEOID)
tract <- subset(tract, grepl("18097", tract@data$GEOID)) # extract Marion County

# plot study area
ggtract <- fortify(tract, region = "GEOID") 

ggplot() +
  geom_polygon(data = ggtract , aes(x=long, y=lat, group = group), color="grey50", , fill='white') + 
  theme_light() + theme(text = element_text(size=16)) + xlab("Longitude") + ylab ("Latitude")

# get lat/lon for all census tracts
coordinates <- data.frame(tract$GEOID, as.numeric(as.character(tract$INTPTLAT)), as.numeric(as.character(tract$INTPTLON)))
names(coordinates) <- c('GEOID','Latitude','Longitude')
setwd(outputdir)
write.csv(coordinates,file='censustracts.csv')

######## LOAD DATA ######
setwd(datadir2)

# Census Data
load('censusdata.rdata')

# Water Consumption Data
load('waterdata.rdata')

# Climate Data
load('climatedata.rdata')

######## MERGE INPUT DATA ########

# Separate Climate Data by Months
jandata <- climdata[which(climdata$month == '2018-01'),]; febdata <- climdata[which(climdata$month == '2018-02'),]
mardata <- climdata[which(climdata$month == '2018-03'),]; aprdata <- climdata[which(climdata$month == '2018-04'),]
maydata <- climdata[which(climdata$month == '2018-05'),]; jundata <- climdata[which(climdata$month == '2018-06'),]
juldata <- climdata[which(climdata$month == '2018-07'),]; augdata <- climdata[which(climdata$month == '2018-08'),]
sepdata <- climdata[which(climdata$month == '2018-09'),]; octdata <- climdata[which(climdata$month == '2018-10'),]
novdata <- climdata[which(climdata$month == '2018-11'),]; decdata <- climdata[which(climdata$month == '2018-12'),]

# Merge Census and Water Data
censuswater <- merge(aggwaterdata, censusdata, by='geoid')

# Merge Monthly Climate Data with Corresponding Census and Water Data
january <- merge(censuswater[,-c(3:13)],jandata[,-c(2:5)],by='geoid');february <- merge(censuswater[,-c(2,4:13)],febdata[,-c(2:5)],by='geoid')
march <- merge(censuswater[,-c(2:3,5:13)],mardata[,-c(2:5)],by='geoid');april <- merge(censuswater[,-c(2:4,6:13)],aprdata[,-c(2:5)],by='geoid')
may <- merge(censuswater[,-c(2:5,7:13)],maydata[,-c(2:5)],by='geoid');june <- merge(censuswater[,-c(2:6,8:13)],jundata[,-c(2:5)],by='geoid')
july <- merge(censuswater[,-c(2:7,9:13)],juldata[,-c(2:5)],by='geoid');august <- merge(censuswater[,-c(2:8,10:13)],augdata[,-c(2:5)],by='geoid')
september <- merge(censuswater[,-c(2:9,11:13)],sepdata[,-c(2:5)],by='geoid');october <- merge(censuswater[,-c(2:10,12:13)],octdata[,-c(2:5)],by='geoid')
november <- merge(censuswater[,-c(2:11,13)],novdata[,-c(2:5)],by='geoid');december <- merge(censuswater[,-c(2:12)],decdata[,-c(2:5)],by='geoid')

# Merge All Months into a List
finaldata <- list(january, february, march, april, may, june, july, august, september, october, november, december)

setwd(datadir2)
save(finaldata,file='alldata.rdata')

######## MODEL DEVELOPMENT - INITIAL MODEL RUN #######
setwd(datadir2)
load('alldata.rdata')

# separate data into seasons
for (i in 1:12) {
  names(finaldata[[i]])[2] <- "consumption"
}

spring <- rbind(finaldata[[3]], finaldata[[4]], finaldata[[5]])
summer <- rbind(finaldata[[6]], finaldata[[7]], finaldata[[8]])
fall <- rbind(finaldata[[9]], finaldata[[10]], finaldata[[11]])
winter <- rbind(finaldata[[12]], finaldata[[1]], finaldata[[2]])

seasons <- list(spring, summer, fall, winter)

# split data into two groups
outliers <- list()
normals <- list()
for (i in 1:4) {
  
  q75 <- quantile(seasons[[i]][,2])[4]
  
  outliers[[i]] <- seasons[[i]][which(seasons[[i]][,2] > q75),]
  normals[[i]] <- seasons[[i]][which(seasons[[i]][,2] <= q75),]
}

data <- list(normals, outliers)

alldatafin <- list()
allmodsfin <- list()
allrsqfin <- list()
allrmsefin <- list()
allnrmsefin <- list()

for (d in 1:2) {
  
  alldata2 <- list()
  allmods2 <- list()
  allrsq <- c()
  allrmse <- c()
  allnrmse <- c()
  
  # loop though seasons
  for (m in 1:4) {
    # loop through folds
    allyhats <- c()
    alltracts <- c()
    allytest <- c()
    rsq <- c()
    rmse <- c()
    nrmse <- c()
    
    # set up 5-fold cross validation
    k <- 5
    n <- nrow(data[[d]][[m]])
    
    set.seed(11)
    newdata <- data[[d]][[m]][sample(n),]
    
    folds <- cut(seq(1,n),breaks = k, labels = FALSE)
    
    for (j in 1:k) {
      testIndex <- which(folds == j, arr.ind = TRUE)
      testData <- newdata[testIndex,]
      trainData <- newdata[-testIndex,]
      
      testData <- na.omit(testData)
      trainData <- na.omit(trainData)
      
      rfmod <- randomForest(trainData[,c(4:81)],trainData[,2], importance = T)
      yhat <- predict(rfmod, testData[,c(4:81)])
      
      # store variables
      allyhats <- append(allyhats, yhat)
      alltracts <- append(alltracts, testData[,1])
      allytest <- append(allytest, testData[,m+1])
      
      # calculate model performance
      rsq[j] <- mean(rfmod$rsq)
      rmse[j] <- mean(sqrt(rfmod$mse))
      nrmse[j] <- mean(sqrt(rfmod$mse)/(max(trainData[,2])-min(trainData[,2])))
    }
    
    # store variables
    alldata2[[m]] <- list(alltracts,allytest, allyhats)
    allmods2[[m]] <- rfmod
    allrsq[m] <- mean(rsq)
    allrmse[m] <- mean(rmse)
    allnrmse[m] <- mean(nrmse)
  }
  alldatafin[[d]] <- alldata2
  allmodsfin[[d]] <- allmods2
  allrsqfin[[d]] <- allrsq
  allrmsefin[[d]] <- allrmse
  allnrmsefin[[d]] <- allnrmse
  
}

setwd(outputdir)
save(list=c("data","alldatafin","allmodsfin","allrsqfin","allrmsefin","allnrmsefin"), file="firstmodelrunresults.rdata")

######## MODEL DEVELOPMENT - VARIABLE SELECTION #######
setwd(outputdir)
load('firstmodelrunresults.rdata')

# first step: filtering based on correlation

# initialize variables
alldata <- list()

# loop through datasets
for (d in 1:2) {
  
  # initialize variables
  monthdata <- list()
  
  # loop through seasons
  for (m in 1:4) {
    
    # initiatlize
    impvars <- sort(allmodsfin[[d]][[m]]$importance[,1], decreasing = T)
    varnames <- names(impvars)
    M <- cor(data[[d]][[m]][,4:81])
    corrdata <- data.frame(M)
    
    # loop through variables
    for (i in 1:78) {
      
      if (nrow(corrdata) == 0) {
        break
      }
      
      checknames <- any(rownames(corrdata) == varnames[i])
      
      # keep rows that meet threshold conditions
      if (checknames == TRUE){
        corrdata[which(rownames(corrdata) == varnames[i]),which(colnames(corrdata) == varnames[i])] <- NA
        corrdata <- corrdata[which(corrdata[which(colnames(corrdata) == varnames[i])] > -0.6 | is.na(corrdata[which(colnames(corrdata) == varnames[i])])),]
        corrdata <- corrdata[which(corrdata[which(colnames(corrdata) == varnames[i])] < 0.6 | is.na(corrdata[which(colnames(corrdata) == varnames[i])])),]
      }
      
    }
    
    # create new dataset with updated variables
    newnames <- rownames(corrdata)
    newdata <- data[[d]][[m]][,newnames]
    
    # store data
    monthdata[[m]] <- newdata
  }
  
  # store data
  alldata[[d]] <- monthdata
}

# second step: rerun analysis with new variables

allmodsfin <- list()
for (d in 1:2) {
  allmods2 <- list()
  for (m in 1:4) {
    # set up 5-fold cross validation
    ndata <- data.frame(data[[d]][[m]][,2],alldata[[d]][[m]])
    
    k <- 5
    n <- nrow(ndata)
    
    set.seed(11)
    newdata <- ndata[sample(n),]
    
    folds <- cut(seq(1,n),breaks = k, labels = FALSE)
    
    for (j in 1:k) {
      testIndex <- which(folds == j, arr.ind = TRUE)
      testData <- newdata[testIndex,]
      trainData <- newdata[-testIndex,]
      
      testData <- na.omit(testData)
      trainData <- na.omit(trainData)
      
      n <- ncol(trainData)
      
      rfmod <- randomForest(trainData[,c(2:n)],trainData[,1], importance = T)
      yhat <- predict(rfmod, testData[,c(2:n)])
    }
    
    # store variables
    allmods2[[m]] <- rfmod
  }
  allmodsfin[[d]] <- allmods2
}

# third step: threshold-based variable selection

finaldata <- list()
for (d in 1:2) {
  newvars <- list()
  for (m in 1:4) {
    impdata <- importance(allmodsfin[[d]][[m]])[,1]
    threshold <- quantile(impdata,.9)
    drops <- c()
    for (i in 1:length(impdata)) {
      if (impdata[i] < threshold)  {
        drops <- append(drops, names(impdata[i]))
      }
    }
    newvars[[m]] <- cbind(data[[d]][[m]][,1:2],alldata[[d]][[m]][,!(names(alldata[[d]][[m]]) %in% drops)])
  }
  finaldata[[d]] <- newvars
}


save(list=c("finaldata"), file="variableselection.rdata")


######## FINAL MODEL RUN #######
setwd(outputdir)
load('variableselection.rdata')

alldatafin <- list()
allmodsfin <- list()
allrsqfin <- list()
allrmsefin <- list()
allnrmsefin <- list()

for (d in 1:2) {
  
  alldata2 <- list()
  allmods2 <- list()
  allrsq <- c()
  allrmse <- c()
  allnrmse <- c()
  
  # loop though seasons
  for (m in 1:4) {
    # loop through folds
    allyhats <- c()
    alltracts <- c()
    allytest <- c()
    rsq <- c()
    rmse <- c()
    nrmse <- c()
    
    # set up 5-fold cross validation
    k <- 5
    n <- nrow(finaldata[[d]][[m]])
    
    set.seed(11)
    newdata <- finaldata[[d]][[m]][sample(n),]
    newdata$consumption <- newdata$consumption/1000000
    
    folds <- cut(seq(1,n),breaks = k, labels = FALSE)
    for (j in 1:k) {
      testIndex <- which(folds == j, arr.ind = TRUE)
      testData <- newdata[testIndex,]
      trainData <- newdata[-testIndex,]
      
      testData <- na.omit(testData)
      trainData <- na.omit(trainData)
      
      n <- ncol(trainData)
      
      rfmod <- randomForest(trainData[,c(3:n)],trainData[,2], importance = T)
      yhat <- predict(rfmod, testData[,c(3:n)])
      
      # store variables
      allyhats <- append(allyhats, yhat)
      alltracts <- append(alltracts, testData[,1])
      allytest <- append(allytest, testData[,2])
      
      # calculate model performance
      rsq[j] <- mean(rfmod$rsq)
      rmse[j] <- mean(sqrt(rfmod$mse))
      nrmse[j] <- mean(sqrt(rfmod$mse)/(max(trainData[,2])-min(trainData[,2])))
    }
    
    # store variables
    alldata2[[m]] <- list(alltracts,allytest, allyhats)
    allmods2[[m]] <- rfmod
    allrsq[m] <- mean(rsq)
    allrmse[m] <- mean(rmse)
    allnrmse[m] <- mean(nrmse)
  }
  alldatafin[[d]] <- alldata2
  allmodsfin[[d]] <- allmods2
  allrsqfin[[d]] <- allrsq
  allrmsefin[[d]] <- allrmse
  allnrmsefin[[d]] <- allnrmse
  
}

save(list=c("alldatafin","allmodsfin","allrsqfin","allrmsefin","allnrmsefin"), file="finalanalysis.rdata")


######## FIGURES #######
setwd(outputdir)
load('finalanalysis.rdata')

seasnames <- c('Spring','Summer','Fall','Winter')
labelnames <- list(c('Some College Eduction','Families w/ Kids','House Value < $50k','Home Ownership','Walk to Work'),
                   c("Associate's Degree",'Income $75k-$100k','Families w/ Kids','House Value $50k-$100k','Home Ownership','Aged 50-64'),
                   c('Some College Education','Families w/ Kids','House Value $50k-$100k','Home Ownership','Aged 50-64'),
                   c('Some College Education','Family w/ Kids','House Value < $50k','Home Ownership','Take Public Transportation to Work'))
labelnames2 <- list(c('Income $75k-$100k','Native Language: European','Native Language: Asian','Married Families','Black People'),
                    c('Income $100k-$150k','Families w/ Kids','Single People','Marital Status: Separated','Work from Home'),
                    c('Income $100k-$150k','Families w/ Kids','Marital Status: Separated','Marital Status: Other','Asian Immigrants'),
                    c('Income $50k-$75k','Income $100k-$150k','Families w/ Kids','Mobile Home','Drive to Work'))


p <- list()
for (i in 1:4) {
  imp <- as.data.frame(varImpPlot(allmodsfin[[1]][[i]], type=1))
  imp$varnames <- labelnames[[i]]
  
  p[[i]] <- print(ggplot(imp, aes(x=reorder(varnames, `%IncMSE`), y=`%IncMSE`)) + 
    geom_point(size = 3.5) +
    geom_segment(aes(x=varnames,xend=varnames,y=0,yend=`%IncMSE`), size=1.5) +
    ylab("% Increase in MSE\nif Variable is Removed") + xlab('') + ggtitle(paste(seasnames[i])) +
    coord_flip() + theme_light() +theme(text = element_text(size=18)))
}

plot_grid(p[[1]],p[[2]],p[[3]],p[[4]],align = 'v',nrow = 2)

# edit geoid column
for (i in 1:4) {
  alldatafin[[1]][[i]][[1]] <- gsub("14000US","",alldatafin[[1]][[i]][[1]])
  alldatafin[[2]][[i]][[1]] <- gsub("14000US","",alldatafin[[2]][[i]][[1]])
}

p <- list()
for (i in 1:4) {
  mondata <- data.frame(alldatafin[[1]][[i]][[1]], alldatafin[[1]][[i]][[2]], alldatafin[[1]][[i]][[3]], ((alldatafin[[1]][[i]][[3]])-(alldatafin[[1]][[i]][[2]])))
  
  mondata <- aggregate(mondata[,2:4], by=list(mondata[,1]), FUN=mean)
  names(mondata) <- c("id", "actualvalues", "predvalues", "anomaly")
  
  # using ggplot to plot spatial data
  
  ggtract <- fortify(tract, region = "GEOID") 
  ggtract <- left_join(ggtract, mondata, by=c("id")) # join tabular data
  
  if (i == 1 | i == 3) {
    p[[i]] <- print(ggplot() +
                      geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=anomaly), color="grey50") +
                      scale_fill_gradientn(colors = c("cadetblue", "white", "red"), values = c(1,0.5, 0), name = 'Difference\n(mill. gal.)',limits=c(-4,4)) + 
                      theme_light() + theme(text = element_text(size=18)) + xlab("Longitude (deg. E)") + ylab("Latitude (deg. N)") +
                      #ggtitle(paste('Anomalies in ',seasnames[i],' Water Use')) + 
                      ggtitle(paste(seasnames[i])) + 
                      theme(legend.position = "none"))
                      #theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(),
                            #axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
                            #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank()))
  } else {
    p[[i]] <- print(ggplot() +
                      geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=anomaly), color="grey50") +
                      scale_fill_gradientn(colors = c("cadetblue", "white", "red"), values = c(1,0.5, 0), name = 'Difference\n(mill. gal.)',limits=c(-4,4)) + 
                      theme_light() + theme(text = element_text(size=18)) + xlab("Longitude (deg. E)") + ylab("Latitude (deg. N)") +
                      ggtitle(paste(seasnames[i])))   
                      #theme(legend.position = "none") +
                      #theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(), 
                            #axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), panel.grid.major = element_blank(), 
                            #panel.grid.minor = element_blank(), panel.border = element_blank()))
                      

  }
}

plot_grid(p[[1]],p[[2]],p[[3]],p[[4]],align = 'v',nrow = 2, rel_widths = c(3/7,4/7))

# correlation plot
load('alldata.rdata')
cormat <- cor(finaldata[[1]][,14:75])
par(cex=.7)
corrplot(cormat)

# partial dependence plots - moderate intensity
load('variableselection.rdata')
par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[1]][[1]], finaldata[[1]][[1]][,3:7],ownhouse, 
            xlab = "Percentage of Owned Houses", ylab = "Water Use (gal.)", 
            main = '')
partialPlot(allmodsfin[[1]][[1]], finaldata[[1]][[1]][,3:7],families, 
            xlab = "Percentage of Families w/ Kids", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[1]], finaldata[[1]][[1]][,3:7],houseless50k, 
            xlab = "Percentage of Houses Valued < $50k", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[1]], finaldata[[1]][[1]][,3:7],walk, 
            xlab = "Percentage of Pop. that Walks to Work", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[1]], finaldata[[1]][[1]][,3:7],somecollege, 
            xlab = "Percentage of Pop. w/ Some College", ylab = "Water Use (gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Spring Months", side = 3, line = -2, outer = TRUE)

par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[1]][[2]], finaldata[[1]][[2]][,3:8],ownhouse, 
            xlab = "Percentage of Owned Houses", ylab = "Water Use (mill. gal.)", 
            main = '')
partialPlot(allmodsfin[[1]][[2]], finaldata[[1]][[2]][,3:8],house50to100k, 
            xlab = "Percentage of Houses Valued $50-100k", ylab = "Water Use (mill. gal.)", 
            main = '')
partialPlot(allmodsfin[[1]][[2]], finaldata[[1]][[2]][,3:8],income75to100k, 
            xlab = "Percentage of Pop. w/ Income $75-100k", ylab = "Water Use (mill. gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[2]], finaldata[[1]][[2]][,3:8],pop50to64, 
            xlab = "Percentage of Pop. Aged 50-64", ylab = "Water Use (mill. gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[2]], finaldata[[1]][[2]][,3:8],associates, 
            xlab = "Percentage of Pop. w/ Assoc. Degree", ylab = "Water Use (mill. gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[2]], finaldata[[1]][[2]][,3:8],families, 
            xlab = "Percentage of Families w/ Kids", ylab = "Water Use (mill. gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Summer Months", side = 3, line = -2, outer = TRUE)

par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[1]][[3]], finaldata[[1]][[3]][,3:7],ownhouse, 
            xlab = "Percentage of Owned Houses", ylab = "Water Use (gal.)", 
            main = '')
partialPlot(allmodsfin[[1]][[3]], finaldata[[1]][[3]][,3:7],somecollege, 
            xlab = "Percentage of Pop. w/ Some College", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[3]], finaldata[[1]][[3]][,3:7],house50to100k, 
            xlab = "Percentage of Houses Valued $50-100k", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[3]], finaldata[[1]][[3]][,3:7],families, 
            xlab = "Percentage of Families w/ Kids", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[3]], finaldata[[1]][[3]][,3:7],pop50to64, 
            xlab = "Percentage of Pop. Aged 50-64", ylab = "Water Use (gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Fall Months", side = 3, line = -2, outer = TRUE)

par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[1]][[4]], finaldata[[1]][[4]][,3:7],ownhouse, 
            xlab = "Percentage of Owned Houses", ylab = "Water Use (gal.)", 
            main = '')
partialPlot(allmodsfin[[1]][[4]], finaldata[[1]][[4]][,3:7],publictransit, 
            xlab = "Percentage of Pop. Uses Public Trans.", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[4]], finaldata[[1]][[4]][,3:7],families, 
            xlab = "Percentage of Families w/ Kids", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[4]], finaldata[[1]][[4]][,3:7],somecollege, 
            xlab = "Percentage of Pop. w/ Some College", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[1]][[4]], finaldata[[1]][[4]][,3:7],houseless50k, 
            xlab = "Percentage of Houses Valued < $50k", ylab = "Water Use (gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Winter Months", side = 3, line = -2, outer = TRUE)

# high intensity plots

par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[2]][[1]], finaldata[[2]][[1]][,3:7],asianlang, 
            xlab = "Percentage of Pop. w/ Native Asian Lang.", ylab = "Water Use (gal.)", 
            main = '')
partialPlot(allmodsfin[[2]][[1]], finaldata[[2]][[1]][,3:7],married, 
            xlab = "Percentage of Married People", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[1]], finaldata[[2]][[1]][,3:7],europeanlang, 
            xlab = "Percentage of Pop. w/ Native Euro. Lang.", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[1]], finaldata[[2]][[1]][,3:7],income75to100k, 
            xlab = "Percentage of Pop. w/ Income $75-100k", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[1]], finaldata[[2]][[1]][,3:7],black, 
            xlab = "Percentage of Black People", ylab = "Water Use (gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Spring Months", side = 3, line = -2, outer = TRUE)

par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[2]][[2]], finaldata[[2]][[2]][,3:7],families, 
            xlab = "Percentage of Families w/ Kids", ylab = "Water Use (gal.)", 
            main = '')
partialPlot(allmodsfin[[2]][[2]], finaldata[[2]][[2]][,3:7],separated, 
            xlab = "Percentage of Separated People", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[2]], finaldata[[2]][[2]][,3:7],single, 
            xlab = "Percentage of Single People", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[2]], finaldata[[2]][[2]][,3:7],workfromhome, 
            xlab = "Percentage of Pop. that Works from Home", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[2]], finaldata[[2]][[2]][,3:7],income100to150k, 
            xlab = "Percentage of Pop. w/ Income $100-150k", ylab = "Water Use (gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Summer Months", side = 3, line = -2, outer = TRUE)


par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[2]][[3]], finaldata[[2]][[3]][,3:7],families, 
            xlab = "Percentage of Families w/ Kids", ylab = "Water Use (gal.)", 
            main = '')
partialPlot(allmodsfin[[2]][[3]], finaldata[[2]][[3]][,3:7],income100to150k, 
            xlab = "Percentage of Pop. w/ Income $100-150k", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[3]], finaldata[[2]][[3]][,3:7],separated, 
            xlab = "Percentage of Separated People", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[3]], finaldata[[2]][[3]][,3:7],asiapob, 
            xlab = "Percentage of Asian Immigrants", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[3]], finaldata[[2]][[3]][,3:7],othermstat, 
            xlab = "Percentage of Pop. w/ 'Other' Marital Status", ylab = "Water Use (gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Fall Months", side = 3, line = -2, outer = TRUE)

par(mfrow = c(2,3), lwd = 2,cex=1)
partialPlot(allmodsfin[[2]][[4]], finaldata[[2]][[4]][,3:7],families, 
            xlab = "Percentage of Families w/ Kids", ylab = "Water Use (gal.)", 
            main = '')
partialPlot(allmodsfin[[2]][[4]], finaldata[[2]][[4]][,3:7],income50to75k, 
            xlab = "Percentage of Pop. w/ Income $50-75k", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[4]], finaldata[[2]][[4]][,3:7],singlecar, 
            xlab = "Percentage of Pop. that Drives to Work", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[4]], finaldata[[2]][[4]][,3:7],income100to150k, 
            xlab = "Percentage of Pop. w/ Income $100-150k", ylab = "Water Use (gal.)",
            main = '')
partialPlot(allmodsfin[[2]][[4]], finaldata[[2]][[4]][,3:7],mobilehome, 
            xlab = "Percentage of Pop. w/ Mobile Homes", ylab = "Water Use (gal.)",
            main = '')
mtext("Partial Dependence Plots for Important Variables in the Winter Months", side = 3, line = -2, outer = TRUE)

# plotting specific census variables
setwd(datadir2)
load('censusdata.rdata')

mondata <- data.frame(alldatafin[[1]][[2]][[1]], alldatafin[[1]][[2]][[2]])
mondata <- aggregate(mondata[,2], by=list(mondata[,1]), FUN=mean)
names(mondata) <- c("id", "consumption")

censusdata[,1] <- gsub("14000US","",censusdata[,1])
specvariable <- data.frame(censusdata$geoid,censusdata$house50to100k, censusdata$incomeless20k + censusdata$income20to35k + censusdata$income35to50k, censusdata$walk)
names(specvariable) <- c('id', 'house50to100k', 'incomeless50k', 'walktowork')

combdata2 <- left_join(mondata, specvariable, by = c('id'))

ggtract <- fortify(tract, region = "GEOID") 
ggtract <- left_join(ggtract, combdata2, by=c("id")) # join tabular data


p1 <- ggplot() + geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=consumption), color="grey50") +
  scale_fill_gradient(high = '#08306b', low = '#f7fbff', name = 'Mill. Gal.',limits=c(0,10)) + 
  theme_light() + theme(text = element_text(size=18)) +
  ggtitle('Summer Water Consumption') +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

p2 <- ggplot() + geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=house50to100k), color="grey50") +
  scale_fill_gradientn(colors = c("cadetblue", "white", "#f2b195"),values = c(1,.3,0), name = '%',limits=c(0,100)) + 
  theme_light() + theme(text = element_text(size=18)) +
  ggtitle('Houses Valued $50-100k') +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

p3 <- ggplot() + geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=incomeless50k), color="grey50") +
  scale_fill_gradient(high = '#00441b', low = '#f7fcf5', name = '%',limits=c(20,100)) + 
  theme_light() + theme(text = element_text(size=18)) + 
  ggtitle('Household Income < $50k') + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())

p4 <- ggplot() + geom_polygon(data = ggtract , aes(x=long, y=lat, group = group, fill=walktowork), color="grey50") +
  scale_fill_gradient(high = '#3f007d', low = '#fcfbfd', name = '%',limits=c(0,30)) + 
  theme_light() + theme(text = element_text(size=18)) + 
  ggtitle('Population that Walks to Work') +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())


plot_grid(p1,p4,p2,p3,align = 'v',nrow = 2)


