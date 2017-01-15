require(rinat)
library(bdvis)
library(randomForest)
library(raster)
library(maptools)
inat=get_inat_obs_project("reptileindia") 

##plotting the species on the india map.
## inat[,16:15]=longitude and lattitude
plot(inat[,16:15], cex=0.5, col='blue')
data(wrld_simpl)
plot(wrld_simpl, add=TRUE)

##World climate data
wc <- getData('worldclim', res=10, var='bio')

## Now extracting climate data for our species
bfc <- extract(wc, inat[,16:15])
## Bio1-bio19 are climatic parameters which influence the species survival.
head(bfc)



##For ecological niche species
i <- which(is.na(bfc[,1]))
print(i)


##Ploting the two important parameters.
plot(bfc[ ,'bio1'] / 10, bfc[, 'bio12'], xlab='Annual mean temperature (C)',
     ylab='Annual precipitation (mm)')



##Now as our data contains only 748 points so we apply Machine Learning algorithms
##we may get biased results,so we increase the data by dynamic resampling.


##Removal NA values for analysis.
inat1<-inat[-i,]

##Getting the range of the values
e <- extent(SpatialPoints(inat1[, 16:15]))
print(e)


## Now generating 1000 points by resampling
set.seed(0)
bg <- sampleRandom(wc, 1000, ext=e)
dim(bg)

##Setting the actual data value class as 1 and generated data class as 0
d <- rbind(cbind(pa=1, bfc), cbind(pa=0, bg))
d <- data.frame(d)
dim(d)


## Now our data sample is ready for application of algorithms

##First applying CART model
library(rpart)
cart <- rpart(pa~., data=d)
printcp(cart)

##This model will classify the data according to the most significant parameters

## Ploting of the rpart model.
plotcp(cart)

plot(cart, uniform=TRUE, main="Regression Tree")
text(cart, cex=.8)

## From the above model we can easily determine the conditions for proper survival of the species

## Above we used 1000 data points,but we can different amount of data points for
##better classification

