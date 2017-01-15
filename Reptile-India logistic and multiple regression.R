require(rinat)
library(bdvis)
library(randomForest)
library(raster)
library(bdvis)
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

##Now we will Principle Component Analysis,to remove the highly correlated predictors
##Then will apply different regression models.

library(caret)

nearZeroVar(d[,2:20])
View(d)
# When predictors should be removed, a vector of integers is
# returned that indicates which columns should be removed.
correlations <- cor(d[,2:20])
dim(correlations)

library(corrplot)
corrplot(correlations, order = "hclust")

##Correlation barrier
highCorr <- findCorrelation(correlations, cutoff = .75)

length(highCorr)
print(highCorr)

##Now we will remove the highly correalted parameters
##Removing the columns with id:- 6 11  4  7  9  1 12  8 16  5 17

##Now using simple multiple regession model 
x<-lm(pa~bio2+bio3+ bio10+ bio13+ bio14+bio3+bio15+ bio18 + bio19,d)
summary(x)

##Now logistic regression is applied below

model <- glm(pa~bio2+bio3+ bio10+ bio13+ bio14+bio3+bio15+ bio18 + bio19,family=binomial(link='logit'),data=d)
summary(model)
plot(model)



##Now we will analyse the model parameters using anova and chi test
anova(model, test="Chisq")


##The difference between the null deviance and the residual deviance shows how
##our model is doing against the null model (a model with only the intercept).
##The wider this gap, the better
