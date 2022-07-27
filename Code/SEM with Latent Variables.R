#Packages that you will need to run this script
library(lavaan)
library(semPlot)

#The supplementary codebook provides definitions of all the variables

##Some lavaan terminology
# ~ is a regression
# ~~ is a covariance
# =~ defining a factor 
# := defines a parameter

##Some notes about SEM fit
#RMSEA should be under .05
#TLI should be over .95
#CFI should be over .95
#Chi squared should be non-significant

d<-read.csv("./Data/semdat.csv")

#Here is a basic CFA

model1 <- "TL =~ Socialization + Marriage + Sexuality + FuneralsMourning + Gender + LawEthics" #lavaan works through formulas, so you see here that the factor "TL" is defined through the indicators (domains)

fit1 <- cfa(model = model1, data = d, missing = "ML")       ##the cfa function takes the model that was defined above, and "ML" is a method of imputing missing data under the assumption that it is missing at random
summary(fit1, fit.measures = T, standardized = T,rsquare=T) #standardized = T means that loadings are standardized and can be compared to each other

semPlot::semPaths(fit1)                                     #this is a (not ideal) package that can visualize CFA models

#Incorporating exogenous variables

model2 <- "TL =~ Socialization + Marriage + Sexuality + FuneralsMourning + Gender + LawEthics

           TL ~ complexity"                                  #after defining the factor, we can now use it in regressions. Here we have a model where "tightness" is regressed on "social complexity" 

fit2 <- cfa(model = model2, data = d, missing = "ML")
summary(fit2, fit.measures = T, standardized = T,rsquare=T)

semPlot::semPaths(fit2)

#Here is a model with poor fit because of an unspecified link (Law and ethics wants to be part of the tightness factor rather than an independent predictor)

model2 <- "TL =~ Socialization + Marriage + Sexuality + FuneralsMourning + Gender
           LawEthics ~ complexity"

fit2 <- cfa(model = model2, data = d, missing = "ML")
summary(fit2, fit.measures = T, standardized = T,rsquare=T)

semPlot::semPaths(fit2)

modindices(fit2) #You can see from the modification indices that law and ethics wants to covary with the other domains of tightness

#New models

#Try your own models here
model3 <- "TL =~ Socialization + Marriage + Sexuality + FuneralsMourning + Gender + LawEthics

           TL ~ Authoritarianism"                                  #after defining the factor, we can now use it in regressions. Here we have a model where "tightness" is regressed on "social complexity" 

fit3 <- cfa(model = model3, data = d, missing = "ML")
summary(fit3, fit.measures = T, standardized = T,rsquare=T)

semPlot::semPaths(fit3)

