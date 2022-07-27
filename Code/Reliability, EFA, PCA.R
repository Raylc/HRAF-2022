#Packages that you will need in this session
library(stats)      #used to determine eigenvalues in the EFA
library(nFactors)   #used to do the parallel analysis in the EFA
library(psych)      #used to calculate reliability
library(factoextra) #used to do the PCA
library(missMDA)    #used to impute scores into the no missingness PCA data frame

d<-read.csv("./Data/domains.csv")

#Let's start by looking at variable distributions
hist(d$Socialization)
hist(d$Marriage)
hist(d$Sexuality)
hist(d$FuneralsMourning)
hist(d$Gender)
hist(d$LawEthics)

#Compute correlation matrix
cor(d,use = "complete.obs")  
#do you see clusters of variables that seem to belong together, or do these variables have uniformly high correlations?

#Reliability
psych::alpha(d)
#raw_alpha and std.alpha represent the internal consistency of the variables
#average_r is the average inter-item correlation
#more information about parameters is available here: https://www.rdocumentation.org/packages/psych/versions/2.1.6/topics/alpha

#Factor analysis
ev <- eigen(cor(d,use="complete.obs"))              # this line gets eigenvalues from your covariance matrix. "complete.obs" means that you are using casewise instead of listwise deletion of missing data
ap <- parallel(subject=nrow(d),var=ncol(d),         # this line acquires eigenvalues 
               rep=100,cent=.05)
nS <- nScree(x=ev$values)
plotnScree(nS)                                      #this plot gives you the ideal number of factors according to different methods. Eigenvalues and parallel analysis are the two most common, but you can read about optimal coordinates and acceleration factor here (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5518610/). Both techniques are essentially trying to determine when the slopoe of a scree plot changes most 

d2<-na.omit(d)                                      #these next few lines require complete cases, so we'll delete cases with missing data

fit <- factanal(d2, factors=2, rotation="promax")  #change the rotation strategy to "promax" for an oblique rotation (factors that are allowed to correlate). 
print(fit, digits=2, cutoff=0, sort=TRUE)           #parallel analysis is another method of determining the optimal number of factors, see here for an explanation: https://stats.idre.ucla.edu/stata/faq/how-to-do-parallel-analysis-for-pca-or-factor-analysis-in-stata/
# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n")                                 #these next two lines plot out the variables in a 2-D space (that matches the (the two strongest factor loadings)
text(load,labels=names(d2),cex=.7) 


#PCA
res.pca <- prcomp(d2, scale = TRUE)                 #the scale = true argument standardizes the data
summary(res.pca)                                    #this line gives you the percent variance explained by each PC, and the cumulative % explained by adding the most explanatory PCs
fviz_eig(res.pca)                                   #visualize the PCs by variance explained

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
pcs<-get_pca(res.pca, "ind") 
#this last line actually allows you to save out the data from your PC analysis so that you can add the factors to your dataset. The list object saved on this line has three properties. Two of these are important to explain:
#1. the first property is the scores of each society on the PCs. Which societies are highest on PC1?
#2. the second property is the cos2 weight for each society, which shows the importance of each PC for determining the score for the society

#Adding factors and PCs to the data

#Factors

#We just average together the indicators of each factor to create their factor score
d$Factor1<-rowMeans(data.frame(d$Socialization,d$Marriage,d$Sexuality,d$FuneralsMourning,d$Gender,d$LawEthics),na.rm=T)

#PCs

#to add PCs, we'll begin by reproducing the PCA on a complete dataset with no missing values
imputed<-imputePCA(d,ncp=1)      #The imputePCA function will create a dataset with full values using the information from a PCA. The "ncp" argument specifies how many factors you want to use during imputation
d3<-as.data.frame(imputed$completeObs)

#Then refit the PCA
res.pca <- prcomp(d3, scale = TRUE)
pcs<-get_pca(res.pca, "ind") 

#Finally, save out the scores and add them to the dataset
d$PC1<-pcs$coord[,1]   #if you wanted to save more than the first PC, just edit the "1" inside of the square brackets to be a different number

#How correlated is our factor and our PC?
cor.test(d$PC1,d$Factor1) #this correlation won't be as high in more complex factor structures
