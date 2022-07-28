###########################
## NSF Summer Institutes
## Tutorial
## Phylogenetic signal in R
## Fiona Jordan 
###########################

library(ape)
library(phytools)
library(geiger)
library(phangorn)
library(ggplot2)
library(caper)
library(readxl)
library(usedist)

# set working directory, remember yours will be different

setwd("~/Dropbox/Teaching/Non-Bristol/Summer Schools/2022 NSF-HRAF Summer Institute/Materials/8b PCM2/phylosignal/") 

ls() # what do you have in your workspace?
rm(list=ls()) # if you're like me you like to clear out random stuff

##################################################
## Section 1: Visualise some data with NeighborNet
##################################################

# These data are from 
# Polynesian hafted adzes: A comparative study of form and decoration
# Sidney Mead (1968)
# PhD Dissertation, Southern Illinois University

# They consist of binary-coded data on traits of adzes, across 18 Polynesian societies

adze<-read.nexus("./Data/phylogenies/adzes/a18.adze.nex") #read in NEXUS file 
adze

# There are two trees in here already! A NEXUS file can contain both data/characters and trees. Look at the file in a text editor to see what is going on and what the characters are. 

# There is a maximum-consensus tree for the adze data and of a separate language tree. We can call it with

plot.phylo(adze$MCT.ADZE)

# Let's make it tidy with ladderize!

plot.phylo(ladderize(adze$MCT.ADZE))

# This is ... not a great tree, if you know about Polynesia. 
# It's not terrible, but there are weird things going on (Rapanui and Tongan are really not sister taxa). You can compare it with the language phylogeny.

plot.phylo(ladderize(adze$MCT.LANG))

# To use NeighborNet (in the phangorn package) we need to turn our data into a distance matrix.
# Read in the data from an excel sheet (or csv, tab, whatever -- this is just to give you some extra code to riff off!)

read_excel(path = "./Data/phylogenies/adzes/polynesian_adzes.xls", sheet = "matrix", col_names = FALSE, na = "NA") -> adzedata

adzedata

# Use a method to turn binary data into distances. 

dist(adzedata, method = "bin", ) -> dm
# You got some NAs through coercion. As in life we don't necessarily like coercion, but let's ignore for now.
# What does this matrix look like?

dm

# We know we have 18 societies but we can't remember them, so let's add some labels to our matrix

labels <- c("Kapingamarangi", "Hawaiian", "Mangaian", "Mangarevan", "Maori", "Marquesan", "Niue", "Rapanui", "Samoan", "Tahitian", "Tongan", "Tongarevan", "EastFutuna", "EastUvea", "Tuvalu", "Rennellese", "Tikopia", "Rotuman")

dist_setNames(d = dm, nm = labels) -> dm
dm

# Much better, just like reading travelling distances before the invent of Satnav!
# You can play around with different ways of estimating a matrix if you like.

# The NeighborNet implementation in R is pretty rudimentary and for categorical data I would recommend the standalone program, but for the Summer Institute this is fine.

# Make a NeighborNet

nnet <- neighborNet(dm)
plot(nnet,show.tip.label = TRUE, type = "2D")

# What does your plot look like? Compare it with the adze.mcc.tree.
# What taxa are grouped together?
# It looks like Mangaian and Tongarevan cause some conflicting signal. What could you think to do in order to examine that? 

################################################################
## Section 2: Calculate phylogenetic signal of a continuous trait
################################################################

# Read in the Austronesian tree

AN <- read.nexus(file = "./Data/phylogenies/Austronesian97.mcc.tree")

plot(AN)

# read in data on speaker population size (logged so we're not dealing with a massive range), and number of nodes

read.delim("./Data/phylogenies/AN97_lnpop.txt", row.names = 1) -> popdata
popdata[AN$tip.label,] -> popdata #order the data frame by the order of tree tiplabels
popdata

# Make sure our rownames are our taxa

lnpop <- setNames(popdata[,1], rownames(popdata))
name.check(AN, lnpop) # sanity check on matching tree and trait
lnpop

# plot Tree 

plot.phylo(AN)

# Now let's estimate the maximum likelihood value (i.e. the best value) of lambda for the data on this tree

lambda_pop_est <-fitContinuous(phy = AN, dat = lnpop, model = "lambda")
lambda_pop_est                           
                           
# To understand the output:lambda = Pagel's lambda; sigsq = Brownian rate parameter; AIC = Akaike Information Criterion; AICc = corrected AIC.
 
# What is the estimated value of lambda?

# 

# What is the log-likelihood?

# 

# In this case, we are interested in whether the value of lambda is different from 0 (no phylogenetic signal) and 1 (trait distribution matches a Brownian model of evolution where the phylogeny is structuring the variation). Here, lambda is intermediate, tending towards 1 - but is it statistically significant? We should test this  using likelihood ratio tests! 

# We could also permute our data for a SeanRoberts (TM) approach (this is kindof what Blomberg's K does (see below line 215))
 
# First we need to get the log likelihood (lnl) if lambda = 0 and if lambda = 1. We can do this by transforming our tree using the geiger function lambdaTree to get a phylogeny where lambda = 0, i.e., a star phylogeny. We then estimate lambda for population size on this tree and compare log likelihoods using likelihood ratio tests. We can also compare the maximum likelihood value of lambda to lambda =1 using the original tree and not estimating lambda (under Brownian motion, lambda = 1).

# Don't worry about the deprecation messages if you get them

AN_L0 <- lambdaTree(AN, lambda = 0)
AN_L1 <- lambdaTree(AN, lambda = 1)

par(mfrow=c(2,1)) # graphical parameter to let us display two trees at a time
plot(AN_L0) # Note this is now a star or rake phylogeny i.e. with no hierarchical information
plot(AN_L1) # Our original phylogeny

par(mfrow=c(1,1)) # back to a single figure

# We can then generate the log likelihoods for this tree and the original tree where lambda = 1, under a model of stochatic change

lambda_pop_0 <-fitContinuous(phy = AN_L0, dat = lnpop, model = "BM")
lambda_pop_1 <-fitContinuous(phy = AN_L1, dat = lnpop, model = "BM")

# take a look at these and note the Lh

lambda_pop_0

lambda_pop_1

# We can then use the log likelihood ratio formula: LR = -2* (log_likelihood_null - log_likelihood_alternative) to compare these three models
 
# We can compare the log likelihood ratio where lambda is 0 to where it is allowed to take its maximum likelihood value (our first analysis):

LLR0 <- -2*(lambda_pop_0$opt$lnL - lambda_pop_est$opt$lnL)

# What is this? What does it mean?

LLR0

# We can also compare the log likelihood ratio of where lambda is 1 to where it is allowed to take its maximum likelihood value. Can you figure this one out? Answer at the end in case you are struggling.

LLR1 <- -2*(lambda_pop_1$opt$lnL - lambda_pop_est$opt$lnL)

LLR1

# Then you can get a p-value from the χ2 squared distribution:

pchisq(LLR0, df=1,lower.tail = FALSE)

pchisq(LLR1, df=1,lower.tail = FALSE)
 
# What are your p values? What do you think they mean?

# The fitContinuous() function also allows you to fit other transform models such as delta or kappa to investigate the model of evolution. 

# Kappa (κ) transform: Each branch length in the tree is raised to the power kappa (Pagel 1999). Kappa = 0 is a speciational or punctuated model of evolution where all branch lengths are equal. 
# Kappa = 1 is a Brownian motion model (so the tree is returned unchanged).
# The syntax is the same. Try fitting Kappa in the same way as lambda.

kappa_pop_est <-fitContinuous(phy = AN, dat = lnpop, model = "kappa")
kappa_pop_est

# Look at a tree transformed by kappa = 0

AN_K0 <- kappaTree(AN, kappa = 0)

plot(AN_K0) # 
plot(AN_L1) # Our original phylogeny

# Try kappa = other values (they must be positive)

AN_K5 <- kappaTree(AN, kappa = 5)
plot(AN_K5)

# Then try delta - remember to first see what it does to a tree

AN_D05 <- deltaTree(AN, delta = 0.5)
plot(AN_D05)

# Finally, because these measures are useful, they are implemented in other packages like phylosig 

phylosig(AN, lnpop, method="lambda",test=TRUE)

# Oh wow that was much more user friendly! You know what that one line of code is actually doing.
# You know about comparing likelihoods, what lambda does to a tree, and practiced some R skills, so it was worth it :)

# You can estimate signal with Blomberg's K here too. 
# This is a randomisation approach that randomly assigns the trait values to the species and then calculates K. 
# This is repeated 1000 times (if reps = 1000) and the observed value of K is then compared to the randomised values to determine its significance.

phylosig(AN, lnpop, method = "K" ,test=TRUE)


##################################################
## Section 3: Calculate phylogenetic signal of a categorical trait
##################################################

# Let's return to our data on land tenure taken from Kushnick, Gray & Jordan (2014)
# Here we were trying to model how the ownership (or not) of land might have evolved in Austronesian societies
# lt_main gives the main type of land tenure for each society; the others are "no ownership", "group", "kin group", "individuals". Some societies have several types of land tenure present.

AN <- read.nexus(file = "./Data/phylogenies/Austronesian97.mcc.tree")
read.delim("./Data/phylogenies/lt2.trait") -> ltdata
head(ltdata)

# The caper package contains Fritz and Purvis's "phylo.D", used for estimating the phylogenetic signal of a binary trait.

# We need to make a special "comparative data" objects first

comparative.data(phy = AN, data = ltdata, names.col = soc, na.omit = FALSE, warn.dropped = TRUE) -> cd.ltAN

# What is this? It's a composite object - our tree combined with our trait data.

cd.ltAN

# We run our phyloD test on one of our binary variables contained in the cd.ltAN object  

# lt_main: multistate coding where {0123} correspond as above and societies are coded for the MAIN form of land tenure (don't use this column!)
# lt_none: binary coding 1 = no land tenure 0 = some form of land tenure 
# lt_group: binary coding 1 = group land tenure 0 = absent
# lt_kin: binary coding 1 = kin land tenure 0 = absent
# lt_indiv: binary coding 1 = individual land tenure 0 = absent
# here's one for you to see

kin = phylo.d(data=cd.ltAN, binvar = lt_kin, permut = 1000)

# Take a look to see what's going on here

kin

# In words what do you think is going on? 
# Now you do the rest and see what you find!
# Does altering the number of permutations make a difference?
# What would you do to incorporate more uncertainty here?
# Remember you can peek at the end if you are struggling with code















### ANSWERS

LLR1 <- -2*(lambda_pop_1$opt$lnL - lambda_pop_est$opt$lnL)


none = phylo.d(data=cd.ltAN, binvar = lt_none, permut = 1000)
indiv = phylo.d(data=cd.ltAN, binvar = lt_indiv, permut = 1000)
group = phylo.d(data=cd.ltAN, binvar = lt_group, permut = 1000)

# kin: D (0.17) is significantly less than 1 (so is not randomly structured) but not significantly different from the Brownian expectation (D = 0).


