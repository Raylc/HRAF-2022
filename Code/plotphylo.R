###########################
## NSF Summer Institutes
## Tutorial
## Plotting phylogenies in R
## Fiona Jordan & Catherine Sheard
###########################
## Phylogenetic tree originally from Gray et al 2009, used in Kushnick et al 2014
###########################

library(ape)
library(phytools)
library(geiger)
library(phangorn)
library(ggplot2)

# set working directory, remember yours will be different

setwd("~/Dropbox/Teaching/Non-Bristol/Summer Schools/2022 NSF-HRAF Summer Institute/Materials/8a PCM1/Plotting Phylogenies")

ls() # what do you have in your workspace?
rm(list=ls()) # if you're like me you like to clear out random stuff

########################################
## Section 1: reading and writing trees
########################################

# The purpose of this tutorial is give you tools for understanding how R treats phylogenies

AN<-read.nexus("./Data/phylogenies/Austronesian97.mcc.tree") #read in a NEXUS file of an Austronesian phylogenetic tree
# what is a NEXUS file? https://en.wikipedia.org/wiki/Nexus_file  
plot(AN)

# A barely functional plot
# Want to know what mcc stands for? Internet search "maximum clade credibility" tree
# To start with, what is 'AN'?

AN

# AN is an object of class 'phylo'. Typing in just the name of the tree is a good way to check that you're working with the tree you think you are!

# Now let's explore what it means to be an object of class 'phylo'.

names(AN) # what makes up a phylo object? 
# Gives us four parts of the object: edge, Nnode, tip.label, edge.length. Each of these have data associated.

AN$edge

# A long matrix. Each tip and node is assigned a number, with tips assigned numbers 1 through N (where N = number of tips). AN$edge lists which nodes/tips are connected by edges (branches, in the vernacular). This is MUCH easier to conceptualise if we look at the simplest tree possible, with three taxa:

abc <- read.tree(text = "((Aphid,Beetle),Caterpillar);") # create a tree with three taxa
abc

plot.phylo(abc, type="clado") #plot the tree
tiplabels()
nodelabels() #plot the node and tip labels on the tree. Tips come first, so Aphid=1, Beetle=2, Caterpillar=3 etc, then the numbering will start from the root
abc$edge #show the matrix 

# You can reconstruct the tree itself  with this matrix, which is sometimes useful for checking things. See if you can work out what is going on. Hint: the rows are the edges. ANYHOW ENOUGH MATHS.

# Below, AN$Nnode lists, unsurprisingly, the number of nodes (which, for a fully-resolved tree like this one, is the number of taxa, minus 1). 

AN$Nnode

# AN$tip.label lists the tip labels (Austronesian language names). This is also often very useful for checking!

AN$tip.label

# AN$edge.length lists the edge lengths (branch lengths), with edges in the same order as AN$edge

AN$edge.length

#This Austronesian tree has edge lengths which are scaled to lexical change, not time. Objects of class "phylo" need not have edge lengths, however:

tinytree <- read.tree(text = "(((A,B),(C,D)),E);") #simulate a small tree; feel free to change the taxa names!
tinytree 
names(tinytree) # see? no edge lengths
plot(tinytree)

# On a tree this small, we easily add in the automatically-assigned tip and node labels:

plot(tinytree, label.offset = 5) # That looks stupid. 'label.offset' gives a scaling parameter that lets you determine how much space there is between the end of the branch and the label. Change it to see what it does. 

# You can use parameters to change the colours and shapes etc of labels - see the help for each function
nodelabels(frame = "circle", col= "black", bg = "pink") #nodes now have labels
tiplabels(frame  = "none", col = "blue", bg = "white", offset = .5) #same for the tips

# Note that node and tip labels are fairly ugly on larger trees:
plot(AN)
nodelabels()
tiplabels() #hideous!

# Let's save our example tree before returning to the Austronesian tree
write.nexus(tinytree, file="./Figure/tinyexampletree.nex") 

# We just saved it as a NEXUS file, to match the format of the Austronesian tree. 
# There are other formats but NEXUS is pretty standard

# You could now open these in another programme such as FigTree, and prepare a figure for publication.
# http://tree.bio.ed.ac.uk/software/figtree/

########################################
## Section 2: manipulating trees
########################################

# Back to our Austronesian tree

# is the tree fully resolved? 
is.binary(AN) #whew

plot(AN,show.tip.label=FALSE) #this turns off the tip labels, which are a bit illegible at this scale
nodelabels()

# We can use R to manipulate the tree. For example, maybe you're only interested in part of the Austronesian phylogeny? 
subsetAN <- extract.clade(AN, 171)  # extract a clade that descends from an arbitrary node number (here, 171). Choose a different number to check.
plot(subsetAN)  #plot this subset

# Figuring out what node you actually want can be messy on larger trees.
# A handy way of figuring out what node you want uses the command 'mrca', which stands for 'Most Recent Common Ancestor'
# The MRCA is the latest possible (i.e., most recent) node that contains the specified tips as a monophyletic clade

randomnode<-mrca(AN)["Kiribati", "Maori"]  # find the MRCA of Kiribati and Maori
randomnode
# This should be 171. Do you see why? Look again at the plot of 'subsetAN'
# These all happen to be Oceanic languages (*hearts*) so lets give the clade an informative name 

oceanic<-extract.clade(AN,randomnode)
plot(oceanic)

#Now you try. Pick some languages from AN$tip.label and see what their smallest possible monophyletic clade looks like using the code above.

AN$tip.label

# We can also remove languages from the tree, for example because we don't have data for them, or because they're inconvenient for our purposes today 

# The drop.tip command is super useful for editing trees 
# Note that the drop.tip command will also work across lists of multiple phylogenies -- class 'multiPhylo' -- using the command lapply.

# perhaps you irrationally hate societies that have long language names?
tipswithlongnames <- c("BolaangMongondow","LepantoSegada","UplandBagobo") # name and shame the offenders

AN.shortnames <- drop.tip(AN, tipswithlongnames) #create phylo object without long names
plot(AN.shortnames,show.tip.label=TRUE) #plot it

# Now you try. What if you want a tree that just shows the languages 'Atayal', 'Sengseng', and 'Choiseul'? 
# You can use the command 'setdiff'. The elements of setdiff(x,y) are those elements in x but not in y.
# Depending on how good your R is, this may be hard to get your head around at first, but don't worry. 

AN.sca <- drop.tip(AN, setdiff(AN$tip.label,c("Sengseng","Choiseul","Atayal")))

# Work from inside out here. 'setdiff' will give us the members of AN that are NOT our 3 languages of interest. 'drop.tip' will then drop these from the tree, and we'll assign the "leftover" bits of the tree to 'AN.sca'
plot(AN.sca)

# What if you sample 10 languages at random (using the command 'sample') and just plot those? 
# HINT: remember that AN$tip.label will list all tips and that there are 97 languages total.

AN.10<-drop.tip(AN,sample(AN$tip.label,87))
plot(AN.10)

# Do it again for another sample. This might be useful if you want to get a sense of how much phylogenetic sampling might affect the trait of interest.

#Let's return to our sample tree for this next one:
plot(tinytree)
nodelabels(frame = "cir") #you can abbreviate "circle" as long as it is unambiguous

# At the moment, it reads top-to-bottom E, D, C, B, A, which can be annoying if you're fussy. Let's try to rotate at a single node.
tinytree1 <- rotateNodes(tinytree, 9)
plot(tinytree1)

# Which node(s) were rotated? That's better but not quite what we want. Let's try this:
tinytree2<- rotateNodes(tinytree, "all")
plot(tinytree2)

# Yay! We can also re-root the tree:

nodelabels()
tinytree3<- root(tinytree, node = 8)
plot(tinytree3)

# Remember how our tree didn't have edge lengths? What happens if we try to re-root a tree that has edge lengths?
plot(oceanic)
nodelabels(frame = "c")
oceanic.PN<-root(oceanic,node=31) #re-root tree at node 31 (happens to be ProtoPolynesian)
plot(oceanic.PN) #plot it, scroll back and forth to see what is different

# If we don't want to deal with node labels, we can also root by a specific taxon:

AN.wrong <- root(AN, "Manus")
plot(AN.wrong)

# This is useful if your analysis contains a known outgroup! (please note Manus is not a realistic outgroup for Austronesian)

# One more piece of phylogeny manipulation, and then we'll move on to the actual graphics. 
# You might have reason to want to know the phylogenetic distance between two languages.
# For example, you might want to know if phylogenetic separation correlates with geographic separation, or differences in cultural traits, etc.
# One way to extract this information uses the command 'cophenetic.'

cophenetic(AN)["Maori", "Kiribati"]

# This gives you an answer in the units of the branch lengths, which may or may not be meaningful in terms of estimated time.
# In this case, branch lengths are equivalent to cognate change and the units are not very interpretable. But they could be!
# The result of 512 could correspond to 512 cognate sets on average distance.
# Running the 'cophenetic' command on an entire tree gives you a matrix of all pairwise distances:

cophenetic(AN)
cophenetic(AN)[1:5,1:5] # gives the first 5x5

########################################
## Section 3: graphics
########################################


# Back to graphics. So far, we have a Austronesian phylogeny that looks like this:
plot(AN,show.tip.label=FALSE)

# The plot.phylo command is really flexible
# For example, we can plot various types of trees

plot.phylo(AN,type="cladogram",show.tip.label=FALSE) 
plot.phylo(AN,type="fan",show.tip.label=FALSE)
plot.phylo(AN,type="radial",show.tip.label=FALSE)
plot.phylo(AN, type="phylogram") #default

# You might want to use the smaller Oceanic tree to play around with plot.phylo options 
# And look at the help for the function help("plot.phylo")

plot.phylo(oceanic, use.edge.length = FALSE, type = "phylo", show.node.label = TRUE, font = 3, cex = 0.7, root.edge = TRUE, label.offset = .5, direction = "up") 
nodelabels(frame = "c", bg = "pink")

# We can change the edge colors/thickness:
plot.phylo(oceanic, edge.color="3", edge.width=2, show.tip.label=FALSE)

# What about plotting the tip states of discrete / categorical characters?
# This is data on land tenure taken from Kushnick, Gray & Jordan (2014)
# Here we were trying to model how the ownership (or not) of land might have evolved in Austronesian societies
# 0: no tenure, 1: group ownership, 2: kin ownership, 3: individual ownership

read.delim("lt2.trait", row.names = 1) -> ltdata # make sure the society names are the row names
ltdata # see what you have

# What does this data look like on the tree?
# Remember we have four states to the variable so we can create a new colour palette
lt.col=c("#D9F03C","#DA4646","#7E2091","#3F8B57") 
# There are many other ways to do this but if you are ~particular~ you might like this because you can be specific

# create a new column that assigns colours based on their state
ltdata$main_col[ltdata$lt_main == 0] = lt.col[1]
ltdata$main_col[ltdata$lt_main== 1] = lt.col[2]
ltdata$main_col[ltdata$lt_main == 2] = lt.col[3]
ltdata$main_col[ltdata$lt_main== 3] = lt.col[4]
ltdata <- ltdata[AN$tip.label,] #order the data frame by the order of tree tiplabels

plot.phylo(AN, no.margin=TRUE, use.edge.length=F, tip.color = ltdata$main_col, cex=0.7, adj=0, font=1, label.offset=4, direction="right")
tiplabels(pch=15, cex=1.2, col=ltdata$main_col, adj= c(2.5))
legend("topleft",
       legend=c("no tenure", "group ownership", "kin ownership", "individual ownership"), pch=c(15), col=lt.col, 
       title="Land Tenure")

# Now try plotting one of the binary traits, use a different symbol (pch, or plotting character) and two different colours. An example with an alternative approach is at the end

ltkin <- as.factor(setNames(ltdata$lt_kin, rownames(ltdata)))
ltkin

# For a quick approach you can use the dotTree function 
# It's a bit messy but gets the job done

dotTree(AN, ltkin)

# Something a bit nicer

ltdata$kincol[ltdata$lt_kin==0] = lt.col[3]
ltdata$kincol[ltdata$lt_kin==1] = lt.col[4]
ltdata

plot.phylo(AN, no.margin=TRUE, use.edge.length=F, cex=0.7, adj=0, font=1, label.offset=4, direction="right")
tiplabels(pch=19, cex=0.8, col = ltdata$kincol, adj= c(2.5))
legend("topleft",
       legend=c("other forms", "kin ownership"), pch=c(19), col=lt.col[3:4], 
       title="Land Tenure: Kin groups")

# Mastering colours and widths etc (graphical parameters) is a whole skill in itself in R. See the gg family of approaches as well. 


#############################################
# ANCESTRAL STATES
############################################

# Because this is an example, let's use as our character the number of letters in the language name, because that's obviously an important cultural trait! (It is not)
# We'll show a couple of ways to infer ancestral states

trait<-nchar(AN$tip.label)
names(trait)<-AN$tip.label
trait

# Reconstruct ancestral state using phytools function 'fastAnc'
x<-contMap(AN,trait,plot=FALSE) 

# This is a function for CONTINUOUS data

x # what have we created? 
# what is the range of variation for language name length?

plot(x)

# This gives us nonsense but you get the gist. We don't often have continuous characters in cultural data.

# Let's try plotting the ancestral states of CATEGORICAL (discrete) characters.
# We start by simulating a trait. We'll do some real data as well but this code might be useful for you in the future, particularly when designing a pipeline.

sims<-sample(c("S1","S2","S3"), AN$Nnode+1, replace=TRUE) 
# This is simulating a character (S1-S3) at random, with no regard for phylogeny.
names(sims)<-AN$tip.label #this makes a little data frame
sims # take a look to see what you have

# We can first plot the characters on the tips

plot(AN,type="phylo", show.tip.label=FALSE)
# setting a colour palette with a number of colours equal to the states of our data in "sims"
cols<-setNames(palette()[1:length(unique(sims))],sort(unique(sims))) 
cols
tiplabels(pie=to.matrix(sims,sort(unique(sims))),piecol=cols,cex=0.4)
add.simmap.legend(colors=cols) #click where you want to draw your legend! I don't normally recommend interactive commands in R except for exploratory work.

# ER here stands for 'equal-rates'
# ACE can estimate ancestral states under a few other models e.g. "ARD" means All Rates Different, and the packages 'diversitree' has even more flexibility. 
# But this is randomly distributed data so what do you think will be the estimate for S1,S2,and S3 at the root?

fitER <- ace(sims, AN, type = "discrete")
fitER

# Were you right?

# Let's have some data simulated under a proper model of evolution, THEN we can estimate ancestral states probabilities for each node.
#If you want to simulate some discrete traits with phylogenetic signal, you can use this code:

q<-list(rbind(c(-.5, .5), c(.5, -.5))) #this is a matrix of rates of evolution from state 1 to state 2.
q
sims<-sim.char(AN, q, model="discrete", n=1) # simulates one character under the discrete model of evolution described by q, over the phylogeny AN 
sims

# We can inspect the estimated probabilities of each trait for each node.

fitER <- ace(sims, AN, type = "discrete")
fitER
fitER$lik.anc

# re-plot our tree
plot(AN,type="phylo",show.tip.label=FALSE)
cols<-setNames(palette()[1:length(unique(sims))],sort(unique(sims))) 
cols
tiplabels(pie=to.matrix(sims,sort(unique(sims))),piecol=cols,cex=0.4)
# Add the node labels:
nodelabels(node=1:AN$Nnode+Ntip(AN),
           pie=fitER$lik.anc,piecol=cols,cex=0.5)

# What is the relationship between the q matrix and what you see?
# You could play around with the matrix and alter the underlying model of evolution - use very small deviations. All the power in the universe is in your little R code! Don't let it go to your head.

# Now back to our real land tenure data
# lt_main gives the main type of land tenure for each society; the others are "no ownership", "group", "kin group", "individuals". Some societies have several types of land tenure present.

read.delim("lt.trait", row.names = 1) -> ltdata
ltdata[AN$tip.label,] -> ltdata #order the data frame by the order of tree tiplabels

# Let's make things easy for ourselves and just use lt_main (the first column, so we call [,1]); we need to make sure our rownames are our taxa
lt <- setNames(ltdata[,1], rownames(ltdata))
lt
name.check(AN, lt) # sanity check on matching tree and trait

# What does this data look like on the tree?
# Remember we have four states to the variable so we want to create a new colour palette
cols<-setNames(palette()[1:length(unique(lt))],sort(unique(lt))) 
cols

# Other approaches to colours
library(RColorBrewer)
ltcols <- brewer.pal(4, "Set1")
hist(discoveries, col = ltcols) # visualise our colours using a built-in dataset

plot(AN,type="phylo",show.tip.label=FALSE)
tiplabels(pie=to.matrix(lt,sort(unique(lt))),piecol=cols,cex=0.4)

# Let's have a legend too
legend("bottomright",
       legend=c("no tenure", "group ownership", "kin ownership", "individual ownership"), pch=c(16), col=cols, title="Land Tenure")

# Now let's infer ancestral states

fitER<-ace(lt, AN, type="discrete", model = "ER") 
fitER

# What is the log-likelihood of this model? Note it.

# Add the node labels:
nodelabels(node=1:AN$Nnode+Ntip(AN),
           pie=fitER$lik.anc,piecol=cols,cex=0.5)

# You can also try the "all rates different" model and compare the Lh. However, depending on R's mathematical mood, it might not like these data (too many states to arrive at the maximum likelihood solution)

fitARD<-ace(lt, AN, type="discrete", model = "ARD") 
fitARD

# If you managed to compute the likelihood under this model, compare it to the Equal Rates model. Which one would you prefer?

# More advanced stuff - you can also look at the parameter estimates and think about which changes are estimated at zero. Refer back to the description of the land tenure trait. In words, what would you infer?


########################################
## Appendix: Answers to (some) mini-exercises
########################################

#Plotting a tree containing a random 10 languages
AN.10<-drop.tip(AN,sample(AN$tip.label,87))
plot(AN.10)
