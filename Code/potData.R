# Create some fake pot data
#  (you can ignore this bit of code)
potData = data.frame(id=1:100)
pdx = as.data.frame(
  matrix(c(rep(c("red","round"),41),
       rep(c("blue","round"),15),
       rep(c("red","square"),20),
       rep(c("blue","square"),24)),ncol=2,byrow = TRUE))
potData = cbind(potData,pdx)
names(potData) = c("id","colour","shape")
potData = potData[sample(1:nrow(potData)),]
# Take a look at the data
head(potData)

# Make a contingency table
potTable = table(potData$colour,potData$shape)
potTable
# Calculate the difference in distribution
trueDiffScore = abs(potTable[1,1] - potTable[1,2]) + abs(potTable[2,1] - potTable[2,2])
trueDiffScore

# Create a function that takes a data frame of pot data,
#  permutes the colour column using "sample()",
#  creates a contingency table,
#  calculates a difference (like above)
#  and returns that difference
permutePots = function(potData){
  potData$colour=sample(potData$colour)
  potTable = table(potData$colour,potData$shape)
  trueDiffScore = abs(potTable[1,1] - potTable[1,2]) + abs(potTable[2,1] - potTable[2,2])
  return(trueDiffScore)
}

# Use 'replicate' to run this function 10,000 times,
#  and store the results in a variable.
manysamples<- replicate (10000,permutePots(potData))
# Use "hist()" to create a histogram of the results 
hist(manysamples)
# In how many random worlds was the result equal
#  or more extreme than the true difference?
# TODO

sum(manysamples >= 30)
