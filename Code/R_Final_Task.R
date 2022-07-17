# package preparation
install.packages("maps")
library(maps)

# draw a map of the world
map() 
# add a point at longitude 0, latitude 52
#  (and two other arguments to make it a filled red circle)
points(0, 52, col = 2)
# add a horizontal line at the equator
abline(h = 0, col = 2)


# Specify URL where file is stored
url <- "https://raw.githubusercontent.com/seannyD/IntroToRForAnthropology/main/data/societies.csv"
# Specify destination where file should be saved
destfile <- "./Data/societies.csv"
# fetch the data set
download.file(url, destfile)

# Load the data set
societies <- read.csv("./Data/societies.csv")

#
You’ll need to:
  
  Download the “societies.csv” file and put it in your “data” folder.
Load the roof, rainfall and society data
Combine the data so that you have the roof shape, rainfall amount and geographic coordinates in one data frame. For the geographic coordinates, you might have to peek at the societies.csv data to figure out which column relates to the latitude and longitude.
Filter out rows where the rainfall is NA.
Create a new column in the dataframe which is either “Flat” or “Not Flat”.
Now we also need to assign a colour to each row, based on whether the rooves are flat or not flat.

Create another new column pointColour in the main data that is e.g. “green” if the roof type is flat and “red” if the roof type is not flat.
Now we can draw the map:
  
  Draw a map
Draw the points. The function points also has a col argument, where you can give it a vector which contains the colour for each point. You can put the pointColour data here.
Add some code to write the map to a file
Here’s my plot, with a legend for reference. Click on the code button to the right to reveal one possible answer.

