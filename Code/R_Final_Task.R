# package preparation
library(maps)
library(tidyverse)
# draw a map of the world
map() 
# add a point at longitude 0, latitude 52
#  (and two other arguments to make it a filled red circle)
points(0, 52, col = 2)
# add a horizontal line at the equator
abline(h = 0, col = 2)


# Specify URL where file is stored
url1 <- "https://raw.githubusercontent.com/seannyD/IntroToRForAnthropology/main/data/societies.csv"
url2 <- "https://raw.githubusercontent.com/seannyD/IntroToRForAnthropology/main/data/RoofShapeData.csv"
url3 <- "https://raw.githubusercontent.com/seannyD/IntroToRForAnthropology/main/data/RainfallData_onlyComplete.csv"
# Specify destination where file should be saved
destfile1 <- "./Data/societies.csv"
destfile2 <- "./Data/roofshape.csv"
destfile3 <- "./Data/rainfall.csv"
# fetch the data set
download.file(url1, destfile1)
download.file(url2, destfile2)
download.file(url3, destfile3)

# Load the data set
societies <- read.csv("./Data/societies.csv")
roofshape <- read.csv("./Data/roofshape.csv")
rainfall <- read.csv("./Data/rainfall.csv")

# data combination
#societies_new <- societies %>% select(xd_id, Lat, Long)
societies <- dplyr::rename(societies, society_xd_id = xd_id)
societies_new <- left_join(societies, roofshape, by="society_xd_id")
societies_new <- left_join(societies_new, rainfall, by="society_xd_id")

# Filter out rows where the rainfall is NA.
societies_complete <- societies_new  %>%
  filter(!is.na(code.y))

# Create a new column in the dataframe which is either “Flat” or “Not Flat”.
societies_complete$Flatroof <- ifelse(endsWith(societies_complete$code_label.x, "t"), "Flat", "Not flat")
societies_complete1 <- societies_complete  %>%
  filter(!is.na(Flatroof))
# Create another new column pointColour in the main data that is e.g. “green” if the roof type is flat and “red” if the roof type is not flat.

societies_complete1$color <- ifelse(startsWith(societies_complete1$code_label.x, "F"), "green", "red")

# Draw a map
pdf(file = "./Figure/My Plot.pdf", width=20, height=20)
# draw a map
maps::map()
# add points
points(societies_complete1$Long, societies_complete1$Lat, col= societies_complete1$color)
# add a legend
legend(-45,-40, legend=c("Flat","Not Flat"), 
       col=c("green","red"), pch=16, ncol=2)
dev.off()
