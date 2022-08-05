# 0. preparation
## load the required packages
library(tidyverse)
library(maps) 
## Specify URL where file is stored
url1 <- "https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv
"
# Specify destination where file should be saved
destfile1 <- "./Data/EAsocieties.csv"
# fetch the data set
download.file(url1, destfile1)

## Load the data set
EAsocieties <- read.csv("./Data/EAsocieties.csv")
EHRAFccr <- read.csv("./Data/ehraf-ccr.csv")
toollearning <- read.csv("./Data/non-industrial tool learning.csv")


# 1. Create a csv for the conversion of OWC code and Ethnographic Atlas ID
## Drop duplicates and nones in eHRAF.ID and EA.ID
EHRAFccr_clean <- EHRAFccr %>% dplyr::distinct(eHRAF.ID, .keep_all= TRUE)
EHRAFccr_clean <- EHRAFccr_clean %>% dplyr::distinct(EA.ID, .keep_all= TRUE)
EHRAFccr_clean <- EHRAFccr_clean %>% dplyr::filter(eHRAF.ID != "None")

## Create a clean data frame that contains only these two columns
EHRAFccr_clean <- EHRAFccr_clean %>% dplyr::select(eHRAF.ID, EA.ID)

## Using regular expression to standardize the id and create a new variable (no more id with just three digits like AA1. It should always be AA01) 
EAsocieties$EA.ID = gsub("([a-z])([1-9])$","\\10\\2", EAsocieties$id)

## Merge two data frames to extract coordinates
EHRAFccr_geo <- left_join(EHRAFccr_clean, EAsocieties, by = "EA.ID")

# 2. Clean the toollearning database
## Drop duplicates in OWC codes
toollearning_clean <- toollearning %>% dplyr::distinct(OWC_Code, .keep_all= TRUE)

## Rename the column for matching
toollearning_clean <- toollearning_clean %>% dplyr::rename(eHRAF.ID = OWC_Code)

## Merge two data frames to extract coordinates
toollearning_geo <- left_join(toollearning_clean, EHRAFccr_geo, by = "eHRAF.ID")

## Subset the data frame for mapping
toollearning_geo <- toollearning_geo %>% dplyr::select(eHRAF.ID, EA.ID, Subsistence, Society, Lat, Long)

## Manually filling the gap of latitude and longitude based on the map function of eHRAF
### Mongo
toollearning_geo[2,"Lat"] <- 1
toollearning_geo[2,"Long"] <- 22
### Chagga
toollearning_geo[3,"Lat"] <- -3.25
toollearning_geo[3,"Long"] <- 37.5
### Ovambo
toollearning_geo[4,"Lat"] <- -17.60
toollearning_geo[4,"Long"] <- 15.50
### Tonga
toollearning_geo[6,"Lat"] <- -17.42
toollearning_geo[6,"Long"] <- 27.10
### Ovimbundu
toollearning_geo[7,"Lat"] <- -12.25
toollearning_geo[7,"Long"] <- 16.50
### Akan
toollearning_geo[8,"Lat"] <- 7.00
toollearning_geo[8,"Long"] <- -1.50
### Dogon
toollearning_geo[9,"Lat"] <- 15.06
toollearning_geo[9,"Long"] <- -2.92
### Igbo
toollearning_geo[11,"Lat"] <- 5.50
toollearning_geo[11,"Long"] <- 7.33
### Kpelle
toollearning_geo[12,"Lat"] <- 6.92
toollearning_geo[12,"Long"] <- -9.96
### Okayama
toollearning_geo[16,"Lat"] <- 34.90
toollearning_geo[16,"Long"] <- 133.81
### Bengali
toollearning_geo[18,"Lat"] <- 24.00
toollearning_geo[18,"Long"] <- 90.00
### Ifugao
toollearning_geo[20,"Lat"] <- 16.83
toollearning_geo[20,"Long"] <- 121.17
### Saami
toollearning_geo[24,"Lat"] <- 68.70
toollearning_geo[24,"Long"] <- 21.50
### Nahua
toollearning_geo[25,"Lat"] <- 18.90
toollearning_geo[25,"Long"] <- -96.95
### Zapotec
toollearning_geo[26,"Lat"] <- 17.21
toollearning_geo[26,"Long"] <- -96.22
### Aleut
toollearning_geo[28,"Lat"] <- 55.00
toollearning_geo[28,"Long"] <- -162.85
### Kaska
toollearning_geo[31,"Lat"] <- 60.00
toollearning_geo[31,"Long"] <- -131.00
### Blackfoot
toollearning_geo[38,"Lat"] <- 49.34
toollearning_geo[38,"Long"] <- -111.21
### Hopi
toollearning_geo[39,"Lat"] <- 35.77
toollearning_geo[39,"Long"] <- -110.53
### Navajo
toollearning_geo[40,"Lat"] <- 36.21
toollearning_geo[40,"Long"] <- -110.08
### Trobriands
toollearning_geo[45,"Lat"] <- -8.54
toollearning_geo[45,"Long"] <- 151.01
### Kiribati
toollearning_geo[46,"Lat"] <- 3.38
toollearning_geo[46,"Long"] <- 172.99
### Maori
toollearning_geo[48,"Lat"] <- -35.33
toollearning_geo[48,"Long"] <- 174.17
### Ndyuka
toollearning_geo[49,"Lat"] <- 4.31
toollearning_geo[49,"Long"] <- -54.64
### Shipibo
toollearning_geo[51,"Lat"] <-  -7.17
toollearning_geo[51,"Long"] <- -74.82
### Goajiro
toollearning_geo[54,"Lat"] <-  11.92
toollearning_geo[54,"Long"] <- -71.75

## save the data frame as a csv
write.csv(toollearning_geo,"./Data/toollearning_geo.csv", row.names = FALSE)
toollearning_geo <- read.csv("./Data/toollearning_geo.csv")

# 3. Create a map
## create a color variable
toollearning_geo <-toollearning_geo %>%
  mutate(color = case_when(
    startsWith(Subsistence, "Hu") ~ "#88CCEE",
    startsWith(Subsistence, "Pr") ~ "#CC6677",
    startsWith(Subsistence, "Ho") ~ "#DDCC77",
    startsWith(Subsistence, "Pa") ~ "#117733",
    startsWith(Subsistence, "Ag") ~ "#332288",
    startsWith(Subsistence, "In") ~ "#44AA99",
    startsWith(Subsistence, "Ot") ~ "#999933",
  ))
## Draw a map
pdf(file = "./Figure/Map.pdf", width=15, height=10)
maps::map()
## add points
points(toollearning_geo$Long, toollearning_geo$Lat, col=toollearning_geo$color, pch=16, cex = 2)
## add a legend
legend(-40,-38, legend=c("Hunter-gatherers(n=14)","Primarily hunter-gatherers(n=4)", 
                         "Horticulturalists(n=16)", "Pastoralists(n=2)", 
                         "Agro-pastoralists(n=2)","Intensive agriculturalists(n=11)", 
                         "Other subsistence combinations(n=7)"), 
       col=c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
             "#44AA99", "#999933"), pch=16, ncol=2)
dev.off()

# 4. Make treemaps for visualizing frequency data
# library
library(treemap)
tlearn_grouped <- table(toollearning$Transmission.bias,useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Transmission_bias = Var1)

ggplot(data=tlearn_grouped, aes(x=reorder(Transmission_bias,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Transmission biases", y = "Frequency")
ggplot2::ggsave("./Figure/transbias.png", dpi = 600)

tlearn_grouped1 <- table(toollearning$Transmission.modes,useNA = "always")
tlearn_grouped1 <- as.data.frame(tlearn_grouped1)

ggplot(data=tlearn_grouped1, aes(x=reorder(Var1,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Transmission modes", y = "Frequency")
ggplot2::ggsave("./Figure/transmodes.png", dpi = 600)


tlearn_grouped2 <- table(toollearning$Age.group.of.learner,useNA = "always")
tlearn_grouped2 <- as.data.frame(tlearn_grouped2)

ggplot(data=tlearn_grouped2, aes(x=reorder(Var1,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Age group of learner", y = "Frequency")
ggplot2::ggsave("./Figure/learnerage.png", dpi = 600)

tlearn_grouped3 <- table(toollearning$Age.group.of.model,useNA = "always")
tlearn_grouped3 <- as.data.frame(tlearn_grouped3)

ggplot(data=tlearn_grouped3, aes(x=reorder(Var1,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Age group of model", y = "Frequency")
ggplot2::ggsave("./Figure/modelage.png", dpi = 600)


# treemap
png(filename="./Figure/Tree_transbias.png",width=1000, height=1000)

treemap::treemap(tlearn_grouped,
                 index="Transmission_bias",
                 vSize="Freq",
                 type="index"
)
dev.off()

ggplot2::ggsave("./Figure/Tree_transbias.png", path="figure", dpi = 600)

ggplot(data = toollearning_geo) +
  geom_sf(aes(geometry = Society)) +
  geom_point(data = toollearning_geo, mapping = aes(x = Long, y = Lat), colour = "red") + 
  geom_text(data= toollearning_geo,aes(x=Long, y=Lat, label=Society),
            color = "darkblue", fontface = "bold", check_overlap = FALSE)

library("rnaturalearth")
library("rnaturalearthdata")
library("sf")
library("rgeos")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) + 
  geom_sf() + 
  geom_point(data = toollearning_geo, mapping = aes(x = Long, y = Lat), colour = "red")+
  coord_sf()+
  geom_text(data= toollearning_geo,aes(x=Long, y=Lat, label=Society),
            color = "darkblue", fontface = "bold", check_overlap = TRUE)
