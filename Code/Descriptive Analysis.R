# 0. preparation
## load the required packages
library(tidyverse)
library(maps)
library(treemapify)
library(patchwork)
library(naniar)

## Specify URL where file is stored
url1 <- "https://raw.githubusercontent.com/D-PLACE/dplace-data/master/datasets/EA/societies.csv"
# Specify destination where file should be saved
destfile1 <- "./Data/EAsocieties.csv"
# fetch the data set
download.file(url1, destfile1)

## Load the data set
EAsocieties <- read.csv("./Data/EAsocieties.csv")
EHRAFccr <- read.csv("./Data/ehraf-ccr.csv")
toollearning <- read.csv("./Data/craft learning.csv")


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
### Mbuti
toollearning_geo[2,"Lat"] <- 1.5
toollearning_geo[2,"Long"] <- 28.33
### Mongo
toollearning_geo[3,"Lat"] <- 1
toollearning_geo[3,"Long"] <- 22
### Chagga
toollearning_geo[7,"Lat"] <- -3.25
toollearning_geo[7,"Long"] <- 37.5
### Berbers of Morocco
toollearning_geo[15,"Lat"] <- 34.92
toollearning_geo[15,"Long"] <- -3.25
### Tuareg
toollearning_geo[17,"Lat"] <- 23.0
toollearning_geo[17,"Long"] <- 6.5
### Bemba
toollearning_geo[18,"Lat"] <- -10.5
toollearning_geo[18,"Long"] <- 30.5
### Khoi
toollearning_geo[19,"Lat"] <- -27.5
toollearning_geo[19,"Long"] <- 17.0
### Ovambo
toollearning_geo[20,"Lat"] <- -17.60
toollearning_geo[20,"Long"] <- 15.50
### Ovimbundu
toollearning_geo[21,"Lat"] <- -12.25
toollearning_geo[21,"Long"] <- 16.50
### Tonga
toollearning_geo[25,"Lat"] <- -17.42
toollearning_geo[25,"Long"] <- 27.10
### Akan
toollearning_geo[27,"Lat"] <- 7.00
toollearning_geo[27,"Long"] <- -1.50
### Dogon
toollearning_geo[28,"Lat"] <- 15.06
toollearning_geo[28,"Long"] <- -2.92
### Igbo
toollearning_geo[31,"Lat"] <- 5.50
toollearning_geo[31,"Long"] <- 7.33
### Kpelle
toollearning_geo[32,"Lat"] <- 6.92
toollearning_geo[32,"Long"] <- -9.96
### Tiv
toollearning_geo[36,"Lat"] <- 7.25
toollearning_geo[36,"Long"] <- 9.0
### Inner Mongolia
toollearning_geo[39,"Lat"] <- 43.3
toollearning_geo[39,"Long"] <- 114.59
### Okayama
toollearning_geo[42,"Lat"] <- 34.90
toollearning_geo[42,"Long"] <- 133.81
### Bengali
toollearning_geo[49,"Lat"] <- 24.00
toollearning_geo[49,"Long"] <- 90.00
### Bhil
toollearning_geo[50,"Lat"] <- 20.85
toollearning_geo[50,"Long"] <- 73.9592
### Garo
toollearning_geo[51,"Lat"] <- 26.0
toollearning_geo[51,"Long"] <- 91.0
### Central Thai
toollearning_geo[57,"Lat"] <- 14.0
toollearning_geo[57,"Long"] <- 100.85
### Ifugao
toollearning_geo[59,"Lat"] <- 16.83
toollearning_geo[59,"Long"] <- 121.17
### Saami
toollearning_geo[65,"Lat"] <- 68.70
toollearning_geo[65,"Long"] <- 21.50
### Island Carib
toollearning_geo[66,"Lat"] <- 15.41
toollearning_geo[66,"Long"] <- -61.27
### Kuna
toollearning_geo[68,"Lat"] <- 9.25
toollearning_geo[68,"Long"] <- -78.5
### Nahua
toollearning_geo[69,"Lat"] <- 18.90
toollearning_geo[69,"Long"] <- -96.95
### Zapotec
toollearning_geo[70,"Lat"] <- 17.21
toollearning_geo[70,"Long"] <- -96.22
### Maya (Yucatán Peninsula)
toollearning_geo[72,"Lat"] <- 18.7757
toollearning_geo[72,"Long"] <- -88.9567
### Lur
toollearning_geo[75,"Lat"] <- 32.5438
toollearning_geo[75,"Long"] <- 47.916
### Aleut
toollearning_geo[76,"Lat"] <- 55.00
toollearning_geo[76,"Long"] <- -162.85
### Kaska
toollearning_geo[82,"Lat"] <- 60.00
toollearning_geo[82,"Long"] <- -131.00
### Western Woods Cree
toollearning_geo[84,"Lat"] <- 57.0
toollearning_geo[84,"Long"] <- -99.0
### Mi'kmaq
toollearning_geo[89,"Lat"] <- 46.99
toollearning_geo[89,"Long"] <- -65.0
### Blackfoot
toollearning_geo[104,"Lat"] <- 49.34
toollearning_geo[104,"Long"] <- -111.21
### Hopi
toollearning_geo[114,"Lat"] <- 35.77
toollearning_geo[114,"Long"] <- -110.53
### Navajo
toollearning_geo[117,"Lat"] <- 36.21
toollearning_geo[117,"Long"] <- -110.08
### Trobriands
toollearning_geo[131,"Lat"] <- -8.54
toollearning_geo[131,"Long"] <- 151.01
### Belau
toollearning_geo[133,"Lat"] <- 7.5
toollearning_geo[133,"Long"] <- 134.5
### Kiribati
toollearning_geo[135,"Lat"] <- 3.38
toollearning_geo[135,"Long"] <- 172.99
### Maori
toollearning_geo[141,"Lat"] <- -35.33
toollearning_geo[141,"Long"] <- 174.17
### Tikopia
toollearning_geo[144,"Lat"] <- -12.29
toollearning_geo[144,"Long"] <- 168.83
### Jivaro
toollearning_geo[147,"Lat"] <- -3.0
toollearning_geo[147,"Long"] <- -78.0
### Ndyuka
toollearning_geo[148,"Lat"] <- 4.31
toollearning_geo[148,"Long"] <- -54.64
### Shipibo
toollearning_geo[151,"Lat"] <-  -7.17
toollearning_geo[151,"Long"] <- -74.82
### Sirionó
toollearning_geo[152,"Lat"] <-  -14.5
toollearning_geo[152,"Long"] <- -63.5
### Otavalo Quichua
toollearning_geo[156,"Lat"] <-  0.31776
toollearning_geo[156,"Long"] <- -78.3729
### Saraguro Quichua
toollearning_geo[157,"Lat"] <-  -3.69352
toollearning_geo[157,"Long"] <- -79.29151
### Goajiro
toollearning_geo[164,"Lat"] <-  11.92
toollearning_geo[164,"Long"] <- -71.75
### Kogi
toollearning_geo[165,"Lat"] <-  10.9485
toollearning_geo[165,"Long"] <- -73.8043

## save the data frame as a csv
write.csv(toollearning_geo,"./Data/toollearning_geo.csv", row.names = FALSE)
toollearning_geo <- read.csv("./Data/toollearning_geo.csv")

## counting subsistence type for mapping
toollearning_geo %>% count(Subsistence)

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
points(toollearning_geo$Long, toollearning_geo$Lat, col=alpha(toollearning_geo$color, 0.75), pch=16, cex = 1.5)
## add a legend with color-blind friendly palette
legend(-40,-38, legend=c("Hunter-gatherers(n=45)","Primarily hunter-gatherers(n=14)", 
                         "Horticulturalists(n=36)", "Pastoralists(n=5)", 
                         "Agro-pastoralists(n=13)","Intensive agriculturalists(n=28)", 
                         "Other subsistence combinations(n=29)"), 
       col=c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
             "#44AA99", "#999933"), pch=16, ncol=2)
dev.off()





############## descriptive analysis and plotting ######################
## create a subset for clay technologies
toollearning_clay <- toollearning %>%filter(Raw.material=="Clay")

## what question
tlearn_grouped <- table(toollearning$Technology_cat)
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Technology_cat = Var1) %>% dplyr::arrange(Freq)
what1<-ggplot(data=tlearn_grouped, aes(x=reorder(Technology_cat,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Toolmaking skills", y = "Frequency")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )

tlearn_grouped <- table(toollearning$Raw.material, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Raw.material = Var1) %>% dplyr::arrange(Freq)
what2<-ggplot(data=tlearn_grouped, aes(x=reorder(Raw.material,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Types of raw material", y = "Frequency")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )+scale_y_continuous(limits = c(0,200))
patchwork <- (what1 + what2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("./Figure/what.jpg", width = 44, height = 20, units = "cm", dpi = 300)

## where question
tlearn_grouped <- table(toollearning$Location.type., useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Location.type. = Var1) %>% dplyr::arrange(Freq)
where1<-ggplot(data=tlearn_grouped, aes(x=reorder(Location.type.,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Types of location", y = "Frequency")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )
## treemap making
tmap<-toollearning %>%  select(Location.type., Technology_cat) %>%  drop_na()
tmap1<- tmap %>% group_by(Location.type.,Technology_cat) %>% summarise(N=n())
where2<-ggplot(tmap1, aes(area = N, fill = Location.type.,
               label = paste0(Technology_cat,'\n',N), subgroup = Location.type.)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "black",
                             fontface = "italic") +
  geom_treemap_text(colour = "black",
                    min.size = 3, grow = TRUE)+
  scale_fill_discrete(name = "Location")
patchwork <- (where1 + where2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("./Figure/where.jpg", width = 44, height = 20, units = "cm", dpi = 300)

## why question (transmission bias)
tlearn_grouped <- table(toollearning$Transmission.bias_cat, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Transmission.bias_cat = Var1) %>% dplyr::arrange(Freq)
why1<-ggplot(data=tlearn_grouped, aes(x=reorder(Transmission.bias_cat,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Transmission bias", y = "Frequency(Overall)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )+scale_y_continuous(limits = c(0,300))
### Clay
tlearn_grouped <- table(toollearning_clay$Transmission.bias_cat, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Transmission.bias_cat = Var1) %>% dplyr::arrange(Freq)
why2<-ggplot(data=tlearn_grouped, aes(x=reorder(Transmission.bias_cat,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Transmission bias", y = "Frequency(Clay)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )+scale_y_continuous(limits = c(0,30))
patchwork <- (why1 + why2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("./Figure/why.jpg", width = 44, height = 20, units = "cm", dpi = 300)

## how question (learning process)
tlearn_grouped <- table(toollearning$Learning.process_cat, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Learning.process_cat = Var1) %>% dplyr::arrange(Freq)
how1<-ggplot(data=tlearn_grouped, aes(x=reorder(Learning.process_cat,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Learning process", y = "Frequency(Overall)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )
### Clay
tlearn_grouped <- table(toollearning_clay$Learning.process_cat, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Learning.process_cat = Var1) %>% dplyr::arrange(Freq)
how2<-ggplot(data=tlearn_grouped, aes(x=reorder(Learning.process_cat,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Learning process", y = "Frequency(Clay)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )
patchwork <- (how1 + how2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("./Figure/how.jpg", width = 44, height = 20, units = "cm", dpi = 300)

## when question (age group)
tlearn_grouped <- table(toollearning$Age.group.of.learner, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Age.group.of.learner = Var1) %>% dplyr::arrange(Freq)
when1<-ggplot(data=tlearn_grouped, aes(x=reorder(Age.group.of.learner,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Learners' age group", y = "Frequency(Overall)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )+scale_y_continuous(limits = c(0,300))
### clay
tlearn_grouped <- table(toollearning_clay$Age.group.of.learner, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Age.group.of.learner = Var1) %>% dplyr::arrange(Freq)
when2<-ggplot(data=tlearn_grouped, aes(x=reorder(Age.group.of.learner,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Learners' age group", y = "Frequency(Clay)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )+scale_y_continuous(limits = c(0,30))
patchwork <- (when1 + when2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("./Figure/when.jpg", width = 44, height = 20, units = "cm", dpi = 300)

## who question (transmission mode)
tlearn_grouped <- table(toollearning$Transmission.modes_cat, useNA = "always")
tlearn_grouped <- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Transmission.modes_cat = Var1) %>% dplyr::arrange(Freq)
who1<-ggplot(data=tlearn_grouped, aes(x=reorder(Transmission.modes_cat,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Transmission mode", y = "Frequency(Overall)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )+scale_y_continuous(limits = c(0,170))
###clay
tlearn_grouped <- table(toollearning_clay$Transmission.modes_cat, useNA = "always")
tlearn_grouped<- as.data.frame(tlearn_grouped)
tlearn_grouped <- tlearn_grouped %>% dplyr::rename(Transmission.modes_cat = Var1) %>% dplyr::arrange(Freq)
who2<-ggplot(data=tlearn_grouped, aes(x=reorder(Transmission.modes_cat,-Freq), y=Freq)) +
  geom_bar(stat="identity")+ coord_flip()+ 
  labs(x="Transmission mode", y = "Frequency(Clay)")+theme(axis.text=element_text(size=18), axis.title.x = element_text(size=20),axis.title.y = element_text(size=20))+
  geom_text(
    label=tlearn_grouped$Freq,
    hjust = -0.1, colour = "red",
  )+scale_y_continuous(limits = c(0,17))
patchwork <- (who1 + who2)
patchwork + plot_annotation(tag_levels = 'A')
ggplot2::ggsave("./Figure/who.jpg", width = 44, height = 20, units = "cm", dpi = 300)

##########################
## missing pattern analysis


toollearning <- toollearning %>% select(!c(Area, OWC_Code, Note, Reference, Location, Age.of.learner, Age.of.model, Technology))

# Visualize missing data patterns
miss1<-naniar::vis_miss(toollearning)+theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=1))
ggplot2::ggsave("./Figure/missing.jpg", width = 22, height = 20, units = "cm", dpi = 300)


miss2<-gg_miss_upset(toollearning, nsets = 10)
jpeg("Figure/upset.jpg", units="in", width=6, height=5, res=300)
gg_miss_upset(toollearning, nsets = 10)
dev.off()

explanatory = c("Location.type.", "Age.group.of.learner", 
                "nodes", "obstruct.factor")
dependent = "Subsistence"
colon_s %>% 
  missing_compare(dependent, explanatory) %>% 
  knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r"))

# Perform Little's MCAR test
naniar::mcar_test(toollearning)
missCompare::miss_var_correlation(toollearning)

