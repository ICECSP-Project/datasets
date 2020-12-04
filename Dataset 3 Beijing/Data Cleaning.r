library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyverse)
library(ggmap)
library(lubridate)
options(repr.plot.width=6, repr.plot.height=4)

data <- read_csv("./Git Repo/ICECSP-Project/datasets/Dataset 3 Beijing/beijing.csv", locale = locale(encoding = "UTF-8")) %>% mutate(floor = str_trim(str_extract(floor,"( .*)"), side = "both"))
dim(data)
str(data)


data <-  select(data,-id,-url)
#should we remove cid

sapply(data, function(x){length(unique(factor(x)))})

str(data)

names(data)
cols <- c("Cid", "livingRoom", "kitchen", "bathRoom","floor","buildingType","renovationCondition","buildingStructure",
          "elevator","fiveYearsProperty","subway","district","province","drawingRoom")
#should we convert "constructionTime" to factor


data[cols] <- lapply(data[cols], factor) 
data$constructionTime <- as.numeric(data$constructionTime)

str(data)
summary(data)

missing_vals <- sapply(data, function(x){sum(is.na(x))})
per_missing <- data.frame(missing_vals,percent_missing = paste0(round((missing_vals/nrow(data))*100,2),'%'))%>% arrange(desc(missing_vals))
per_missing

# plot of missing data
x1 <- map_df(data, function(x){sum(is.na(x))})
missing <- x1 %>% gather(key = "Variable") %>% filter(value > 0) %>% mutate(value = value/nrow(data))
ggplot(missing, aes(x = reorder(Variable, -value),y = value)) + 
  geom_bar(stat = "identity", fill = "salmon")+
  coord_flip()

# remove missing vales from all column except DOM
completeFun <- function(df, desiredCols) {
  completeVec <- complete.cases(df[, desiredCols])
  return(df[completeVec, ])
}

data <- completeFun(data, c("bathRoom","subway","livingRoom","floor","fiveYearsProperty",
                            "elevator","drawingRoom","communityAverage","constructionTime"))

#Missing Data for DOM

des <- psych::describe(data$DOM)
des
qqnorm(data$DOM)
qqline(data$DOM)

#########################################

# The Na DOM could mean that the house may be sold in 0 days,  since the beijing 
#very fast-paced with 50% of the offers staying under 6 days on the portal! not sure to what to do with it

data2 <-data[,c("tradeTime","price","DOM")]
data2$Missing <- is.na(data2$DOM)
data2 <- data2 %>%
  group_by( year = year(tradeTime),Missing) %>%
  summarise(price = mean(price))



ggplot(data = data2, aes(x = year, y = price))+
  geom_line(aes(color = Missing), size = 1)

#This proves the missing values in DOM are missing and not houses sold in 0 days
#library(mice)
#init = mice(data, maxit=0) 
#meth = init$method
#predM = init$predictorMatrix

#meth[c("DOM")]="norm" 

#set.seed(103)
#imputed = mice(data, method=meth, predictorMatrix=predM)

data$DOM<- ifelse(is.na(data$DOM),median(data$DOM,na.rm=TRUE),data$DOM)
missing_vals <- sapply(data, function(x){sum(is.na(x))})
missing_vals
#########################################

table(data$buildingType)
# 1908 is labled as "NaN"
(sum(data$buildingType == "NaN")/nrow(data))*100
data <- data[!data$buildingType == "NaN",]
data$buildingType <- droplevels(data$buildingType)
data$buildingType <- factor(data$buildingType,labels = c("Tower","Bungalow","Plate/Tower","Plate"))

table(data$renovationCondition)
data$renovationCondition <- droplevels(data$renovationCondition)
data$renovationCondition <- factor(data$renovationCondition,labels = c("Other","Rough","Simplicit","Hardcover"))

table(data$buildingStructure)
data$buildingStructure <- droplevels(data$buildingStructure)
data$buildingStructure <- factor(data$buildingStructure,labels = c("Unavailable","Mixed","Brick/Wood","Brick/Concrete",
                                                                   "Steel","Steel/Concrete"))

data$elevator <- factor(data$elevator,labels = c("No_elevator","Has_Elevator")) 

data$subway <- factor(data$subway,labels = c("No_Subway","Has_Subway"))

data$fiveYearsProperty <- factor(data$fiveYearsProperty,labels = c("Ownership > 5y","Ownership < 5y"))

table(data$district)
data$district <- factor(data$district,labels = c("DongCheng","FengTai","DaXing","FaXing","FangShang","ChangPing",
                                                 "ChaoYang","HaiDian","ShiJingShan","XiCheng","TongZhou","ShunYi","MenTouGou"))
#Working with lat and log

locations_df0 <- data.frame(lon = data$Lng, lat = data$Lat)
locations_df <- locations_df0 %>% group_by(lon,lat) %>% mutate(count = n()) %>% arrange(count)%>%select(lon,lat,count)
locations_df <- as_tibble(distinct(locations_df))
locations_sf <- sf::st_as_sf(locations_df, coords = c("lon", "lat"), crs = 4326)
mapview::mapview(locations_sf,cex="count",label="count")

# we can consider the lat and log as factor variable in that case we can either remove locations
# whose count is low or we can draw a circle from mean in the map which remove of data not in the circle - remove farthest points farthest.

ggplot(data,aes(y=price,x=district))+geom_boxplot(     # custom boxes
  color="blue",
  fill="blue",
  alpha=0.2,
  
  # Notch?
  notch=TRUE,
  notchwidth = 0.8,
  
  # custom outliers
  outlier.colour="black",
  outlier.fill="black",
  outlier.size=1)

