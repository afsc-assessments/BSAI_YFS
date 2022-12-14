#oto=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/YFS_oto_fishery.csv",header=TRUE)
oto=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2022/YFS_oto_fishery_2022.csv",header=TRUE)


library(tidyverse)
library(maps)
library(maptools)
library(ggrepel)
library(viridis)
library(ggplot2)
#library(pals)
library(grid)


world <- map_data("world2")

word_data <- data.frame(world)
head(word_data)
regions <- unique(word_data$region)

USA <- map_data("world2") %>% 
 filter(region=="USA")

Canada <- map_data("world2") %>% 
 filter(region=="Canada")

Russia <- map_data("world2")%>% 
 filter(region=="Russia")
# 
Japan <- map_data("world2")%>% 
 filter(region=="Japan")
# 
China<- map_data("world2")%>% 
 filter(region=="China")

Korea <- map_data('world2',region=c('South Korea', 'North Korea'))

neAsia <- map_data(map = 'world2',
                   region = c('South Korea', 'North Korea', 'China',
                              'Japan', 'Mongolia', 'Taiwan'))

# Read in your data frame with longitude, latitude, and other

#full_set_long <- full_set$Longitude
#full_set$Long_360 <- make360(full_set_long)
#head(full_set)
#PLOT 1

#all locations in blue, with labels of their location, no geo boundary differences
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 theme(panel.background = element_rect(fill = "white"), 
       panel.grid.major = element_line(colour = NA), 
       axis.text=element_text(size=10),
       axis.title =element_text(size=12),
       legend.title=element_text(size=12),
       legend.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=1))+
 coord_map(xlim= c(185, 205),  ylim = c(54, 61))+
 labs(x = "Longitude", y = "Latitude")+geom_point(data=oto,aes(x= LonDD.End, y=LatDD.End), alpha=0.5,size=0.5,colour="red")+facet_wrap(~Month)+ggtitle("Yellowfin sole otolith collections by month, 2021")

#I ordered the date
otoo=oto[which(oto$Specimen.Type.Description=="Otoliths"),]
#try for 700 to be aged
#do a systematic random sample to avoid clustering in space and time
write.csv(sample(otoo$Specimen.Number,700,replace=FALSE),"/Users/ingridspies/Downloads/YFS_fishery_otoliths.csv")


#now look over lots of years to compare spawn timing

oto=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2022/YFS_oto_fishery_ALLYRS.csv",header=TRUE)
#select your year here.
yr=2019 #1999 was cold 2019 hot
oto2=oto[which(oto$Year==yr),]

#all locations in blue, with labels of their location, no geo boundary differences
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 theme(panel.background = element_rect(fill = "white"), 
       panel.grid.major = element_line(colour = NA), 
       axis.text=element_text(size=10),
       axis.title =element_text(size=12),
       legend.title=element_text(size=12),
       legend.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=1))+
 coord_map(xlim= c(185, 205),  ylim = c(54, 61))+
 labs(x = "Longitude", y = "Latitude")+geom_point(data=oto2,aes(x= LonDD.End, y=LatDD.End), alpha=0.5,size=0.5,colour="red")+facet_wrap(~Month)+ggtitle(paste("Yellowfin sole otolith collections by month,",yr))

