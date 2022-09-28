
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
  coord_map(xlim= c(175, 220),  ylim = c(49, 63))+
  labs(x = "Longitude", y = "Latitude")

catyfs=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2020/YFS_fisheryloc_1954_2020.csv",header=TRUE)

catyfs=read.csv("/Users/ingridspies/Documents/WorkDellStuff/Assessments/YFS/2021/YFS_fisheryloc_1991_2021.csv",header=TRUE)

geom_point(data=catchYFS19,aes(x= LONDD_END, y=LATDD_END), alpha=0.5,size=0.5,colour="red")+facet_wrap(~Month)+ylab("Latitude")+xlab("Longitude")+theme_bw(base_size=11)

#Just plot presence absence by gear type. Here you see one odd record of yFS in the NBS by a pot  in 2020.
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 geom_point(data=catyfs[which(catyfs$Gear==1&catyfs$Year>2007),],aes(x=Longitude2,y=Latitude),alpha=0.5,colour="red",cex=0.1)+facet_wrap(~Year)+
 coord_map(xlim= c(175, 220),  ylim = c(49, 68))+
 labs(x = "Longitude", y = "Latitude")+theme(text = element_text(size = 12))+ggtitle("YFS catch, non-pelagic trawl")
8=longline
6=pot
2=pelagic trawl
1=non pelagic trawl

#This script sets up a data frame of 2 degree cells with amount of catch in each year for ggplot2
yrs=names(table(catyfs$Year))
lat=seq(52.3,65.6,2)
long=seq(181.1,200.8,2)
first=matrix(0,2,4)
colnames(first)=c("Latitude","Longitude","Weight","Year")

for (k in 1: length(yrs)){
 loc_mat=matrix(0,length(lat)+1,length(long)+1)
 rownames(loc_mat)=c(lat,max(lat)+2)
 colnames(loc_mat)=c(long,max(long)+2)
for (i in 1:length(lat)){
 for(j in 1:length(long)){
  loc_mat[i,j]=sum(catyfs$Extrapolated_weight[which(catyfs$Latitude>lat[i]&catyfs$Latitude<=lat[i+1]&catyfs$Longitude2>long[j]&catyfs$Longitude2<=long[j+1]&catyfs$Year==yrs[k])])#&catyfs$Gear==1
  }
}
 d <- (as.data.frame(loc_mat))
 d$Latitude=row.names(d)
 loc_mat2=pivot_longer(d,-Latitude)
 colnames(loc_mat2)=c("Latitude","Longitude","Weight")
 loc_mat2%>%add_column(Year=as.double(rep(yrs[k],nrow(loc_mat2))))
 locmatdf=as.data.frame(loc_mat2)
 locmatdf$Latitude=as.numeric(locmatdf$Latitude)
 locmatdf$Longitude=as.numeric(locmatdf$Longitude)
 locmatdf$Weight=as.numeric(locmatdf$Weight)
 locmatdf1=cbind(locmatdf,Year=as.numeric(rep(yrs[k],nrow(loc_mat2))))
 
 first=rbind(locmatdf1,first)
 
}
write.csv(first,"/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/Catch1991_2021_allgear_2deg.csv")

Catch1991_2021_allgear_2deg=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/Catch1991_2021_allgear_2deg.csv",header=TRUE)
Catch1991_2021a_allgear_2deg=Catch1991_2021_allgear_2deg[which(Catch1991_2021_allgear_2deg$Weight>0),]
Catch1991_2021a_allgear_2deg$Weight=Catch1991_2021a_allgear_2deg$Weight/1000

Catch1991_2021_trawl_2deg=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/Catch1991_2021_trawl_2deg.csv",header=TRUE)
Catch1991_2021a_trawl_2deg=Catch1991_2021_trawl_2deg[which(Catch1991_2021_trawl_2deg$Weight>0),]
Catch1991_2021a_trawl_2deg$Weight=Catch1991_2021a_trawl_2deg$Weight/1000

Catch1991_2021_pot_2deg=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/Catch1991_2021_pot_2deg.csv",header=TRUE)
Catch1991_2021a_pot_2deg=Catch1991_2021_pot_2deg[which(Catch1991_2021_pot_2deg$Weight>0),]
Catch1991_2021a_pot_2deg$Weight=Catch1991_2021a_pot_2deg$Weight/1000

Catch1991_2021_longline_2deg=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/Catch1991_2021_longline_2deg.csv",header=TRUE)
Catch1991_2021a_longline_2deg=Catch1991_2021_longline_2deg[which(Catch1991_2021_longline_2deg$Weight>0),]
Catch1991_2021a_longline_2deg$Weight=Catch1991_2021a_longline_2deg$Weight/1000

#All gear types, 2 degree bins.
#YFS_20yrs_allgear_2deg.pdf
par(mar=c(0,0,0,0))
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 geom_point(data=Catch1991_2021a_allgear_2deg[which(Catch1991_2021a_allgear_2deg$Year>2001),],aes(x=Longitude+1,y=Latitude+1,color=Weight),cex=1,pch=19)+facet_wrap(~Year)+
 theme(panel.background = element_rect(fill = "gray96"), 
panel.grid.major = element_line(colour = NA), 
axis.text=element_text(size=8),
axis.title =element_text(size=12),
legend.title=element_text(size=12),title=element_text(size=12),
legend.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=1))+
 coord_map(xlim= c(175, 220),  ylim = c(49, 68))+
 labs(x = "Longitude", y = "Latitude")+scale_color_gradient(low="orange", high="red",name="Catch (mt)")+ggtitle("Yellowfin Sole catch, all gear types, 2 degree bins")


#Trawl only, bottom, 2 degree bins.
#YFS_20yrs_trawl12_2deg.pdf
par(mar=c(0,0,0,0))
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 geom_point(data=Catch1991_2021a_trawl_2deg[which(Catch1991_2021a_trawl_2deg$Year>2001),],aes(x=Longitude+1,y=Latitude+1,color=Weight),cex=1,pch=19)+facet_wrap(~Year)+
 theme(panel.background = element_rect(fill = "gray96"), 
       panel.grid.major = element_line(colour = NA), 
       axis.text=element_text(size=8),
       axis.title =element_text(size=12),
       legend.title=element_text(size=12),title=element_text(size=12),
       legend.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=1))+
 coord_map(xlim= c(175, 220),  ylim = c(49, 68))+
 labs(x = "Longitude", y = "Latitude")+scale_color_gradient(low="orange", high="red",name="Catch (mt)")+ggtitle("Yellowfin Sole catch, bottom trawl gear, 2 degree bins")

#Pot only, 2 degree bins.
#YFS_20yrs_pot_2deg.pdf
par(mar=c(0,0,0,0))
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 geom_point(data=Catch1991_2021a_pot_2deg[which(Catch1991_2021a_pot_2deg$Year>2001),],aes(x=Longitude+1,y=Latitude+1,color=Weight),cex=1,pch=19)+facet_wrap(~Year)+
 theme(panel.background = element_rect(fill = "gray96"), 
       panel.grid.major = element_line(colour = NA), 
       axis.text=element_text(size=8),
       axis.title =element_text(size=12),
       legend.title=element_text(size=12),title=element_text(size=12),
       legend.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=1))+
 coord_map(xlim= c(175, 220),  ylim = c(49, 68))+
 labs(x = "Longitude", y = "Latitude")+scale_color_gradient(low="orange", high="red",name="Catch (mt)")+ggtitle("Yellowfin Sole catch, pot gear only, 2 degree bins")

#Longline only, types 1 and 2, 2 degree bins.
#YFS_20yrs_longline_2deg.pdf
par(mar=c(0,0,0,0))
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 geom_point(data=Catch1991_2021a_longline_2deg[which(Catch1991_2021a_longline_2deg$Year>2001),],aes(x=Longitude+1,y=Latitude+1,color=Weight),cex=1,pch=19)+facet_wrap(~Year)+
 theme(panel.background = element_rect(fill = "gray96"), 
       panel.grid.major = element_line(colour = NA), 
       axis.text=element_text(size=8),
       axis.title =element_text(size=12),
       legend.title=element_text(size=12),title=element_text(size=12),
       legend.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=1))+
 coord_map(xlim= c(175, 220),  ylim = c(49, 68))+
 labs(x = "Longitude", y = "Latitude")+scale_color_gradient(low="orange", high="red",name="Catch (mt)")+ggtitle("Yellowfin Sole catch, longline gear only, 2 degree bins")


###Now create map of just 2020 catch by month
#This script sets up a data frame of 1 degree cells with amount of catch in each month for ggplot2
mth=names(table(catyfs$Month))
lat=seq(52.3,62,1)
long=seq(181.1,205,1)
first=matrix(0,2,4)
colnames(first)=c("Latitude","Longitude","Weight","Month")

for (k in 1: length(mth)){
 loc_mat=matrix(0,length(lat)+1,length(long)+1)
 rownames(loc_mat)=c(lat,max(lat)+1)
 colnames(loc_mat)=c(long,max(long)+1)
 for (i in 1:length(lat)){
  for(j in 1:length(long)){
   loc_mat[i,j]=sum(catyfs$Extrapolated_weight[which(catyfs$Latitude>lat[i]&catyfs$Latitude<=lat[i+1]&catyfs$Longitude2>long[j]&catyfs$Longitude2<=long[j+1]&catyfs$Month==mth[k]&catyfs$Gear<3&catyfs$Year==2021)])
  }
 }
 d <- (as.data.frame(loc_mat))
 d$Latitude=row.names(d)
 loc_mat2=pivot_longer(d,-Latitude)
 colnames(loc_mat2)=c("Latitude","Longitude","Weight")
 loc_mat2%>%add_column(Year=as.double(rep(yrs[k],nrow(loc_mat2))))
 locmatdf=as.data.frame(loc_mat2)
 locmatdf$Latitude=as.numeric(locmatdf$Latitude)
 locmatdf$Longitude=as.numeric(locmatdf$Longitude)
 locmatdf$Weight=as.numeric(locmatdf$Weight)
 locmatdf1=cbind(locmatdf,Month=as.numeric(rep(mth[k],nrow(loc_mat2))))
 
 first=rbind(locmatdf1,first)
 
}
write.csv(first,"/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/Catch2021_Trawl_Month_1deg.csv")
Catch2021_Trawl_Month_1deg=read.csv("/Users/ingridspies/admbmodels/flatfish/assessments/yfs/doc/2021/Catch2021_Trawl_Month_1deg.csv",header=TRUE)

Catch2021_Trawl_Month_1deg=Catch2021_Trawl_Month_1deg[which(Catch2021_Trawl_Month_1deg$Weight>0),]
Catch2021_Trawl_Month_1deg$Weight=Catch2021_Trawl_Month_1deg$Weight/1000

#TO get the months in the correct order, set up months as factors and order them.
months=c("January","February","March","April","May","June","July","August","September","October")
Catch2021_Trawl_Month_1deg$Month2=months[Catch2021_Trawl_Month_1deg$Month]
Catch2021_Trawl_Month_1deg$Month2 <- factor(Catch2021_Trawl_Month_1deg$Month2,levels=c("January","February","March","April","May","June","July","August","September","October"))

#plot 2021 by month
ggplot() +
 geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey30", alpha=0.3) +
 geom_polygon(data = Canada, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3) +
 geom_polygon(data = Russia, aes(x=long, y = lat, group = group), fill=" grey30", alpha=0.3)+
 geom_point(data=Catch2021_Trawl_Month_1deg,aes(x=Longitude+0.5,y=Latitude+0.5,color=Weight),cex=1,pch=19)+facet_wrap(~Month2)+
 theme(panel.background = element_rect(fill = "gray96"), 
       panel.grid.major = element_line(colour = NA), 
       axis.text=element_text(size=8),
       axis.title =element_text(size=12),
       legend.title=element_text(size=12),title=element_text(size=12),
       legend.text=element_text(size=12),axis.text.x=element_text(angle=45,hjust=1))+
 coord_map(xlim= c(175, 210),  ylim = c(50, 62))+
 labs(x = "Longitude", y = "Latitude")+scale_color_gradient(low="orange", high="red",name="Catch (mt)")+ggtitle("Yellowfin Sole catch by trawl, 1 degree bins")


