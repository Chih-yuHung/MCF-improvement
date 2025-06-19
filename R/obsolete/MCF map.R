library(ggmap)
library(ggplot2)
library(mapproj)
MCF.loc<-read.csv("D:/Dropbox/AAFC/Project 1_MCF/3_results/MCF with locations.csv", header = TRUE)

# map("worldHires","Canada",xlim=c(-141,-53), ylim=c(40,85), col="gray90", fill=TRUE)

register_google(key = "AIzaSyCL9A3YMNhi1AdSwXoZidmJg84Jj6uESc4")

#camap <- get_googlemap(center = c(lon=-97.00,lat=62.50), 
#                       zoom = 3)
     
#camap<-get_map(c(left = -132, bottom = 40, right = -53, top = 60)
#               , maptype ="toner-lite")
#ggmap(camap)+
#  geom_point(data=MCF.loc, 
#             aes(x=Lon, y=Lat,
#                 color=MCF),size=1)+ 
#  scale_color_continuous(
#    low = "yellow",high = "red")+ 
#  guides(size=FALSE)


#To obtain city name by lat and lon.
Loc<-c(1:3403)
for (i in 1:3404){
Loc[i]<- revgeocode(c(MCF.loc[i,2],MCF.loc[i,3]),output="address")
}
Loc<-as.data.frame(Loc)
write.csv(Loc,"D:/Dropbox/AAFC/Project 1_MCF/3_results/Address of weather stations.csv")
