setwd("R")


rm(list=ls())

library(sp)
library(spatstat)
library(rgeos)
library(maptools)

#source("Network_Tools_package.r")
source("undirected_factory.R")
source("directed_factory.R")

##To open a shapefile polyline do
pv.map<-readShapeLines("../Data/Lleida/Lleida_connected_UTM.shp")

#class(pv.map) ##to know the class of file

#to open additional information
#pv.map@data #metadata, each row represent each entry in pv.map@lines
#pv.map@lines #open the line information

#plot(pv.map,axes=TRUE)
##Now pv.map is a SpatialLinesDataframe object

##To obtain a linnet object
pv.map.ln<- as.linnet.SpatialLines(pv.map, fuse=TRUE)
pv_map_acc<-st_read("../Data/Lleida/Lleida_accidents_UTM.shp")


##Generate linnet object and check if it is connected
pv_map_ln<- as.linnet.SpatialLines(pv.map, fuse=TRUE)
LNnew<-pv_map_ln
spatstat::is.connected(pv_map_ln)


#plot(pv.map.ln,axes=TRUE)
##to obtain information from pv.map.ln
#ls(pv.map.ln)
#ls(pv.map.ln$window)
#ls(pv.map.ln$window$xrange)
#pv.map.ln$window$xrange[1]


##Call point pattern###

lleida_shp <- "../Data/Lleida/Lleida_connected_UTM.shp"
lleida_accidents <- "../Data/Lleida/Lleida_accidents_UTM.shp"


undirected <- UndirectedFactory()
undirected_plannar <- undirected$getExtendedGraph(map_obj = lleida_shp, 
                                                  events_file_path = lleida_accidents,
                                                  graph_characteristic = "plannar")

    #                                            graph_characteristic = "plannar")
# Get all the crossings of the network
crossings <- undirected_plannar$getCrossings()
crossings


g <- undirected_plannar$getGraph()
points(vertex_attr(g)$long[crossings],vertex_attr(g)$lat[crossings],type="p",pch=19,col="green",cex=0.60)

##Generate point pattern
cord<-st_coordinates(pv_map_acc)

m<-length(pv_map_acc$field_1)
x<-c();y<-c()
pv_map_acc$geometry[[]]
for(i in 1:m){
  x[i]<-pv_map_acc$geometry[[i]][1]
  y[i]<-pv_map_acc$geometry[[i]][2]
}


#####Si queremos reducir el area de analisis

minx<-292000
maxx<-300000
miny<-4600000
maxy<-4610000


rect(minx,miny,maxx,maxy,border="red",lwd=2)

win=owin(xrange=c(minx,maxx), yrange= c(miny, maxy))
##Generate new linnet object (new Linear Network)

LNnew<-pv_map_ln[win]
pv_map_ln<-LNnew

#####Fin si queremos reducir area de analisis



##Necessitem transformar les coordenades del punts a la finestra observació del network
#x<-pv_map_ln$window$xrange[1]+(cord[,1]-min(cord[,1]))/(max(cord[,1])-min(cord[,1]))*(pv_map_ln$window$xrange[2]-pv_map_ln$window$xrange[1])
#y<-pv_map_ln$window$yrange[1]+(cord[,2]-min(cord[,2]))/(max(cord[,2])-min(cord[,2]))*(pv_map_ln$window$yrange[2]-pv_map_ln$window$yrange[1])
#x<-cord[,1]
#y<-cord[,2]
#generate a ppp object


ppp1<-ppp(x,y,window=pv_map_ln$window) ##ppp1 is an ppp pbject


##Generate the lpp object

##Passar de class linnet to class psp
pv_map_psp<-as.psp(LNnew)
is.psp(pv_map_psp)
##Necessitem que l'objecte ppp1 estigu projectat sobre le network. Projectat vol dir que la localització dels punts que es en R2 passi a esser demunt dels segments lineals

Acc_LN<-project2segment(ppp1,pv_map_psp)

##The output ppp object is
Acc_LN$Xproj

Xi<-Acc_LN$Xproj
points(Xi,col="black",pch=19,cex=0.8)

##Necessitem que els punts i  
##Ara ja pots generar el objecte que contingui el point pattern i les carreteres
LN_pp<-lpp(Acc_LN$Xproj,LNnew)
plot(LN_pp,axes=TRUE) 

class(LN_pp)


####Calcul de la probabilitat d'accident####

##Obté el objecte linim

#LN_pp<-lpp(Acc_pp,LNnew)


#LN_pp.km <- rescale(LN_pp, 0.01000, c("km","km"))

#Pixel size the above are meters, for instance pixel side=0.5, 10 meters
#The pixel size depens on the grid size of our covariable output. Try several pixels size and see what is the result of the gridx and gridy size
#rangex<-(LNnew$window$xrange[2]-LNnew$window$xrange[1])#/0.01 #the range is given in meters)/0.01
#rangey<-(LNnew$window$yrange[2]-LNnew$window$yrange[1])#/0.01

#size_px<-1.01 #100 #in meters
gridLx<-100  #rangex/size_px
gridLy<-100 #rangey/size_px

#use the density function to obtain a linim object (a density image)
bw<-0.0001 #200.0 #  1000.4 #bandwidth in km
##la funcio es la funcio de McSwiggan et al (2017) mira llibre Baddeley pag 720-721
#help(density.lpp)

ww<- density(LN_pp, sigma=bw,dimyx=c(gridLy, gridLx)) #,dimyx=c(50, 50))
#ww<- density(LN_pp, sigma=bw) #,dimyx=c(50, 50))

object.size(ww)
memory.limit()
memory.size()
plot(ww) #the scale is of events/km

##Dataset chicago
data(chicago)

ww<-density(unmark(chicago),sigma=10)
object.size(ww)






















Acc<-read.table("Projected_Accidents_Cat.data",header=FALSE)

minx<-278486
maxx<-278486+40000
miny<-4587647
maxy<-4587647+40000


rect(minx,miny,maxx,maxy,border="red",lwd=2)

win=owin(xrange=c(minx,maxx), yrange= c(miny, maxy))
##Generate new linnet object (new Linear Network)

LNnew<-pv.map.ln[win]
#str(LNnew)
#plot this new window of observation
plot(LNnew,axes=TRUE)

Acc_pp<-ppp(Acc$V1,Acc$V2,window=LNnew$window)
LN_pp<-lpp(Acc_pp,LNnew)
plot(LN_pp)


#Pixel size the above are meters, for instance pixel side=0.5, 10 meters
#The pixel size depens on the grid size of our covariable output. Try several pixels size and see what is the result of the gridx and gridy size
rangex<-(LNnew$window$xrange[2]-LNnew$window$xrange[1]) #the range is given in meters)/0.01
rangey<-(LNnew$window$yrange[2]-LNnew$window$yrange[1])

size_px<-500 #100 #in meters
gridLx<-rangex/size_px
gridLy<-rangey/size_px

#use the density function to obtain a linim object (a density image)
bw<-20.0 #  1000.4 #bandwidth in km
##la funcio es la funcio de McSwiggan et al (2017) mira llibre Baddeley pag 720-721
#help(density.lpp)
ww<- density(LN_pp,sigma=bw,dimyx=c(gridLy, gridLx)) #,dimyx=c(50, 50))
plot(ww) #the scale is of events/km
points(Acc_pp,col="green",pch=19)


#x11(width=20,height=20)
#plot(LNnew,axes=TRUE)
####Identify vertices in the Linear network

