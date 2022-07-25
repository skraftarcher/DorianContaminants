# Developing a long-term understanding of seagrass patterns on Abaco. Pre- and post-Dorian.
# Script written by S. Archer 6/22/20

# This script makes a figure examining site-specific trends in Thalassia testudinum shoot densities over time.
# ---- packages.figs ----
# packages necessary for this script----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(sf))install.packages("sf");library(sf)
if(!require(rgdal))install.packages("rgdal");library(rgdal)
if(!require(sp)) install.packages('sp');library(sp)
if(!require(readxl)) install.packages('readxl');library(readxl)
if(!require(ggspatial)) install.packages('ggspatial');library(ggspatial)

#----load datasets----
land<-st_read("01_odata/land_polygons.shp",)# read in shape file
dt<-read_xlsx("01_odata/dorian_track.xlsx",sheet = "Sheet2")# read in dorian's track
b<-st_crop(land,xmin=-82,xmax=-76,ymin=23,ymax=28)# create the cropped map for the overview map
a<-st_crop(b,xmin=-78,xmax=-76.93,ymin=25.6,ymax=27.1)# create the cropped map for the abaco map
sites<-read_xlsx("01_odata/sites.xlsx") # load in the site coordinates
sites$Site<-factor(sites$Site,levels = c("Treasure Cay","Hills Creek","Man O War","Hope Town","Camp Abaco", "Snake Cay","Bight of Old Robinson"))
sites<-sites%>%
    filter(!Site %in% c("Man O War","Hope Town"))
osites<-read_xlsx("01_odata/sites.xlsx",sheet="oysters")%>%
    filter(Monitoring==1)%>%
    filter(!Site %in% c("Man O War","Man o War","Hope Town"))
osites$Site<-factor(osites$Site,levels=c("Treasure Cay","Hills Creek","Man O War","Hope Town","Marsh Harbour","Camp Abaco", "Snake Cay","Little Harbour","Crossing Rocks","Cross Harbour","Great Harbour Cay"))
mhsites<-read_xlsx("01_odata/sites.xlsx",sheet="MH_Oysters")
colrs<-read.csv("working_data/colrs.csv")[,-1]
bsites<-read_xlsx("01_odata/sites.xlsx",sheet="all")
bsites$Site<-factor(bsites$Site,levels=c("Treasure Cay","Hills Creek","Man O War","Hope Town","Marsh Harbour","Camp Abaco", "Snake Cay","Little Harbour","Crossing Rocks","Cross Harbour","Great Harbour Cay"))
colrs<-read.csv("01_odata/colrs.csv")

# ---- overview figure ----
(ov<-ggplot(b)+ # make the overview map
   geom_sf(fill="grey",size=.75)+
   theme_bw()+
   geom_rect(aes(xmin=-78,xmax=-76.93,ymin=25.8,ymax=27.1),color="black",alpha=0)+
   theme(panel.grid = element_blank(),axis.text = element_blank(),
         axis.ticks = element_blank(),panel.border =  element_rect(size=1)))
ggsave("04_figures/overview_map.jpg",width = 4, height=4)

# ---- seagrassmap----
sg.clrs<-colrs[colrs$site %in% c(as.character(sites$Site),"BOR"),]
ggplot()+ # make the seagrass site map----
    #geom_ribbon(aes(x=LON,ymin=S,ymax=N),data=dt, fill = "black",alpha=.15)+    
    geom_sf(data=a,fill="grey")+    
    #geom_line(aes(x=LON,y=LAT),size=1.5,data=dt,color="black")+  
    theme_bw()+
    geom_point(aes(x=Lon,y=Lat),data=sites,size=3.5)+
    geom_point(aes(x=Lon,y=Lat,color=Site),data=sites,size=2.5)+
    scale_color_manual(values=sg.clrs[,6])+
    annotation_scale(location="tr",width_hint=0.25,pad_x = unit(0.1,"in"),pad_y=unit(.8,"in"))+
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)+
    ylab("")+xlab("")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size=10))
ggsave("04_figures/seagrass_abaco.jpg")

#close up on bight
bor.sites<-read_xlsx("01_odata/sites.xlsx",sheet="BOR")
bor.col<-colrs%>%
    filter(site %in% unique(bor.sites$site))
bor<-st_crop(b,xmin=-77.05,xmax=-76.93,ymin=26.38,ymax=26.31)# create the cropped map for the abaco map
ggplot()+ # make the seagrass site map----
#geom_ribbon(aes(x=LON,ymin=S,ymax=N),data=dt, fill = "black",alpha=.15)+    
geom_sf(data=bor,fill="grey")+    
    #geom_line(aes(x=LON,y=LAT),size=1.5,data=dt,color="black")+  
    theme_bw()+
    geom_point(aes(x=lon,y=lat),data=bor.sites,size=4.5)+
    geom_point(aes(x=lon,y=lat,color=site),data=bor.sites,size=3.5)+
    scale_color_manual(values=bor.col[,6],labels=bor.col[,2])+
    annotation_scale(location="br",width_hint=0.25,pad_x = unit(0.1,"in"),pad_y=unit(.8,"in"))+
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)+
    ylab("")+xlab("")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size=10),
          panel.grid = element_blank())
ggsave("04_figures/bor_sites.jpg")
# ----oystermap----

o.clrs<-colrs[colrs$site.name %in% osites$Site,]
osites$Site<-factor(osites$Site,levels=o.clrs$site.name)
ggplot()+ # make the oyster site map----
    geom_ribbon(aes(x=LON,ymin=S,ymax=N),data=dt, fill = "black",alpha=.15)+    
    geom_sf(data=a,fill="grey")+    
    geom_line(aes(x=LON,y=LAT),size=1.5,data=dt,color="black")+  
    theme_bw()+
    geom_point(aes(x=Lon,y=Lat),data=osites,size=3.5)+
    geom_point(aes(x=Lon,y=Lat,color=Site),data=osites,size=2.5)+
    scale_color_manual(values=o.clrs[,4])+
    annotation_scale(location="tr",width_hint=0.25,pad_x = unit(0.1,"in"),pad_y=unit(.8,"in"))+
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)+
    ylab("")+xlab("")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size=14))+
    guides(color=guide_legend(ncol=1))
ggsave("04_figures/oyster_abaco.jpg")

# ---- mhoystermap ----
mh<-st_crop(a,xmin=-77.07,xmax=-77.03,ymin=26.53,ymax=26.6)# create a cropped version for marsh harbour

mhsites$Site<-factor(mhsites$Site)
ggplot()+ # make the oyster site map----
    geom_sf(data=mh,fill="grey")+    
    theme_bw()+
    theme(panel.grid = element_blank())+
    geom_point(aes(x=Lon,y=Lat,color=Site),data=mhsites,size=5)+
    annotation_scale(location="br",width_hint=0.25,pad_x = unit(0.1,"in"),pad_y=unit(.1,"in"))+
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering)+
    ylab("")+xlab("")+
    scale_color_viridis_d()
ggsave("04_figures/Marsh_harbour_oysters.jpg")

# combined map ----
b.clrs<-colrs[colrs$site %in% bsites$Site,]
bsites<-bsites[order(bsites$Site),]
ggplot()+ 
geom_ribbon(aes(x=LON,ymin=S,ymax=N),data=dt, fill = "black",alpha=.15)+    
    geom_sf(data=a,fill="grey")+    
    geom_line(aes(x=LON,y=LAT),size=1.5,data=dt,color="black")+  
    theme_bw()+
    geom_point(aes(x=Lon,y=Lat,shape=dt),data=bsites,size=3.5)+
    geom_point(aes(x=Lon,y=Lat,color=Site,shape=dt),data=bsites,size=2.5)+
    scale_color_manual(values=b.clrs[,3],
                       breaks = bsites$Site,
                       labels = c(bsites$UseName[1:7],"Little Harbour\nBight of Old Robinson",bsites$UseName[9:11]))+
    annotation_scale(location="tr",width_hint=0.25,pad_x = unit(0.1,"in"),pad_y=unit(.8,"in"))+
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)+
    ylab("")+xlab("")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size=14))+
    guides(color=guide_legend(ncol=1))
ggsave("figures/allsites_abaco.jpg")

# dolphin map ----
#b.clrs<-colrs[colrs$site %in% bsites$Site,]
#bsites<-bsites[order(bsites$Site),]
c<-st_crop(a,xmin=-77.4,xmax=-76,ymin=26.2,ymax=26.8)

ggplot()+ 
    geom_sf(data=c,fill="grey")+    
    theme_bw()+
    geom_point(aes(x=long,y=lat),data=dsites,size=2.5)+
    geom_point(aes(x=long,y=lat,fill=Site),shape=24,data=rsites,size=2.5)+
    # scale_color_manual(values=b.clrs[,3],
    #                    breaks = bsites$Site,
    #                    labels = c(bsites$UseName[1:7],"Little Harbour\nBight of Old Robinson",bsites$UseName[9:11]))+
    annotation_scale(location="tr",width_hint=0.25,pad_x = unit(0.1,"in"),pad_y=unit(.8,"in"))+
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)+
    ylab("")+xlab("")+
    theme(legend.text = element_text(size=14),
          legend.title=element_text(size=14))+
    guides(fill=guide_legend(ncol=1,"Acoustic Recorders"))
ggsave("figures/dolphins_abaco.jpg")

# permit map---
psites<-bsites%>%
    filter(Site %in% c("Treasure Cay","Hills Creek","Marsh Harbour","Snake Cay","Camp Abaco","Little Harbour"))

p.clrs<-colrs[colrs$site %in% psites$Site,]
psites<-psites[order(psites$Site),]
ggplot()+ 
    geom_ribbon(aes(x=LON,ymin=S,ymax=N),data=dt, fill = "black",alpha=.15)+    
    geom_sf(data=a,fill="grey")+    
    geom_line(aes(x=LON,y=LAT),size=1.5,data=dt,color="black")+  
    theme_bw()+
    geom_point(aes(x=Lon,y=Lat),data=psites,size=3.5)+
    geom_point(aes(x=Lon,y=Lat,color=Site),data=psites,size=2.5)+
    scale_color_manual(values=p.clrs[,3],
                       breaks = psites$Site,
                       labels = c(psites$UseName[1:5],"Little Harbour\nBight of Old Robinson"))+
    annotation_scale(location="tr",width_hint=0.25,pad_x = unit(0.1,"in"),pad_y=unit(.8,"in"))+
    annotation_north_arrow(location = "tr", which_north = "true", 
                           pad_x = unit(0.3, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering)+
    ylab("")+xlab("")+
    theme(legend.title = element_blank(),
          legend.text = element_text(size=14))+
    guides(color=guide_legend(ncol=1))
ggsave("figures/permitsites_abaco.jpg")

