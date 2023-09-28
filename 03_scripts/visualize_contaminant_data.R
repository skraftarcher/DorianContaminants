# script to visualize contaminant data

# Yanila Salas-Ortiz and Stephanie K Archer 7/8/2022

# bring in packages and data
source("03_scripts/install_packages_function.R")
#source("03_scripts/01_pull_google_drive_data-EX.R")#only run if you think data updated
lp("tidyverse")
lp("patchwork")
lp("readxl")
lp("sf")
lp("sp")
lp("ggspatial")
lp("readxl")

hm.sed<-read.csv("01_odata/sediment_contaminants.csv")
hm.wat<-read.csv("01_odata/water_contaminants.csv")
hm.sg<-read.csv("02_wdata/seagrass_contaminants.csv")
sites<-read_xlsx("01_odata/sites.xlsx",sheet = "december")%>%
  mutate(Site=Site2)
hm.levels<-read_xlsx("01_odata/hm_loads_sharifuzzaman.xlsx")





# land<-st_read("01_odata/land_polygons.shp",)# read in shape file
# dt<-read_xlsx("01_odata/dorian_track.xlsx",sheet = "Sheet2")# read in dorian's track
# b<-st_crop(land,xmin=-82,xmax=-76,ymin=23,ymax=28)# create the cropped map for the overview map
# a<-st_crop(b,xmin=-78,xmax=-76.93,ymin=25.6,ymax=27.1)# create the cropped map for the abaco map

# look at data to make sure it imported properly etc

theme_set(theme_bw()+theme(panel.grid=element_blank()))

# look at water
hm.wat$sampling<-factor(hm.wat$sampling,levels=c("November2019","May2020","June2021","December2021"))

hm.wat2<-hm.wat%>%
  pivot_longer(Cr:Mo,names_to="hm",values_to="conc")


ggplot(data=hm.wat2%>%
         filter(hm %in% c("As","Cr","Cu","Ni","Pb","V","Zn"))%>%
         filter(Site!="Crossing Rocks"))+
  geom_boxplot(aes(y=conc,fill=sampling))+
  facet_grid(hm~Site,scales="free")

# look at sediment
hm.sed$sampling<-factor(hm.sed$sampling,levels=c("November2019","May2020","June2021","December2021"))

hm.sed2<-hm.sed%>%
  pivot_longer(Pb:V,names_to="hm",values_to="conc")


ggplot(data=hm.sed2%>%
         filter(hm %in% c("As","Cr","Cu","Ni","Pb","V","Zn"))%>%
         filter(Site!="Crossing Rocks"))+
  geom_boxplot(aes(y=conc,fill=sampling))+
  facet_grid(hm~Site,scales="free")

# look at seagrass
hm.sg$sampling<-factor(hm.sg$sampling,levels=c("November2019","May2020","June2021","December2021"))

hm.sg2<-hm.sg%>%
  pivot_longer(Pb:V,names_to="hm",values_to="conc")%>%
  mutate(Site=ifelse(Site=="Jerry's","Jerrys",Site))


ggplot(data=hm.wat2%>%
         filter(hm %in% c("As","Cr","Cu","Ni","Pb","V","Zn"))%>%
         filter(Site%in%c("Camp Abaco","Hills Creek","Snake Cay","Treasure Cay")))+
  geom_boxplot(aes(y=conc,fill=sampling))+
  facet_wrap(hm~Site,scales="free")


