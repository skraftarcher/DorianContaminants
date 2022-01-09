# script to organize the different seagrass datasets into one with 
# bb seagrass abundances
# bb macroalgal abundances
# thalassia shoot densities
# all seagrass shoot densities

# at the same time we'll build a database site names, abbreviations, and 
# full names

# Stephanie K. Archer January 8, 2022

# load required packages
source("03_scripts/install_packages_function.R")
lp("tidyverse")
lp("readxl")
lp("lubridate")

# load all the datasets
pre.bb<-read.csv("02_wdata/pre_bb_all.csv")
pre.bethsd<-read.csv("02_wdata/beth_pre_shd.csv")
pre.stephsd<-read_xlsx("01_odata/pre_shd_steph.xlsx")
nov19<-read.csv("01_odata/Nov2019_seagrass.csv")
nov19.wypts<-read_xlsx("01_odata/Nov2019_waypoints.xlsx")
may20<-read.csv("01_odata/May2020_seagrass.csv")
june21<-read.csv("01_odata/June2021_seagrass.csv")
dec21<-read.csv("01_odata/December2021_seagrass.csv")
taxa<-read.csv("01_odata/bbtaxa.csv")
colnames(taxa)[1]<-"taxa"
sites<-read_xlsx("01_odata/sg_sites.xlsx")


# remind myself of the structure of the pre dataset
colnames(pre.bb)

# Start with Nov 2019
#remind myself what I'm working with
glimpse(nov19)
# do a bit of organizing that will need for both datasets
nov19<-nov19%>%
  mutate(date=dmy(Date),
         yr=year(date),
         mnth=month(date))%>%
  left_join(nov19.wypts)

# create the shoot density data set
nov19.sd<-nov19%>%
  select(site=site2,yr,mnth,lat,long,plot=Quadrat,Dnsty1Tt,Dnsty2Tt,Dnsty3Tt,Dnsty4Tt)%>%
  pivot_longer(Dnsty1Tt:Dnsty4Tt,names_to = "trash",values_to = "count")%>%
  mutate(shd=count/(.1*.1),
         shd=ifelse(is.na(shd),0,shd))%>%
  separate(trash,into=c("trash1","quadrat","trash2"),sep=c(5,6))%>%
  mutate(quadrat=as.numeric(quadrat))%>%
  select(site,plot,quadrat,lat,long,yr,mnth,shd)

#now create the bb dataset
nov19.bb<-nov19%>%
  select(site=site2,yr,mnth,lat,long,plot = Quadrat,Tt:ANEM)%>%
  pivot_longer(Tt:ANEM,names_to="taxa",values_to="abund")%>%
  mutate(abund=ifelse(is.na(abund),0,abund),#change NAs to 0s
         quadrat=1)%>%
  distinct()%>%#make sure there are no duplicates
  select(site,yr,mnth,lat,long,plot,quadrat,taxa,abund)

# Now May 2020
# first make shoot density dataset
# first make a dataset that has all site/plot/quadrats 
may20.sda<-may20%>%
  mutate(date=dmy(Date),
         yr=year(date),
         mnth=month(date))%>%
  select(site=Site,
         plot=Plot,quadrat=Quadrat,
         yr,mnth,
         lat=Latitude,long=Longitude)%>%
  distinct()

may20.sdb<-may20%>%
  filter(Taxa=="T")%>%
  mutate(date=dmy(Date),
         yr=year(date),
         mnth=month(date))%>%
  select(site=Site,
         plot=Plot,quadrat=Quadrat,
         yr,mnth,
         lat=Latitude,long=Longitude,
         shd=Density)%>%
  mutate(shd=ifelse(is.na(shd),0,shd),
         shd=shd/(.1*.1))%>%
  group_by(site,plot,quadrat,yr,mnth,lat,long)%>%
  summarize(shd=mean(shd))

# now join the shoot density dataset to the other so that all plots have a shoot
# density even the 0s
may20.sd<-left_join(may20.sda,may20.sdb)%>%
  mutate(shd=ifelse(is.na(shd),0,shd))

# now make bb dataset
may20.bb<-may20%>%
  mutate(date=dmy(Date),
         yr=year(date),
         mnth=month(date))%>%
  select(site=Site,
         plot=Plot,quadrat=Quadrat,
         yr,mnth,
         lat=Latitude,long=Longitude,
         taxa=Taxa,abund=Abund)%>%
  filter(taxa!="O")%>%
  pivot_wider(names_from=taxa,values_from = abund,values_fill = 0)%>%
  pivot_longer(`T`:DIC,names_to="taxa",values_to="abund")

#Now June 2021
#First do some general organizing
# NOTE - I NEED TO ADD IN LAT/LONGS HERE

june21<-june21%>%
  mutate(date=mdy(Date),
         yr=year(date),
         mnth=month(date))

june21a<-june21%>%
  select(site=Site,plot=Plot,quadrat=Quadrat,yr,mnth)%>%
  distinct()

june21b<-june21%>%
  filter(Taxa=="T")%>%
  select(site=Site,
         plot=Plot,quadrat=Quadrat,
         yr,mnth,
         lat=Latitude,long=Longitude,
         shd=Density)%>%
  mutate(shd=ifelse(is.na(shd),0,shd),
         shd=shd/(.0144))

june21.sd<-left_join(june21a,june21b)%>%
  mutate(shd=ifelse(is.na(shd),0,shd))

# now bb
june21.bb<-june21%>%
  select(site=Site,
         plot=Plot,quadrat=Quadrat,
         yr,mnth,
         lat=Latitude,long=Longitude,
         taxa=Taxa,abund=Abund)%>%
  pivot_wider(names_from=taxa,values_from = abund,values_fill = 0)%>%
  pivot_longer(H:CPT,names_to="taxa",values_to="abund")


#Now dec 2021
#First do some general organizing
# NOTE - I NEED TO ADD IN LAT/LONGS HERE

dec21<-dec21%>%
  mutate(date=dmy(Date),
         yr=year(date),
         mnth=month(date),
         lat=NA,
         long=NA)

dec21a<-dec21%>%
  select(site=Site,plot=Plot,quadrat=Quadrat,yr,mnth)%>%
  distinct()

dec21b<-dec21%>%
  filter(Taxa=="T")%>%
  select(site=Site,
         plot=Plot,quadrat=Quadrat,
         yr,mnth,
         lat,long,
         shd=Density)%>%
  mutate(shd=ifelse(is.na(shd),0,shd),
         shd=shd/(.1*.1))

dec21.sd<-left_join(dec21a,dec21b)%>%
  mutate(shd=ifelse(is.na(shd),0,shd))

# now bb - get rid of first five plots at treasure cay as there was a data
#recording error
dec21.bb<-dec21%>%
  select(site=Site,
         plot=Plot,quadrat=Quadrat,
         yr,mnth,
         lat,long,
         taxa=Taxa,abund=Abund)%>%
  pivot_wider(names_from=taxa,values_from = abund,values_fill = 0)%>%
  pivot_longer(CGP:GO,names_to="taxa",values_to="abund")

dec21.bbTC<-dec21.bb%>%
  filter(site=="Treasure Cay")%>%
  filter(plot <6)

dec21.bb<-anti_join(dec21.bb,dec21.bbTC)
#Now make big datasets for future analysis and figures
#shoot density
all.sd<-bind_rows(pre.bethsd,pre.stephsd,nov19.sd,may20.sd,june21.sd,dec21.sd)

# fix all the different ways sites were entered

all.sd<-all.sd%>%
  left_join(sites)%>%
  select(site=corrected,yr,mnth,lat,long,plot,quadrat,shd)

# save this dataset
write.csv(all.sd,"02_wdata/USE_allyrs_shootdensity.csv",row.names = FALSE)

# bb dataset

all.bb<-bind_rows(pre.bb,nov19.bb,may20.bb,june21.bb,dec21.bb)

# fix all the different ways sites were entered

all.bb<-all.bb%>%
  left_join(sites)%>%
  select(-site)%>%
  select(site=corrected,yr,mnth,lat,long,plot,quadrat,taxa,abund)


# save this

# save this dataset
write.csv(all.bb,"02_wdata/USE_allyrs_alltaxa_bb.csv",row.names = FALSE)

# now make just seagrass and just macroalgae datasets
taxa.sg<-filter(taxa,group=="sg")
taxa.ma<-filter(taxa,group=="ma")

all.bb.sg<-filter(all.bb,taxa %in% taxa.sg$taxa)%>%
  mutate(taxa=case_when(
    taxa=="T"~"T",
    taxa=="H"~"H",
    taxa=="S"~"S",
    taxa=="Tt"~"T",
    taxa=="Hw"~"H",
    taxa=="Sf"~"S"))

write.csv(all.bb.sg,"02_wdata/USE_allyrs_seagrass_bb.csv",row.names = FALSE)

all.bb.ma<-filter(all.bb,taxa %in% taxa.ma$taxa)
write.csv(all.bb.ma,"02_wdata/USE_allyrs_algae_bb.csv",row.names = FALSE)
