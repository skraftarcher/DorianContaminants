# pre-analysis data visualization for seagrass

# Stephanie K. Archer 01/08/2021

# load packaeges
source("03_scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")

# bb data
# load data
bb<-read.csv("02_wdata/USE_allyrs_seagrass_bb.csv")

bb2<-bb%>%
  mutate(date=my(paste(mnth,yr)))%>%
  group_by(site,date,taxa)%>%
  summarize(Abundance=mean(abund),se=sd(abund)/(sqrt(n()+1)))

ggplot(bb2%>%
         filter(site %in% c("BOR2","BOR3","Camp Abaco","Hills Creek",
                            "Snake Cay",
                            "Jerrys","Treasure Cay")))+
  geom_vline(xintercept=my("09-2019"),size=2,alpha=.5,linetype="dashed")+
  geom_line(aes(x=date,y=Abundance,group=site,color=site),size=1.25,alpha=.5)+
  geom_point(aes(x=date,y=Abundance,color=site),size=2)+
  #scale_color_viridis_d()+
  facet_wrap(~taxa)

# shoot density data
# load data
sd<-read.csv("02_wdata/USE_allyrs_shootdensity.csv")

sd2<-sd%>%
  mutate(date=my(paste(mnth,yr)))%>%
  group_by(site,date)%>%
  summarize(ShootDensity=mean(shd),se=sd(shd)/(sqrt(n()+1)))

ggplot(sd2%>%
         filter(!site %in% c("Hope Town","Man o War")))+
  geom_vline(xintercept=my("09-2019"),size=2,alpha=.5,linetype="dashed")+
  geom_line(aes(x=date,y=ShootDensity,group=site,color=site),size=1.25,alpha=.5)+
  geom_point(aes(x=date,y=ShootDensity,color=site),size=2)+
  geom_errorbar(aes(x=date,ymin=ShootDensity-se,ymax=ShootDensity+se,color=site))
