# analysis of BB data

# Stephanie K. Archer 04/29/2022

# load packages
source("03_scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("nparLD")
lp("vegan")
lp("viridis")
lp("glmmTMB")
lp("lmerTest")
lp("DHARMa")
lp("patchwork")
lp("ggeffects")
colrs<-read.csv("01_odata/colrs.csv")
theme_set(theme_bw()+theme(panel.grid=element_blank()))

# bb data
# load data
bb<-read.csv("02_wdata/USE_allyrs_seagrass_bb.csv")%>%
  mutate(period=ifelse(yr<2019,"Pre-Dorian","Post-Dorian"),
         date=ym(paste(yr,mnth)))%>%
  filter(abund<6)%>%
  distinct()

bb$period<-factor(bb$period,levels=c("Pre-Dorian","Post-Dorian"))
bb$site<-factor(bb$site)
bb$taxa<-factor(bb$taxa,labels=c("Halodule wrightii","Syringodium filiforme","Thalassia testudinum"))


# because bb data is ordinal we have to do slightly different analyses
# trying out the wilson approach

# at the plot level

bb.wp<-bb%>%
  group_by(period,site,date,plot,taxa)%>%
  mutate(nquad=n(),d.step=abund/nquad)%>%
  summarize(D=sum(d.step))

ggplot(bb.wp,aes(x=date,y=D,color=site))+
  geom_jitter(alpha=.5)+
  geom_smooth(method="lm")+
  facet_grid(taxa~period,scales="free_x")

# at the site level
bb.ws<-bb%>%
  group_by(period,site,date,taxa)%>%
  mutate(nquad=n(),d.step=abund/nquad)%>%
  summarize(D=sum(d.step))

ggplot(bb.ws,aes(x=date,y=D,color=site))+
  geom_jitter(alpha=.5)+
  geom_smooth(method="lm")+
  facet_grid(taxa~period,scales="free_x")

# now calculate van der Maarel method

bb.vm<-bb%>%
  mutate(otv=case_when(
    abund==0~0,
    abund==0.1~1,
    abund==0.01~1,
    abund==0.5~2,
    abund==0.05~2,
    abund==1~3,
    abund==2~5,
    abund==3~7,
    abund==4~8,
    abund==5~9),
    pc=exp((otv-2)/1.38),
    pc=ifelse(abund==0,0,pc))


# at plot level
bb.vmp<-bb.vm%>%
  group_by(period,site,date,plot,taxa)%>%
  summarize(mpc=mean(pc,na.rm=TRUE),
            medpc=median(pc,na.rm=TRUE),
            sdpc=sd(pc,na.rm=TRUE))
  

ggplot(bb.vmp,aes(x=date,y=mpc,color=site))+
  geom_jitter(alpha=.5)+
  geom_smooth(method="lm")+
  facet_grid(taxa~period,scales="free_x")

# at site level
bb.vms<-bb.vm%>%
  group_by(period,site,date,taxa)%>%
  summarize(mpc=mean(pc,na.rm=TRUE),
            medpc=median(pc,na.rm=TRUE),
            sdpc=sd(pc,na.rm=TRUE))


ggplot(bb.vms,aes(x=date,y=mpc,color=site))+
  geom_jitter(alpha=.5)+
  geom_smooth(method="lm")+
  facet_grid(taxa~period,scales="free_x")

# trends look the same visually. 
# analyze trends at BOR, Hills Creek, and Snake Cay before Dorian



