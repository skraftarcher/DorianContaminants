# script to visualize contaminant data

# Yanila Salas-Ortiz and Stephanie K Archer 7/8/2022

# bring in packages and data
source("03_scripts/install_packages_function.R")

lp("tidyverse")
lp("patchwork")

hm<-read.csv("01_odata/contam.csv")

# look at data to make sure it imported properly etc

glimpse(hm)
summary(hm)
theme_set(theme_bw()+theme(panel.grid=element_blank()))
# working through visualizing contaminant by site

# keeping metals as columns
ggplot(data=hm,aes(x=Site,y=Cr))+
  #geom_boxplot()+
  geom_violin()+
  geom_jitter(aes(color=Site),width = .1,alpha=.5)#+
  #geom_bar(stat="identity",position=position_dodge())

# rearranging the data a bit
hm2<-hm%>%
  pivot_longer(Cr:Ba,
               names_to = "heavymetal",
               values_to = "conc")

ggplot(data=hm2,aes(x=Site,y=conc))+
  geom_jitter(aes(color=Site),width = .1,alpha=.5)+
  facet_grid(rows=vars(heavymetal),scales="free")

# summarize data
hm.sum<-hm2%>%
  group_by(Site,heavymetal)%>%
  summarize(m.conc=mean(conc),
            med.conc=median(conc),
            sd.conc=sd(conc),
            se.conc=sd.conc/sqrt(n()))

(plot1<-ggplot()+
  #geom_bar(data=hm.sum,aes(x=Site,y=m.conc),stat="identity")+
  geom_jitter(data=hm2,aes(x=Site,y=conc,color=Site),width = .1,alpha=.2)+
  geom_point(data=hm.sum,aes(x=Site,y=m.conc,color=Site,fill=Site),shape=23,size=2)+
  geom_errorbar(data=hm.sum,aes(x=Site,ymin=m.conc-se.conc,ymax=m.conc+se.conc,color=Site),width=.1)+
  facet_grid(rows=vars(heavymetal),scales="free")+
  ylab("Concentration (mg/kg)")+
  theme(legend.position = "none")+
  scale_color_viridis_d(option="B",end=.8)+
  scale_fill_viridis_d(option="B",end=.8))

ggsave(plot=plot1,"04_figures/sediment_heavymetals_december2021.jpg",width=4,height=10,dpi=300)
# for one heavy metal

(pb<-ggplot()+
  #geom_bar(data=hm.sum,aes(x=Site,y=m.conc),stat="identity")+
  geom_jitter(data=hm2%>%
                filter(heavymetal=="Pb"),aes(x=Site,y=conc,color=Site),width = .1,alpha=.2)+
  geom_point(data=hm.sum%>%
               filter(heavymetal=="Pb"),aes(x=Site,y=m.conc,color=Site,fill=Site),shape=23,size=2)+
  geom_errorbar(data=hm.sum%>%
                  filter(heavymetal=="Pb"),aes(x=Site,ymin=m.conc-se.conc,ymax=m.conc+se.conc,color=Site),width=.1)+
  ylab("Concentration (mg/kg)")+
    ggtitle("Pb")+
  theme(legend.position = "none")+
  scale_color_viridis_d(option="B",end=.8)+
  scale_fill_viridis_d(option="B",end=.8))


(cr<-ggplot()+
    #geom_bar(data=hm.sum,aes(x=Site,y=m.conc),stat="identity")+
    geom_jitter(data=hm2%>%
                  filter(heavymetal=="Cr"),aes(x=Site,y=conc,color=Site),width = .1,alpha=.2)+
    geom_point(data=hm.sum%>%
                 filter(heavymetal=="Cr"),aes(x=Site,y=m.conc,color=Site,fill=Site),shape=23,size=2)+
    geom_errorbar(data=hm.sum%>%
                    filter(heavymetal=="Cr"),aes(x=Site,ymin=m.conc-se.conc,ymax=m.conc+se.conc,color=Site),width=.1)+
    ylab("Concentration (mg/kg)")+
    theme(legend.position = "none")+
    ggtitle("Cr")+
    scale_color_viridis_d(option="B",end=.8)+
    scale_fill_viridis_d(option="B",end=.8))


(v<-ggplot()+
    #geom_bar(data=hm.sum,aes(x=Site,y=m.conc),stat="identity")+
    geom_jitter(data=hm2%>%
                  filter(heavymetal=="V"),aes(x=Site,y=conc,color=Site),width = .1,alpha=.2)+
    geom_point(data=hm.sum%>%
                 filter(heavymetal=="V"),aes(x=Site,y=m.conc,color=Site,fill=Site),shape=23,size=2)+
    geom_errorbar(data=hm.sum%>%
                    filter(heavymetal=="V"),aes(x=Site,ymin=m.conc-se.conc,ymax=m.conc+se.conc,color=Site),width=.1)+
    ylab("Concentration (mg/kg)")+
    theme(legend.position = "none")+
    ggtitle("V")+
    scale_color_viridis_d(option="B",end=.8)+
    scale_fill_viridis_d(option="B",end=.8))


(pb + plot_spacer() + cr)/(plot_spacer()+v+plot_spacer())


