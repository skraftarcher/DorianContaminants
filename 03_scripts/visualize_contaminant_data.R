# script to visualize contaminant data

# Yanila Salas-Ortiz and Stephanie K Archer 7/8/2022

# bring in packages and data
source("03_scripts/install_packages_function.R")
source("03_scripts/01_pull_google_drive_data-EX.R")
lp("tidyverse")
lp("patchwork")
lp("readxl")
lp("sf")
lp("sp")
lp("ggspatial")
lp("readxl")

hm<-read.csv("01_odata/contam.csv")
sites<-read_xlsx("01_odata/sites.xlsx",sheet = "december")%>%
  mutate(Site=Site2)
hm.levels<-read_xlsx("01_odata/hm_loads_sharifuzzaman.xlsx")

# land<-st_read("01_odata/land_polygons.shp",)# read in shape file
# dt<-read_xlsx("01_odata/dorian_track.xlsx",sheet = "Sheet2")# read in dorian's track
# b<-st_crop(land,xmin=-82,xmax=-76,ymin=23,ymax=28)# create the cropped map for the overview map
# a<-st_crop(b,xmin=-78,xmax=-76.93,ymin=25.6,ymax=27.1)# create the cropped map for the abaco map

# look at data to make sure it imported properly etc

glimpse(hm)
summary(hm)
theme_set(theme_bw()+theme(panel.grid=element_blank()))


# rearranging the data a bit

mhnames<-c("Jib Room",
           "Fish House",
           "Rainbow Rentals",
           "Boat Harbour",
           "Calcutta")
hm2<-hm%>%
  pivot_longer(Cr:Ba,
               names_to = "heavymetal",
               values_to = "conc")%>%
  mutate(Site2=case_when(
    Site %in% mhnames~"Marsh Harbour",
    Site %in% c("BOR2","BOR 2","Jerrys","Jerry's")~"BOR",
    !Site %in% c("BOR2","BOR 2","Jerrys",mhnames)~Site),
    Site=ifelse(Site %in% c("BOR2","BOR 2"),"BOR",Site),
    sg_m=ifelse(Site2 %in% c("Marsh Harbour","Treasure Cay Marina"),"Harbour or Marina","Seagrass Bed"))%>%
  group_by(heavymetal)%>%
  mutate(bn=min(conc),
         igeo=log(conc/(1.5*bn)),
         CF=conc/bn)

hm2$Site2<-factor(hm2$Site2,levels=c("Treasure Cay",
                                     "Treasure Cay Marina",
                                     "Hills Creek",
                                     "Marsh Harbour",
                                     "Camp Abaco",
                                     "Snake Cay",
                                     "BOR"))

hm2$Site<-factor(hm2$Site,levels=c("Treasure Cay",
                                   "Treasure Cay Marina",
                                   "Hills Creek",
                                   "Jib Room",
                                   "Fish House",
                                   "Rainbow Rentals",
                                   "Boat Harbour",
                                   "Calcutta",
                                   "Camp Abaco",
                                   "Snake Cay",
                                   "BOR",
                                   "Jerry's"))

hm2.colors<-data.frame(Site=unique(hm2$Site2))%>%
  left_join(sites)

hm2.colors$Site<-factor(hm2.colors$Site,levels=c("Treasure Cay",
                                                 "Treasure Cay Marina",
                                                 "Hills Creek",
                                                 "Marsh Harbour",
                                                 "Camp Abaco",
                                                 "Snake Cay",
                                                 "BOR"))



hm2.colors2<-data.frame(Site=unique(hm2$Site))%>%
  left_join(sites)

hm2.colors2$Site<-factor(hm2.colors2$Site,levels=c("Treasure Cay",
                                                  "Treasure Cay Marina",
                                                  "Hills Creek",
                                                  "Jib Room",
                                                  "Fish House",
                                                  "Boat Harbour",
                                                  "Calcutta",
                                                  "Camp Abaco",
                                                  "Snake Cay",
                                                  "BOR",
                                                  "Jerry's"))
hm2.colors2<-arrange(hm2.colors2,Site)
hm2.colrs3<-filter(hm2.colors2,Site %in% c("Treasure Cay",
                                           "Treasure Cay Marina",
                                           "Hills Creek",
                                           "Camp Abaco",
                                           "Snake Cay",
                                           "BOR"))

hm2.colors4<-filter(hm2.colors2,!Site %in% c("Treasure Cay",
                                           "Treasure Cay Marina",
                                           "Hills Creek",
                                           "Camp Abaco",
                                           "Snake Cay",
                                           "BOR"))


# visualize contaminant data by site
ggplot(data=hm2,aes(x=Site,y=igeo))+
  geom_hline(aes(yintercept=1),linetype="dashed",alpha=.7)+
  geom_jitter(aes(color=Site),width = .1,alpha=.5)+
  #geom_boxplot(aes(color=Site2),alpha=.5)+
  facet_grid(rows=vars(heavymetal),cols = vars(sg_m),scales="free")+
  scale_color_manual(values=hm2.colors2$colr)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        axis.title.x=element_blank())+
  ylab("mg/kg")


ggsave("04_figures/contaminants_by_site.png",width=7,height=9)


# visualize contaminant data by site with reference levels
hm3<-left_join(hm2,hm.levels)%>%
  filter(!is.na(TEL))

ggplot(data=hm3,aes(x=Site,y=conc))+
  geom_jitter(aes(color=Site),width = .1,alpha=.5)+
  geom_hline(aes(yintercept=PEL),linetype="dashed")+
  #geom_boxplot(aes(color=Site2),alpha=.5)+
  facet_grid(rows=vars(heavymetal),cols = vars(sg_m),scales="free")+
  scale_color_manual(values=hm2.colors2$colr)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        axis.title.x=element_blank())+
  ylab("mg/kg")


ggsave("04_figures/contaminants_by_site_all_PEL.png",width=7,height=9)

ggplot(data=hm3%>%
         filter(sg_m=="Seagrass Bed")%>%
         filter(conc<1000),aes(x=Site,y=conc))+
  geom_jitter(aes(color=Site),width = .1,alpha=.5)+
  geom_hline(aes(yintercept=PEL),linetype="dashed")+
  #geom_boxplot(aes(color=Site2),alpha=.5)+
  facet_grid(rows=vars(heavymetal),cols = vars(sg_m),scales="free")+
  scale_color_manual(values=hm2.colrs3$colr)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        axis.title.x=element_blank())+
  ylab("mg/kg")


ggsave("04_figures/contaminants_by_site_seagrass_PEL.png",width=7,height=9)

ggplot(data=hm3%>%
         filter(sg_m=="Harbour or Marina"),aes(x=Site,y=conc))+
  geom_jitter(aes(color=Site),width = .1,alpha=.5)+
  geom_hline(aes(yintercept=PEL),linetype="dashed")+
  #geom_boxplot(aes(color=Site2),alpha=.5)+
  facet_grid(rows=vars(heavymetal),cols = vars(sg_m),scales="free")+
  scale_color_manual(values=hm2.colors2$colr)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        axis.title.x=element_blank())+
  ylab("mg/kg")


ggsave("04_figures/contaminants_by_site_harbour_PEL.png",width=7,height=9)

ggplot(data=hm3,aes(x=Site,y=conc))+
  geom_jitter(aes(color=Site),width = .1,alpha=.5)+
  geom_hline(aes(yintercept=mod_pol),linetype="dashed",color="orange")+
  geom_hline(aes(yintercept=heavy_pol),linetype="dashed",color="red")+
  geom_hline(aes(yintercept=PEL),linetype="dashed")+
  #geom_boxplot(aes(color=Site2),alpha=.5)+
  facet_grid(rows=vars(heavymetal),cols = vars(sg_m),scales="free")+
  scale_color_manual(values=hm2.colors2$colr)+
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45,vjust=1,hjust=1),
        axis.title.x=element_blank())+
  ylab("mg/kg")


ggsave("04_figures/contaminants_by_site_all_polution.png",width=7,height=9)

ggplot(data=hm3%>%
         filter(sg_m=="Seagrass Bed"),aes(x=Site,y=conc))+
  geom_jitter(aes(color=Site),size=3,width = .1,alpha=.5)+
  geom_hline(aes(yintercept=mod_pol),linetype="dashed",color="orange")+
  geom_hline(aes(yintercept=heavy_pol),linetype="dashed",color="red")+
  geom_hline(aes(yintercept=PEL),linetype="dashed")+
  #geom_boxplot(aes(color=Site2),alpha=.5)+
  facet_wrap(~heavymetal,ncol=3,scales="free")+
  scale_color_manual(values=hm2.colrs3$colr)+
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        strip.text = element_text(size=12))+
  ylab("mg/kg")


ggsave("04_figures/contaminants_by_site_sg_polution.png",width=10,height=7)


ggplot(data=hm3%>%
         filter(sg_m=="Harbour or Marina"),aes(x=Site,y=conc))+
  geom_jitter(aes(color=Site),size=3,width = .1,alpha=.5)+
  geom_hline(aes(yintercept=mod_pol),linetype="dashed",color="orange")+
  geom_hline(aes(yintercept=heavy_pol),linetype="dashed",color="red")+
  geom_hline(aes(yintercept=PEL),linetype="dashed")+
  #geom_boxplot(aes(color=Site2),alpha=.5)+
  facet_wrap(~heavymetal,ncol=3,scales="free")+
  scale_color_manual(values=hm2.colors4$colr)+
  theme(legend.position="top",
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        strip.text = element_text(size=12))+
  ylab("mg/kg")


ggsave("04_figures/contaminants_by_site_harbours_polution.png",width=10,height=7)

# Make igeo figure
(ideop<-ggplot(data=hm2)+
  geom_jitter(aes(x=Site,y=igeo,color=Site),size=2,alpha=.3,width=.1)+
  geom_hline(aes(yintercept=1),linetype="dashed",color="orange")+
  geom_hline(aes(yintercept=4),linetype="dashed",color="red")+
  geom_hline(aes(yintercept=5),linetype="dashed",color="black")+
    scale_color_manual(values=hm2.colors2$colr)+
  facet_wrap(vars(heavymetal),ncol=2))

  

# calculate site specific pllution load index
hm4<-hm2%>%
  group_by(Site,Sample.ID,sg_m)%>%
  select(Site,Sample.ID,sg_m,heavymetal,CF)%>%
  pivot_wider(names_from=heavymetal,values_from=CF)%>%
  #summarize(PLI=(Cr*Fe*Ni*Cu*Zn*As*Pb*V*Mn*Ba)^(1/10))%>%
  summarize(PLI=(Cr*Fe*Ni*Cu*Zn*As*Pb)^(1/7))%>%
  ungroup()%>%
  group_by(Site,sg_m)%>%
  summarize(mPLI=mean(PLI),
            sdPLI=sd(PLI))

(plip<-ggplot(hm4%>%
                filter(sg_m=="Seagrass Bed"))+
    geom_hline(aes(yintercept=1),linetype="dashed")+
    #geom_hline(aes(yintercept=3),linetype="dashed")+
    geom_errorbar(aes(x=Site,ymin=mPLI-sdPLI,ymax=mPLI+sdPLI),width=.1)+
    geom_point(aes(x=Site,y=mPLI,color=Site),size=5)+
    #facet_grid(cols=vars(sg_m),scales="free")+
    scale_color_manual(values=hm2.colrs3$colr)+
    ylab("Pollution Load Index")+
    ylim(c(0,6))+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y=element_text(size=14),
          legend.position = "top",
          legend.title=element_blank()))
ggsave(plot=plip,"04_figures/seagrass_pollutionloadindex.jpg",width=7,height=5)

(plihp<-ggplot(hm4%>%
                filter(sg_m!="Seagrass Bed"))+
    geom_hline(aes(yintercept=1),linetype="dashed")+
    #geom_hline(aes(yintercept=3),linetype="dashed")+
    geom_errorbar(aes(x=Site,ymin=mPLI-sdPLI,ymax=mPLI+sdPLI),width=.1)+
    geom_point(aes(x=Site,y=mPLI,color=Site),size=5)+
    #facet_grid(cols=vars(sg_m),scales="free")+
    scale_color_manual(values=hm2.colors4$colr)+
    ylab("Pollution Load Index")+
    ylim(c(0,36))+
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y=element_text(size=14),
          legend.position = "top",
          legend.title=element_blank()))
ggsave(plot=plihp,"04_figures/habour_pollutionloadindex.jpg",width=7,height=5)

