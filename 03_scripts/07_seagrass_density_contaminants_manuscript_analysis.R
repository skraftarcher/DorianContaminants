# pair contaminant and seagrass data, conduct data analysis

# Stephanie K Archer 3/26/2023

# bring in packages and data----
source("03_scripts/install_packages_function.R")
source("03_scripts/glmmresids.R")
lp("tidyverse")
lp("lubridate")
lp("patchwork")
lp("glmmTMB")
lp("DHARMa")
lp("readxl")
lp("ggeffects")
lp("vegan")
#lp("BiodiversityR")

colrs<-read.csv("01_odata/colrs.csv")

#bring in data----
hm.sed.dec<-read.csv("01_odata/sediment_contam_sgb_dec.csv")%>%
  mutate(Site=ifelse(Site=="Jerrys","Jerry's",Site),
         Site=ifelse(Site=="BOR2","BOR",Site))

sed.standards<-read_xlsx("01_odata/sed_standards.xlsx")
hm.sg<-read.csv("01_odata/seagrass_dec_contam.csv")%>%
  mutate(Site=ifelse(Site=="Jerrys","Jerry's",Site),
         Site=ifelse(Site=="BOR2","BOR",Site))

sed.whirl<-read.csv("01_odata/sediment_whirlpack_dec2021.csv")%>%
  mutate(Plot=as.numeric(Plot),
         Site=ifelse(Site=="BOR 2","BOR2",Site))%>%
  select(WhirlpakID,Site,Plot)%>%
  mutate(Site=ifelse(Site=="Jerrys","Jerry's",Site),
         Site=ifelse(Site=="BOR2","BOR",Site))

sg.whirl<-read.csv("01_odata/seagrass_whirlpacks.csv")%>%
  mutate(Plot=as.numeric(plot),
         Site=ifelse(Site=="BOR 2","BOR2",Site))%>%
  select(WhirlpakID=BagID,Site,Plot)%>%
  mutate(Site=ifelse(Site=="Jerrys","Jerry's",Site),
         Site=ifelse(Site=="BOR2","BOR",Site))

pub.sg<-read_xlsx("01_odata/sanchezetal_database.xlsx",sheet="Thalassia")

sg.pre<-read.csv("02_wdata/USE_allyrs_shootdensity.csv")%>%
  mutate(prepost=ifelse(yr<2019,"pre","post"),
         site=case_when(
            site=="BOR"~"BOR",
            site=="BOR1"~"BOR",
            site=="BOR2"~"BOR",
            site=="BOR3"~"BOR",
            site=="BOR4"~"BOR",
            site=="Jerrys"~"Jerry's",
            TRUE~site))%>%
  group_by(site,prepost,yr,mnth)%>%
  summarize(shd=mean(shd,na.rm=TRUE))%>%
  ungroup()%>%
  group_by(site,prepost)%>%
  mutate(ym=ym(paste(yr,mnth)),
         minym=ifelse(min(ym)==ym,"y","n"),
         maxym=ifelse(max(ym)==ym,"y","n"),
         kp=case_when(
           prepost=="pre" & maxym=="y"~1,
           prepost=="post" & minym=="y"~1))

jb<-sg.pre%>%
  filter(kp==1)%>%
  filter(site=="BOR")%>%
  filter(prepost=="pre")%>%
  mutate(site="Jerry's")

sg.pre2<-rbind(sg.pre,jb)%>%
  filter(kp==1)%>%
  ungroup()%>%
  select(Site=site,prepost,shd)%>%
  filter(Site %in% unique(hm.sg$Site))%>%
  pivot_wider(names_from = prepost,values_from=shd)%>%
  mutate(shd.resist=(post-pre)/pre)%>%
  select(Site,pre,shd.resist)

sg.post<-sg.pre%>%
  filter(prepost=="post")%>%
  filter(site %in% unique(hm.sg$Site))%>%
  mutate(minym=ifelse(min(ym)==ym,"y","n"),
         maxym=ifelse(max(ym)==ym,"y","n"),
         kp=case_when(maxym=="y"~1,
                      minym=="y"~1),
         tp=case_when(maxym=="y"~"end",
                      minym=="y"~"begin"))%>%
  filter(kp==1)%>%
  ungroup()%>%
  select(Site=site,tp,shd)%>%
  pivot_wider(names_from=tp,values_from = shd)%>%
  mutate(shd.recover=(end-begin)/begin/24)%>%
  select(Site,shd.recover)


site.char<-read_xlsx("01_odata/site_char.xlsx")%>%
  left_join(sg.pre2)%>%
  left_join(sg.post)

# water----
water.dec<-read.csv("01_odata/water_sgb_dec_contam.csv")

# bring in seagrass density data----
sg<-read.csv("01_odata/December2021_seagrass.csv")%>%
  mutate(Site=ifelse(Site=="Jerrys","Jerry's",Site),
         Site=ifelse(Site=="BOR2","BOR",Site),
         Site=ifelse(Site=="Hills Cr","Hills Creek",Site))

# organize contaminant info to get rid of # signs and triplicates----
hm.sed2<-hm.sed.dec%>%
  separate(Sample.ID,into=c("ID","WhirlpakID"),sep="#")%>%
  mutate(WhirlpakID=ifelse(is.na(WhirlpakID),ID,WhirlpakID))%>%
  separate(WhirlpakID,into=c("WhirlpakID","rep"),sep=3)%>%
  select(-ID)%>%
  pivot_longer(Pb:V,names_to = "hm",values_to="sed.conc")%>%
  group_by(Site,WhirlpakID,hm)%>%
  summarize(sed.conc.m=mean(sed.conc),
            sed.conc.sd=sd(sed.conc))

hm.sg2<-hm.sg%>%
  separate(Sample.ID,into=c("ID","WhirlpakID"),sep="#")%>%
  mutate(WhirlpakID=ifelse(is.na(WhirlpakID),ID,WhirlpakID))%>%
  separate(WhirlpakID,into=c("WhirlpakID","rep"),sep=3,convert=TRUE)%>%
  select(-ID)%>%
  pivot_longer(Pb:V,names_to = "hm",values_to="sg.conc")%>%
  group_by(Site,WhirlpakID,hm)%>%
  summarize(sg.conc.m=mean(sg.conc),
            sg.conc.sd=sd(sg.conc))


# now join whirlpak IDs to contaminant data----
hm.sed3<-left_join(hm.sed2,sed.whirl)%>%
  rename(sed.whirl=WhirlpakID)

hm.sg3<-left_join(hm.sg2,sg.whirl)%>%
  rename(sg.whirl=WhirlpakID)

# get canopy data per plot ----
sg.can<-sg%>%
  select(Date,Site,Plot,Quadrat,Canopy)%>%
  distinct()%>%
  filter(!is.na(Canopy))

sg2<-sg%>%
  mutate(dens2=Density/(density.size/(100*100)))%>%
  select(Date,Site,Plot,Quadrat,Taxa, dens2)%>%
  filter(Taxa %in% c("T","H","S"))%>%
  pivot_wider(names_from=Taxa,values_from = dens2,values_fill=0)%>%
  mutate(total.dens=`T`+H+S)%>%
  pivot_longer('T':total.dens,names_to="Taxa",values_to = "dens2")%>%
  left_join(sg.can)%>%
  group_by(Date,Site,Plot,Taxa)%>%
  summarize(dens3=mean(dens2,na.rm = TRUE),
            dens.sd=sd(dens2,na.rm=TRUE),
            Canopy=mean(Canopy,na.rm=TRUE))%>%
  pivot_wider(names_from=Taxa,values_from = c(dens3,dens.sd),values_fill=0)%>%
  filter(!is.na(T))%>%
  mutate(total.dens=dens3_T+dens3_H+dens3_S)%>%
  rename(mT=dens3_T,mH=dens3_H,mS=dens3_S,sdT=dens.sd_T,
         sdH=dens.sd_H,sdS=dens.sd_S,mtotal=dens3_total.dens,sdtotal=dens.sd_total.dens)
  

# now join dataset together
sg3<-left_join(sg2,hm.sed3)%>%
  left_join(hm.sg3)%>%
  filter(!is.na(sed.conc.m))%>%
  filter(!is.na(sg.conc.m))%>%
  left_join(site.char)

# visualize sediment vs seagrass----

# Thalassia is the only seagrass we tested----
theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           panel.background = element_rect(fill="#FAFAFA"),
                           axis.title=element_text(size=16),
                           axis.text = element_text(size=14),
                           legend.text=element_text(size=14),
                           legend.title=element_text(size=16)))
ggplot(data=sg3%>%
         filter(hm %in% c("As","Cr","Pb")))+
  geom_point(aes(x=log(sed.conc.m),y=log(sg.conc.m),color=Site),size=2,alpha=.5)+
  facet_grid(Site~hm,scales="free_x")

ggplot(data=sg3%>%
         filter(!hm %in% c("As","Cr","Pb")))+
  geom_point(aes(x=log(sed.conc.m),y=log(sg.conc.m),color=Site),size=2,alpha=.5)+
  facet_grid(Site~hm,scales="free_x")


# compare sites ----

sg3$Site<-factor(sg3$Site,levels = c("Treasure Cay","Hills Creek","Camp Abaco","Snake Cay","BOR","Jerry's"))
hm.sed2$Site<-factor(hm.sed2$Site,levels = c("Treasure Cay","Hills Creek","Camp Abaco","Snake Cay","BOR","Jerry's"))

ggplot(data=hm.sed2)+
  geom_boxplot(aes(y=sed.conc.m,fill=Site))+
  facet_wrap(~hm,scales="free")


# outliers to check 
# over 150 chromium
# over 300 lead


ggplot(data=sg3%>%filter(sg.conc.m<150))+
  geom_boxplot(aes(y=sg.conc.m,fill=Site))+
  facet_wrap(~hm,scales="free")

# looks like there may be some data points to check
# the 15 mg/kg seagrass arsenic at Jerry's
# the 20 and 40 mg/kg copper in seagrass in the bight
# the 200 mg /g nickle in seagrass at camp abaco

# look at if water differs in predictable ways with storm, debris, or 
# dump
#water viz----

water.dec<-left_join(water.dec,site.char)
water.dec$Site<-factor(water.dec$Site,levels = c("Treasure Cay","Hills Creek","Camp Abaco","Snake Cay","BOR","Jerry's"))

water.cols<-colrs%>%
  filter(Site %in% unique(water.dec$Site))

water.cols$Site<-factor(water.cols$Site,levels = c("Treasure Cay","Hills Creek","Camp Abaco","Snake Cay","BOR","Jerry's"))
water.cols<-arrange(water.cols,Site)

ggplot(data=water.dec%>%
         pivot_longer(Cr:V,names_to = "hm",values_to = "conc"))+
  geom_boxplot(aes(y=conc,fill=Site))+
  facet_wrap(~hm,scales="free")+
  scale_color_manual(values=water.cols$sg.color)+
  scale_fill_manual(values=water.cols$sg.color)

ggplot(data=water.dec%>%
         pivot_longer(Cr:V,names_to = "hm",values_to = "conc"))+
  geom_boxplot(aes(y=conc,fill=as.factor(debris.score)))+
  facet_wrap(~hm,scales="free")

ggplot(data=water.dec%>%
         pivot_longer(Cr:V,names_to = "hm",values_to = "conc"))+
  geom_point(aes(y=conc,x=dist.dump,color=Site))+
  facet_wrap(~hm,scales="free")

# multivariate look ----
water.mds<-metaMDS(water.dec[,c(-1:-2,-10:-16)])

water.mds2<-cbind(water.dec[,1:2],data.frame(water.mds$points))
(wmds<-ggplot(water.mds2)+
  #geom_point(aes(x=MDS1,y=MDS2),size=5)+
  geom_point(aes(x=MDS1,y=MDS2,color=Site),size=6)+
  #geom_polygon(aes(x=MDS1,y=MDS2,fill=Site),alpha=.4)+
  scale_color_manual(values=water.cols$sg.color)+
  scale_fill_manual(values=water.cols$sg.color)+
  theme(legend.position = "none"))

# seagrass multivariate----

sg.mdsd<-sg3%>%
  ungroup()%>%
  select(Site,Plot,mtotal,total.dens,mT,hm,sg.conc.m)%>%
  distinct()%>%
  group_by(Site,Plot,mtotal,total.dens,mT,hm)%>%
  summarize(sg.conc.m=mean(sg.conc.m))%>%
  pivot_wider(names_from=hm,values_from = sg.conc.m)

sg.mds<-metaMDS(sg.mdsd[,-1:-5])
sg.mds<-cbind(sg.mdsd[,1:5],data.frame(sg.mds$points))
sg.mds$Site<-factor(sg.mds$Site,levels = c("Treasure Cay","Hills Creek","Camp Abaco","Snake Cay","BOR","Jerry's"))

sg.cols<-colrs%>%
  filter(Site %in% unique(hm.sg$Site))

sg.cols$Site<-factor(sg.cols$Site,levels = c("Treasure Cay","Hills Creek","Camp Abaco","Snake Cay","BOR","Jerry's"))
sg.cols<-arrange(sg.cols,Site)

(sgmds<-ggplot(sg.mds)+
 # stat_ellipse(aes(x=MDS1,y=MDS2,color=Site,fill=Site),geom="polygon",alpha=.1)+
  geom_point(aes(x=MDS1,y=MDS2,color=Site),size=6)+
  scale_color_manual(values=sg.cols$sg.color)+
  scale_fill_manual(values=sg.cols$sg.color))
  

# sediment hm multivariate ----

sed.mdsd<-sg3%>%
  ungroup()%>%
  select(Site,Plot,mtotal,total.dens,mT,hm,sed.conc.m)%>%
  distinct()%>%
  group_by(Site,Plot,mtotal,total.dens,mT,hm)%>%
  summarize(sed.conc.m=mean(sed.conc.m))%>%
  pivot_wider(names_from=hm,values_from = sed.conc.m)

sed.mds<-metaMDS(sed.mdsd[,-1:-5])
sed.mds<-cbind(sed.mdsd[,1:5],data.frame(sed.mds$points))
sed.mds$Site<-factor(sed.mds$Site,levels = c("Treasure Cay","Hills Creek","Camp Abaco","Snake Cay","BOR","Jerry's"))

(sedmds<-ggplot(sed.mds)+
#   stat_ellipse(aes(x=MDS1,y=MDS2,fill=Site),geom="polygon",alpha=.3)+
    geom_point(aes(x=MDS1,y=MDS2,color=Site),size=6)+
    scale_color_manual(values=sg.cols$sg.color)+
    scale_fill_manual(values=sg.cols$sg.color))

# multivariate plot----
(wmds+sedmds)/sgmds+plot_layout(guides="collect")


# compare published hm values to ours

our.hm<-hm.sg3%>%
  ungroup()%>%select(Site,hm,m.conc=sg.conc.m)%>%
  mutate(study="abaco",
         Reference="abaco")

pub.sg2<-pub.sg%>%
  filter(Metal %in% unique(our.hm$hm))%>%
  select(Reference,hm=Metal,m.conc)%>%
  mutate(study="published",
         Site="Published",
         Reference=as.character(Reference))

comp.hm<-bind_rows(our.hm,pub.sg2)
comp.hm$Site<-factor(comp.hm$Site,levels=c("Published","Treasure Cay",
                                             "Hills Creek","Camp Abaco",
                                             "Snake Cay","BOR","Jerry's"))

pcols<-c("darkgrey",sg.cols$sg.color)
ggplot(data=comp.hm%>%
         filter(hm %in% c("As","Cr","Ni"))%>%
         filter(m.conc<150),aes(y=m.conc,fill=study))+
  #geom_density(alpha=.4)+
  geom_boxplot()+
  facet_wrap(~hm,scales="free")+
  #geom_vline(aes(xintercept=-.27),linetype="dashed")+
  scale_fill_manual(values=pcols)+
  ylab("mg/kg")+
  theme(axis.text.x = element_blank(),
        axis.title.x=element_blank())

ggsave("04_figures/seagrassvpublished.jpg")

# look at relation of hm in sediment and seagrass with shoot densities----
# look at relationship between seagrass concentrations and shoot density----

ggplot(data=sg3,aes(y=log(sed.conc.m),x=log(mT)))+
  geom_point(aes(color=Site,size=sg.conc.m),alpha=.4)+
  geom_smooth(method="lm")+
  facet_wrap(~hm,scales="free")+
  scale_color_manual(values=sg.cols$sg.color)

# arsenic
par(mfrow=c(2,2))

cor(sg3$total.dens,sg3$shd.resist)
cor(sg3$total.dens,sg3$pre)
cor(sg3$total.dens,sg3$debris.score)
cor(sg3$shd.resist,sg3$pre)
cor(sg3$shd.resist,sg3$debris.score)
cor(sg3$debris.score,sg3$pre)

tdlab<-expression(paste(italic("T. testudinum")," density (shoots m"^"-2",")"))
tdlabpre<-expression(paste("Pre-Storm Seagrass density (shoots m"^"-2",")"))
sg.leg<-expression(atop("Post-Storm seagrass","  (shoots m"^-2*")"))
td.leg<-expression(atop("Post-Storm "*italic("T. testudinum"),"  (shoots m"^-2*")"))
tdpre.leg<-expression(atop("Pre-Storm "*italic("T. testudinum"),"  (shoots m"^-2*")"))

sed.as<-lm(sed.conc.m~mT+mH+mS+pre,
           data=sg3%>%
             filter(hm=="As"))


plot(sed.as)

summary(sed.as)

sed.as.p<-ggpredict(sed.as,terms = c("mT"))
sed.as.p2<-ggpredict(sed.as,terms = c("pre"))
sed.as.p3<-ggpredict(sed.as,terms = c("mH"))
plot(sed.as.p2)
plot(sed.as.p3)

(asp<-ggplot(sed.as.p)+
    geom_point(data=sg3%>%filter(hm=="As"),alpha=.8,
               aes(x=mT,y=sed.conc.m,color=Site,size=pre))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=200,y=95),size=8,label="p < 0.001")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Arsenic (mg/kg)")+
    xlab(tdlab)+
    scale_size_continuous(name=td.leg))

ggsave(plot=asp,"04_figures/sediment_arsenic_T.jpg")

(asp3<-ggplot(sed.as.p3)+
    geom_point(data=sg3%>%filter(hm=="As"),alpha=.8,
               aes(x=mH,y=sed.conc.m,color=Site,size=pre))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=200,y=95),size=8,label="p < 0.001")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Arsenic (mg/kg)")+
    xlab(tdlab)+
    scale_size_continuous(name=tdpre.leg))

ggsave(plot=asp3,"04_figures/sediment_arsenic_H.jpg")

(asprep<-ggplot(sed.as.p2)+
    geom_jitter(data=sg3%>%filter(hm=="As"),alpha=.8,
               aes(x=pre,y=sed.conc.m,color=Site,size=(mT+mH)))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=300,y=95),size=8,label="p < 0.001")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Arsenic (mg/kg)")+
    xlab(tdlabpre)+
    scale_size_continuous(name=sg.leg))

ggsave(plot=asprep,"04_figures/sediment_arsenic_pre.jpg")

# significant positive relationship with total seagrass density and 
# arsenic concentrations in the sediment

# copper

sed.Cu<-lm(sed.conc.m~mT+mH+mS+pre,
           data=sg3%>%
             filter(hm=="Cu"))
plot(sed.Cu)

summary(sed.Cu)

sed.cu.p<-ggpredict(sed.Cu,terms = c("mS"))
plot(sed.cu.p)

(cup<-ggplot(sed.cu.p)+
    geom_point(data=sg3%>%filter(hm=="As"),alpha=.8,
               aes(x=mS,y=sed.conc.m,color=Site,size=pre))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=200,y=95),size=8,label="p = 0.009")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Copper (mg/kg)")+
    xlab(tdlab)+
    scale_size_continuous(name=tdpre.leg))

# copper is related to syringodium density


# chromium

sed.Cr<-lm(sed.conc.m~mT+mH+mS+pre,
           data=sg3%>%
             filter(hm=="Cr")%>%
             filter(sed.conc.m<140))
plot(sed.Cr)

summary(sed.Cr)

sed.Cr.p<-ggpredict(sed.Cr,terms = c("pre"))

plot(sed.Cr.p)


(crp<-ggplot(sed.Cr.p)+
    geom_jitter(data=sg3%>%filter(hm=="Cr")%>%
                 filter(sed.conc.m<140),
               alpha=.8,aes(x=pre,y=sed.conc.m,color=Site,size=(mT+mH)))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=300,y=120),size=8,label="p = 0.01")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Chromium (mg/kg)")+
    xlab(tdlabpre)+
    scale_size_continuous(name=sg.leg))

ggsave(plot=crp,"04_figures/sediment_chromium_pre.jpg")

#Nickle

sed.Ni<-lm(sed.conc.m~mT+mH+mS+pre,
           data=sg3%>%
             filter(hm=="Ni"))
plot(sed.Ni)

summary(sed.Ni)

sed.Ni.p<-ggpredict(sed.Ni,terms = c("mT"))
plot(sed.Ni.p)
sed.Ni.p2<-ggpredict(sed.Ni,terms = c("pre"))
plot(sed.Ni.p2)

(nip<-ggplot(sed.Ni.p)+
    geom_point(data=sg3%>%filter(hm=="Ni"),
               alpha=.8,aes(x=mT,y=sed.conc.m,color=Site,size=pre))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=200,y=50),size=8,label="p = 0.002")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Nickel (mg/kg)")+
    xlab(tdlab)+
    scale_size_continuous(name=tdpre.leg))

ggsave(plot=nip,"04_figures/sediment_nickel_T.jpg")

(niprep<-ggplot(sed.Ni.p2)+
    geom_jitter(data=sg3%>%filter(hm=="Ni"),
               alpha=.8,aes(x=pre,y=sed.conc.m,color=Site,size=mT))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=300,y=50),size=8,label="p = 0.01")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Nickel (mg/kg)")+
    xlab(tdlabpre)+
    scale_size_continuous(name=td.leg))

ggsave(plot=niprep,"04_figures/sediment_nickel_pre.jpg")

#lead

sed.Pb<-lm(sed.conc.m~mT+mH+mS+pre,
           data=sg3%>%
             filter(hm=="Pb")%>%
             filter(sed.conc.m<290))
plot(sed.Pb)

summary(sed.Pb)


#V


sed.V<-lm(sed.conc.m~mT+mH+mS+pre,
           data=sg3%>%
             filter(hm=="V"))
plot(sed.V)

summary(sed.V)


sed.V.p<-ggpredict(sed.V,terms = c("mT"))
plot(sed.V.p)
sed.V.p2<-ggpredict(sed.V,terms = c("pre"))
plot(sed.V.p2)

(Vp<-ggplot(sed.V.p)+
    geom_point(data=sg3%>%filter(hm=="V"),
               alpha=.8,aes(x=mT,y=sed.conc.m,color=Site,size=pre))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=200,y=95),size=8,label="p = 0.004")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Vanadium (mg/kg)")+
    xlab(tdlab)+
    scale_size_continuous(name=tdpre.leg))

ggsave(plot=Vp,"04_figures/sediment_vanadium_T.jpg")

(Vprep<-ggplot(sed.V.p2)+
    geom_jitter(data=sg3%>%filter(hm=="V"),
                alpha=.8,aes(x=pre,y=sed.conc.m,color=Site,size=(mT+mH)))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=300,y=95),size=8,label="p < 0.001")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Vanadium (mg/kg)")+
    xlab(tdlabpre)+
    scale_size_continuous(name=sg.leg))


ggsave(plot=Vprep,"04_figures/sediment_vanadium_pre.jpg")

#Zn


sed.Zn<-lm(sed.conc.m~mT+mH+mS+pre,
           data=sg3%>%
             filter(hm=="Zn"))
plot(sed.Zn)

summary(sed.Zn)


# composite metal pollution score and seagrass----

sg4<-sg3%>%
  left_join(sed.standards)%>%
  mutate(CF=sed.conc.m/standard.conc,
         Igeo=log(sed.conc.m/(1.5*standard.conc)))

sg.pli<-sg4%>%
  select(Site,Plot,total.dens,mT,mH,mS,pre,shd.resist,CF,hm)%>%
  distinct()%>%
  pivot_wider(names_from=hm,values_from=CF)%>%
  mutate(PLI=(As*Cr*Cu*Ni*Pb*V*Zn)^(1/7))


  
  
pli.lm<-lm(PLI~mT+mH+mS+pre,data=sg.pli%>%filter(PLI<1.8))


plot(pli.lm)



summary(pli.lm)


pli.p<-ggpredict(pli.lm,terms="mH")
pli.p2<-ggpredict(pli.lm,terms="pre")

(plip<-ggplot(pli.p)+
    geom_hline(aes(yintercept=1),linetype="dashed",color="lightgrey")+
    geom_point(data=sg.pli%>%filter(PLI<1.8),
               alpha=.8,aes(x=mH,y=PLI,color=Site,size=pre))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=200,y=1.3),size=8,label="p = 0.02")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Pollution Load Index")+
    xlab(tdlab)+
    scale_size_continuous(name="Pre-storm \nseagrass density"))

ggsave(plot=plip,"04_figures/sediment_PLI.jpg")

(pliprep<-ggplot(pli.p2)+
    geom_hline(aes(yintercept=1),linetype="dashed",color="lightgrey")+
    geom_jitter(data=sg.pli%>%filter(PLI<1.8),
               alpha=.8,aes(x=pre,y=PLI,color=Site,size=(mH+mT)))+
    geom_ribbon(aes(x=x,ymin=conf.low,ymax=conf.high),color="lightgrey",alpha=.3)+
    geom_line(aes(x=x,y=predicted),size=1.2)+
    geom_text(aes(x=300,y=1.3),size=8,label="p = 0.006")+
    scale_color_manual(values=sg.cols$sg.color)+
    ylab("Sediment Pollution Load Index")+
    xlab(tdlabpre)+
    scale_size_continuous(name=sg.leg))


ggsave(plot=pliprep,"04_figures/sediment_PLI_pre.jpg")

#  join water and see if seagrass tissue relates to water, 
# sediment, or neither

water2<-water.dec%>%
  pivot_longer(Cr:V,names_to="hm",values_to = "water.conc")%>%
  select(-Sample.ID)%>%
  group_by(Site,hm)%>%
  summarize(water.conc.m=mean(water.conc))

plij<-sg.pli%>%
  select(Site,Plot,PLI)

sg5<-left_join(sg3,water2)%>%left_join(plij)

# models to look at seagrass contaminants in arsenic, chromium, nickel,
# and vanadium
x<-sg5%>%
  filter(sed.conc.m<90)%>%
  filter(sg.conc.m<15)%>%
  filter(hm=="As")%>%
  filter(!is.na(water.conc.m))

cor(x$water.conc.m,x$sed.conc.m)

as.sgb<-lm(sg.conc.m~water.conc.m+sed.conc.m,data=x)
as.sgs<-lm(sg.conc.m~sed.conc.m,data=x)
as.sgw<-lm(sg.conc.m~water.conc.m,data=x)

plot(as.sgs)
plot(as.sgw)
anova(as.sgs,as.sgw,as.sgb)
summary(as.sgs)
summary(as.sgw)

asr<-expression(paste("R"^2,"= 0.09"))

ggplot(data=x)+
  geom_point(alpha=.5,size=5,
             aes(x=sed.conc.m,color=Site,y=sg.conc.m))+
  geom_smooth(method="lm",
              aes(x=sed.conc.m,y=sg.conc.m),color="black")+
  scale_color_manual(values=sg.cols$sg.color)+
  geom_text(aes(x=22,y=7.5),label=asr,size=8)+
  geom_text(aes(x=22,y=8),label="Arsenic",size=8)+
  ylab("Seagrass concentration (mg/kg)")+
  xlab("Sediment concentration (mg/kg)")

ggsave("04_figures/seagrass_sed_as.jpg")

#chromium

ggplot(data=sg5%>%
         filter(sed.conc.m<140)%>%
         #filter(sg.conc.m<15)%>%
         filter(hm=="Cr")%>%
         filter(!is.na(water.conc.m)))+
  geom_point(alpha=.5,
             aes(x=sed.conc.m,color=Site,y=sg.conc.m,size=water.conc.m))+
  geom_smooth(method="lm",
              aes(x=sed.conc.m,y=sg.conc.m),color="black")+
  scale_color_manual(values=sg.cols$sg.color)

x<-sg5%>%
  filter(sed.conc.m<140)%>%
  #filter(sg.conc.m<15)%>%
  filter(hm=="Cr")%>%
  filter(!is.na(water.conc.m))

cor(x$water.conc.m,x$sed.conc.m)

cr.sgb<-lm(sg.conc.m~water.conc.m*sed.conc.m,data=x)
cr.sgs<-lm(sg.conc.m~sed.conc.m,data=x)
cr.sgw<-lm(sg.conc.m~water.conc.m,data=x)

plot(cr.sgs)
plot(cr.sgw)
anova(cr.sgs,cr.sgw,cr.sgb)
summary(cr.sgs)
summary(cr.sgw)
summary(cr.sgb)
# no relationship between chromium in seagrass tissue and sediment or water 

#Nickel

ggplot(data=sg5%>%
         #filter(sed.conc.m<140)%>%
         filter(sg.conc.m<50)%>%
         filter(hm=="Ni")%>%
         filter(!is.na(water.conc.m)))+
  geom_point(alpha=.5,
             aes(x=sed.conc.m,color=Site,y=sg.conc.m,size=water.conc.m))+
  geom_smooth(method="lm",
              aes(x=sed.conc.m,y=sg.conc.m),color="black")+
  scale_color_manual(values=sg.cols$sg.color)

x<-sg5%>%
  filter(sed.conc.m<30)%>%
  #filter(sg.conc.m<15)%>%
  filter(hm=="Ni")%>%
  filter(!is.na(water.conc.m))

cor(x$water.conc.m,x$sed.conc.m)

ni.sgb<-lm(sg.conc.m~water.conc.m*sed.conc.m,data=x)
ni.sgs<-lm(sg.conc.m~sed.conc.m,data=x)
ni.sgw<-lm(sg.conc.m~water.conc.m,data=x)

plot(ni.sgs)
plot(ni.sgw)
anova(ni.sgs,ni.sgw,ni.sgb)
summary(ni.sgs)
summary(ni.sgw)
summary(ni.sgb)

# interaction is significant, but not sure I understand it

#vanadium

ggplot(data=sg5%>%
         #filter(sed.conc.m<140)%>%
         #filter(sg.conc.m<15)%>%
         filter(hm=="V")%>%
         filter(!is.na(water.conc.m)))+
  geom_point(alpha=.5,
             aes(x=sed.conc.m,color=Site,y=sg.conc.m,size=water.conc.m))+
  geom_smooth(method="lm",
              aes(x=sed.conc.m,y=sg.conc.m),color="black")+
  scale_color_manual(values=sg.cols$sg.color)

x<-sg5%>%
#  filter(sed.conc.m<140)%>%
  #filter(sg.conc.m<15)%>%
  filter(hm=="V")%>%
  filter(!is.na(water.conc.m))

cor(x$water.conc.m,x$sed.conc.m)

v.sgb<-lm(sg.conc.m~water.conc.m*sed.conc.m,data=x)
v.sgs<-lm(sg.conc.m~sed.conc.m,data=x)
v.sgw<-lm(sg.conc.m~water.conc.m,data=x)

plot(v.sgs)
plot(v.sgw)
anova(v.sgs,v.sgw,v.sgb)
summary(v.sgs)
summary(v.sgw)
summary(v.sgb)

# no relationship

