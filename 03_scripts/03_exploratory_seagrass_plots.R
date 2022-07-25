# pre-analysis data visualization for seagrass

# Stephanie K. Archer 01/08/2021

# load packaeges
source("03_scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("vegan")
lp("viridis")
lp("patchwork")
colrs<-read.csv("01_odata/colrs.csv")
theme_set(theme_bw())

#For DEPP report (for paper go to line 139)

# bb data
# load data
bb<-read.csv("02_wdata/USE_allyrs_seagrass_bb.csv")


bb2<-bb%>%
  filter(yr==2021)%>%
  mutate(date=ifelse(mnth==6,"June","December"))%>%
  group_by(site,yr,date,taxa)%>%
  summarize(Abundance=mean(abund),se=sd(abund)/(sqrt(n()+1)))%>%
  filter(site %in% unique(colrs$site))%>%
  left_join(colrs)%>%
  filter(!site %in% c("Hope Town","Man o War"))%>%
  mutate(Abundance=ifelse(is.na(Abundance),0,Abundance),
         se=ifelse(is.na(se),0,se))



colrs2<-colrs%>%
  filter(site %in% unique(bb2$site))

bb2$date<-factor(bb2$date,levels=c("June","December"))
bb2$site<-factor(bb2$site,levels=colrs2$site)
bb2$taxa<-factor(bb2$taxa,labels=c("Halodule wrightii","Syringodium filiforme","Thalassia testudinum"))

(bbplot<-ggplot(bb2)+  
  geom_errorbar(aes(x=as.factor(date),ymin=Abundance-se,ymax=Abundance+se,color=site),width=.2)+
  #geom_vline(xintercept=my("09-2019"),size=2,alpha=.5,linetype="dashed")+
  geom_line(aes(x=as.factor(date),y=Abundance,group=site,color=site),size=1.5,alpha=.5)+
  geom_point(aes(x=as.factor(date),y=Abundance,color=site,group=site),size=2)+
  scale_color_manual(name="Site",labels=colrs2$site.name,values=colrs2$sg.color)+
  theme(panel.grid = element_blank(),
        strip.text = element_text(face="italic"))+
  ylab("Abundance")+
  xlab("2021 Sampling")+
  facet_grid(rows=vars(taxa),scales="free"))


ggsave(plot=bbplot,"04_figures/SG_bb_Dec2021.png",width=7,height=7)


# time since dorian
bb3<-bb%>%
  filter(yr>2018)%>%
  mutate(date=ym(paste(yr,mnth)))%>%
  group_by(site,yr,date,taxa)%>%
  summarize(Abundance=mean(abund),se=sd(abund)/(sqrt(n()+1)))%>%
  filter(site %in% unique(colrs$site))%>%
  left_join(colrs)%>%
  filter(!site %in% c("Hope Town","Man o War"))%>%
  mutate(Abundance=ifelse(is.na(Abundance),0,Abundance),
         se=ifelse(is.na(se),0,se))



colrs2<-colrs%>%
  filter(site %in% unique(bb3$site))

bb3$site<-factor(bb3$site,levels=colrs2$site)
bb3$taxa<-factor(bb3$taxa,labels=c("Halodule wrightii","Syringodium filiforme","Thalassia testudinum"))

(bbplot<-ggplot(bb3)+  
    geom_errorbar(aes(x=as.factor(date),ymin=Abundance-se,ymax=Abundance+se,color=site),width=.2)+
    #geom_vline(xintercept=my("09-2019"),size=2,alpha=.5,linetype="dashed")+
    geom_line(aes(x=as.factor(date),y=Abundance,group=site,color=site),size=1.25,alpha=.5)+
    geom_point(aes(x=as.factor(date),y=Abundance,color=site,group=site),size=2)+
    scale_color_manual(name="Site",labels=colrs2$site.name,values=colrs2$sg.color)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          strip.text = element_text(face="italic"))+
    scale_x_discrete(name = "Sampling",labels=c("+2 Months","+8 Months","+21 Months","+27 Months"))+
    ylab("Abundance")+
    facet_grid(cols=vars(taxa),scales="free"))


ggsave(plot=bbplot,"04_figures/SG_bb_allpost.png",width=12,height=4)

# shoot density data
# load data
sd<-read.csv("02_wdata/USE_allyrs_shootdensity.csv")

sd2<-sd%>%
  mutate(date=my(paste(mnth,yr)))%>%
  group_by(site,yr,date)%>%
  summarize(ShootDensity=mean(shd),se=sd(shd)/(sqrt(n()+1)))%>%
  filter(site %in% unique(colrs$site))%>%
  left_join(colrs)%>%
  filter(!site %in% c("Hope Town","Man o War"))%>%
  filter(yr==2021)
 

colrs3<-colrs%>%
  filter(site %in% unique(sd2$site))

sd2$site<-factor(sd2$site,levels=colrs3$site)
tlab<-expression(paste(italic("Thalassia testudinum"),"shoots m"^"-2"))
(sdplot<-ggplot(sd2)+
  #geom_vline(xintercept=my("09-2019"),size=2,alpha=.5,linetype="dashed")+
  geom_line(aes(x=as.factor(date),y=ShootDensity,group=site,color=site),size=1.5,alpha=.5)+
  geom_point(aes(x=as.factor(date),y=ShootDensity,color=site),size=4)+
  geom_errorbar(aes(x=as.factor(date),ymin=ShootDensity-se,ymax=ShootDensity+se,color=site),width=.3)+
  scale_color_manual(name="Site",labels=colrs3$site.name,values=colrs3$sg.color)+
  theme(panel.grid = element_blank())+
  ylab(tlab)+
  scale_x_discrete(name="2021 Sampling",labels=c("June","December")))

ggsave(plot=sdplot,"04_figures/SG_shoots_Dec2021.png",width=7,height=5)

# seagrass community
bba<-read.csv("02_wdata/USE_allyrs_algae_bb.csv")
bb3<-bind_rows(bb,bba)%>%
  #filter(yr==2021)%>%
  filter(!site %in% c("Hope Town","Man o War"))%>%
  distinct()%>%
  mutate(abund=ifelse(is.na(abund),0,abund))%>%
  pivot_wider(names_from=taxa,values_from=abund,values_fill=0)

bb.env<-bb3[,1:7]%>%
  mutate(samp=case_when(
    yr<2019 ~"pre",
    yr>2018 & mnth==11~"+ 2",
    yr>2018 & mnth==5~"+ 8",
    yr>2018 & mnth==6~"+ 21",
    yr>2018 & mnth==12~"+ 27"))
bb.com<-bb3[,-1:-7]

colrs4<-colrs%>%
  filter(site %in% unique(bb.env$site))
bb.hel<-decostand(bb.com,method="hellinger")

bb.rda<-rda(bb.hel)
plot(bb.rda)

bb.rda.scores<-data.frame(bb.env,scores(bb.rda,c(1,2),display="sites"))%>%
  left_join(colrs)

bb.rda.scores$site<-factor(bb.rda.scores$site,levels=colrs4$site)
bb.rda2<-bb.rda.scores%>%filter(samp=="+ 2")
bb.rda27<-bb.rda.scores%>%filter(samp=="+ 27")
colrs5<-colrs4 %>%
  filter(site %in% unique(bb.rda2$site))
colrs6<-colrs4 %>%
  filter(site %in% unique(bb.rda27$site))

(pprda2<-ggplot(bb.rda2,aes(x=PC1,y=PC2,color=site))+
  geom_jitter(size=4,alpha=.5)+
  scale_color_manual(name="Site",labels=colrs5$site.name,values=colrs5$sg.color)+
  theme(panel.grid = element_blank()))

(pprda27<-ggplot(bb.rda27,aes(x=PC1,y=PC2,color=site))+
    geom_jitter(size=4,alpha=.5)+
    scale_color_manual(name="Site",labels=colrs6$site.name,values=colrs6$sg.color)+
    theme(panel.grid = element_blank(),
          legend.position = "none"))
  #scale_shape_manual(name="Sampling",labels=c("June 2021","December 2021"),values=c(16,22)))

pprda2 + pprda27 + plot_layout(guides = 'collect')

ggsave("04_figures/primary_producers_community.png",width=9,height=5)


# make datasets to attach as appendices

bb4<-bind_rows(bb,bba)%>%
  filter(yr==2021)%>%
  mutate(date=ifelse(mnth==6,"June","December"))%>%
  group_by(site,yr,date,taxa)%>%
  filter(site %in% unique(colrs$site))%>%
  select(Site=site,Month=date,Year=yr,Plot=plot,Quadrat=quadrat,Taxa=taxa,`Abundance Score`=abund)%>%
  arrange(Site,Month,Year,Plot,Quadrat,Taxa)

write.csv(bb4,"02_wdata/bbdata_deppreport.csv")

sd3<-sd%>%
  filter(yr==2021)%>%
  mutate(date=ifelse(mnth==6,"June","December"))%>%
  filter(site %in% unique(colrs$site))%>%
  select(Site=site,Month=date,Year=yr,Plot=plot,Quadrat=quadrat,`T. Testudinum Shoot Density per m2`=shd)%>%
  arrange(Site,Month,Year,Plot,Quadrat)

write.csv(sd3,"02_wdata/sddata_deppreport.csv")


# Exploratory figures for paper

# first look at trends of T. Testudinum density before and after Dorian
sdb<-sd%>%
  mutate(prepost=ifelse(sd$yr<2019,"pre-Dorian","post-Dorian"),
         Date=my(paste(mnth,yr)),
         site2=case_when(
           site %in% c("BOR","BOR1","BOR2","BOR3","BOR4","Jerrys")~"BOR",
           !site %in% c("BOR","BOR1","BOR2","BOR3","BOR4","Jerrys")~site),
         grp1=case_when(
           prepost=="pre-Dorian"~1,
           prepost=="post-Dorian" & site %in% c("BOR","BOR1","BOR2","BOR3","BOR4","Jerrys")~2,
           prepost=="post-Dorian" & site %in% c("Camp Abaco","Snake Cay")~3),
         Date2=ifelse(prepost=="pre-Dorian","pre-Dorian",as.character(Date)))%>%
  filter(!is.na(shd))%>%
  filter(!site %in% c("Man o War","Hope Town"))

colrs.sda<-colrs%>%
  filter(site %in% unique(sdb$site))%>%
  filter(!site %in% c("Man o War","Hope Town"))

sdb$site<-factor(sdb$site,levels=colrs.sda$site)
sdb$Date2<-factor(sdb$Date2,levels=c("pre-Dorian","2019-11-01",
                                     "2020-05-01","2021-06-01","2021-12-01"))
sdc<-sdb%>%
  filter(!site %in% c("Man o War","Hope Town"))%>%
  group_by(site,Date2)%>%
  summarize(mean.shd=mean(shd),
            shd.l95=mean.shd-1.96*(sd(shd)/n()),
            shd.u95=mean.shd+1.96*(sd(shd)/n()))%>%
  filter(Date2!="pre-Dorian")%>%
  mutate(date3=as.numeric(Date2))

siteblegh<-data.frame(site=unique(sdb$site),date3=1)

sdd<-sdb%>%
  filter(!site %in% c("Man o War","Hope Town"))%>%
  group_by(site,Date2)%>%
  summarize(mean.shd=mean(shd),
            shd.l95=mean.shd-1.96*(sd(shd)/n()),
            shd.u95=mean.shd+1.96*(sd(shd)/n()))%>%
  filter(Date2=="pre-Dorian")%>%
  mutate(date3=as.numeric(Date2))%>%
  full_join(siteblegh)

ggplot()+
  geom_errorbar(aes(x=date3,ymin=shd.l95,ymax=shd.u95,color=site),width=.2,data=sdd)+
  geom_errorbar(aes(x=date3,ymin=shd.l95,ymax=shd.u95,color=site),width=.2,data=sdc)+
  geom_line(aes(x=date3,y=mean.shd,group=site,color=site),size=1,data=sdc)+
  geom_segment(aes(x=1,xend=2,y=546.25850,yend=475.00),linetype="dotted",size=1,color="#7DFF56FF",alpha=.75)+
  geom_segment(aes(x=1,xend=2,y=546.25850,yend=550.00),linetype="dotted",size=1,color="#7DFF56FF",alpha=.75)+
  geom_segment(aes(x=1,xend=2,y=546.25850,yend=500.00),linetype="dotted",size=1,color="#7DFF56FF",alpha=.75)+
  geom_segment(aes(x=1,xend=2,y=546.25850,yend=470.00),linetype="dotted",size=1,color="#7DFF56FF",alpha=.75)+
  geom_segment(aes(x=1,xend=2,y=629.3750,yend=42.5),linetype="dotted",size=1,color="#F1CA3AFF",alpha=.75)+
  geom_segment(aes(x=1,xend=2,y=579.4129,yend=351.25),linetype="dotted",size=1,color="#C1F334FF",alpha=.75)+
  geom_segment(aes(x=1,xend=2,y=489.3750,yend=41.25),linetype="dotted",size=1,color="#7A0403FF",alpha=.75)+
  geom_segment(aes(x=1,xend=2,y=420.8889,yend=45),linetype="dotted",size=1,color="#BE2102FF",alpha=.75)+
  geom_point(aes(x=date3,y=mean.shd,color=site),size=4,data=sdd)+
  geom_point(aes(x=date3,y=mean.shd,color=site),size=4,data=sdc)+
  geom_vline(aes(xintercept=1.5),size=2,linetype="dashed",color="grey",alpha=.75)+
  scale_color_manual(name="Site",values=colrs.sda$sg.color,labels=colrs.sda$site.name)+
  scale_x_continuous(labels=c("Pre-Dorian","+2 Months","+8 Months","+21 Months","+27 Months"),breaks=c(1,2,3,4,5),name="Time relative to Hurricane Dorian")+
  ylab(tlab)+
  theme(panel.grid=element_blank(),
        legend.margin = margin(t=0,l=45,b=5,r=0),
        legend.title = element_text(hjust=.5,size=15))

ggsave("04_figures/ttestudinumchangeovertime.png",width=8,height=6)
ggsave("04_figures/ttestudinumchangeovertime.jpg",dpi=500,width=8,height=5)
