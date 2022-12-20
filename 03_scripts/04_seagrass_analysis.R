# initial exploration of seagrass data 

# Stephanie K. Archer 04/04/2022
# to make example for best practice doc
# load packaeges
source("03_scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("vegan")
lp("viridis")
lp("glmmTMB")
lp("DHARMa")
lp("patchwork")
lp("ggeffects")
colrs<-read.csv("01_odata/colrs.csv")
theme_set(theme_bw())

# bb data
# load data
bb<-read.csv("02_wdata/USE_allyrs_seagrass_bb.csv")%>%
  mutate(period=ifelse(yr<2019,"Pre-Dorian","Post-Dorian"),
         date=ym(paste(yr,mnth)))


bb$taxa<-factor(bb$taxa,labels=c("Halodule wrightii","Syringodium filiforme","Thalassia testudinum"))


#Over Time
bb3<-bb%>%
  mutate(site=case_when(
    site=="BOR"~"BOR",
    site=="BOR1"~"BOR",
    site=="BOR2"~"BOR",
    site=="BOR3"~"BOR",
    site=="BOR4"~"BOR",
    site=="Jerrys"~"BOR",
    site=="Hills Creek"~"Hills Creek",
    site=="Snake Cay"~"Snake Cay"))%>%
  group_by(period,site,yr,date,taxa)%>%
  summarize(Abundance=mean(abund),se=sd(abund)/(sqrt(n()+1)))%>%
  filter(site %in% unique(colrs$site))%>%
  left_join(colrs)%>%
  filter(!site %in% c("Hope Town","Man o War"))%>%
  mutate(Abundance=ifelse(is.na(Abundance),0,Abundance),
         se=ifelse(is.na(se),0,se))%>%
  filter(site %in% c("Hills Creek","Snake Cay","BOR"))

colrs2<-colrs%>%
  filter(site %in% unique(bb3$site))


bb3$site<-factor(bb3$site,levels=colrs2$site)
bb3$period<-factor(bb3$period,levels=c("Pre-Dorian","Post-Dorian"))

(bbplot<-ggplot(bb3)+  
    geom_errorbar(aes(x=date,ymin=Abundance-se,ymax=Abundance+se,color=site,group=period),width=.2)+
    #geom_vline(xintercept=my("09-2019"),size=2,alpha=.5,linetype="dashed")+
    geom_line(aes(x=date,y=Abundance,group=site,color=site),size=1.25,alpha=.5)+
    geom_point(aes(x=date,y=Abundance,color=site,group=site),size=2)+
    scale_color_manual(name="Site",labels=colrs2$site.name,values=colrs2$sg.color)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          strip.text = element_text(face="italic"))+
    #scale_x_discrete(name = "Sampling",labels=c("+2 Months","+8 Months","+21 Months","+27 Months"))+
    ylab("Abundance")+
    facet_grid(cols=vars(period),rows=vars(taxa),scales="free"))

#residuals function
glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}

# analysis
hist(bb2$abund)


bb2<-bb%>%
  filter(!site %in% c("Hope Town","Man o War"))%>%
  filter(period=="Post-Dorian")%>%
  mutate(sideofstorm=case_when(
    site %in% c("Treasure Cay","Hills Creek")~"North",
    site %in% c("Camp Abaco","Snake Cay")~"Central",
    site %in% c("BOR","BOR1","BOR2","BOR3","BOR4","Jerrys")~"South"))
  
# shoot density data

sd<-read.csv("02_wdata/USE_allyrs_shootdensity.csv")%>%
  mutate(period=ifelse(yr<2019,"Pre-Dorian","Post-Dorian"),
         date=ym(paste(yr,mnth)))

sd2<-sd%>%
  filter(!site %in% c("Hope Town","Man o War"))%>%
  filter(period=="Post-Dorian")%>%
  mutate(sideofstorm=case_when(
    site %in% c("Treasure Cay","Hills Creek")~"North",
    site %in% c("Camp Abaco","Snake Cay")~"Central",
    site %in% c("BOR","BOR1","BOR2","BOR3","BOR4","Jerrys")~"South"))

hist(sd2$shd)
sd2$sideofstorm<-factor(sd2$sideofstorm,levels = c("North","Central","South"))

sdTlm<-glmmTMB(shd~sideofstorm*date+(1|site),
               data=sd2)


glmm.resids(sdTlm)

summary(sdTlm)

sdt<-ggpredict(sdTlm,terms=c("date","sideofstorm"))
plot(sdt)
tlab<-expression(paste(italic("Thalassia testudinum"),"shoots m"^"-2"))
ggplot()+
  geom_point(aes(x=date,y=shd,color=sideofstorm),
              size=5,alpha=.25,data=sd2%>%
                group_by(site,date,sideofstorm)%>%
                summarize(shd=mean(shd)),
              position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=date,ymin=sdem,ymax=shep,color=sideofstorm),
              alpha=.25,data=sd2%>%
                group_by(site,date,sideofstorm)%>%
                summarize(shep=mean(shd)+sd(shd),
                          sdem=mean(shd)-sd(shd)),
              position=position_dodge(width=0.5))+
  geom_line(aes(x=x,y=predicted,color=group),data=sdt)+
  theme(panel.grid = element_blank())+
  ylab(tlab)+
  scale_color_manual(values=c("red","orange","blue"),name="Side of Storm")+
  scale_x_continuous(labels=c("+2 Months","+8 Months","+21 Months","+27 Months"),breaks=c(ymd("2019-11-01") ,ymd("2020-05-01") ,ymd("2021-06-01") ,ymd("2021-12-01")),name="Time since Hurricane Dorian")

