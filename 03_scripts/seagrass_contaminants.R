# pair contaminant and seagrass data

# Stephanie K Archer 7/8/2022

# bring in packages and data
source("03_scripts/install_packages_function.R")

lp("tidyverse")
lp("patchwork")
lp("glmmTMB")
lp("DHARMa")
lp("readxl")
lp("ggeffects")

#heavy metal data
hm<-read.csv("01_odata/contam.csv")
sed<-read.csv("01_odata/sediment_whirlpack_dec2021.csv")[6:60,]%>%
  mutate(Plot=as.numeric(Plot),
         Site=ifelse(Site=="BOR 2","BOR2",Site))
hm.levels<-read_xlsx("01_odata/hm_loads_sharifuzzaman.xlsx")%>%
  rename(hm=heavymetal)

# bring in seagrass density data
sg<-read.csv("01_odata/December2021_seagrass.csv")%>%
  group_by(Site, Plot, Taxa)%>%
  summarize(shd=sum(Density,na.rm = TRUE)/.04)%>%
  pivot_wider(names_from = Taxa,values_from = shd,values_fill = 0)%>%
  select(Site,Plot,Th="T",H,S)%>%
  mutate(total.shd=Th+H+S,
    Site=ifelse(Site=="Hills Cr","Hills Creek",Site))
  

summary(sg)


# organize contaminant info to get rid of # signs and triplicates
hm2<-hm%>%
  separate(Sample.ID,into=c("ID","WhirlpakID"),sep="#")%>%
  separate(WhirlpakID,into=c("WhirlpakID","rep"),sep=3)%>%
  select(-ID)%>%
  pivot_longer(Cr:Ba,names_to = "hm",values_to="conc")%>%
  left_join(hm.levels)%>%
  group_by(hm)%>%
  mutate(bn=min(conc))

# now join datasets together
sg2<-sg%>%
  left_join(sed)%>%
  mutate(Site=ifelse(Site=="Jerrys","Jerry's",Site),
         Site=ifelse(Site=="BOR2","BOR",Site))%>%
  left_join(hm2)%>%
  filter(!is.na(hm))%>%
  mutate(pol.level=case_when(
    conc < mod_pol ~"Unpoluted",
    conc >= mod_pol & conc < heavy_pol ~"Moderately poluted",
    conc >= heavy_pol~"Heavily poluted"))

sg3<-sg2%>%
  mutate(CF=conc/bn)%>%
  group_by(Site,Plot)%>%
  mutate(PLI=sum(CF)^(1/10))%>%
  select(Site,Plot,Th,H,S,total.shd,PLI)%>%
  distinct()

sg4<-sg2%>%
  filter(!is.na(TEL))%>%
  mutate(CF=conc/bn)%>%
  group_by(Site,Plot)%>%
  mutate(PLI=sum(CF)^(1/6))%>%
  select(Site,Plot,Th,H,S,total.shd,PLI)%>%
  distinct()

sg2$Site<-factor(sg2$Site,levels=c("Treasure Cay",
                 "Hills Creek",
                 "Camp Abaco",
                 "Snake Cay",
                 "Jerry's",
                 "BOR"))

ggplot(data=sg2,aes(color=Site))+
  geom_point(aes(x=total.shd,y=conc))+
  geom_smooth(method="lm",aes(x=total.shd,y=conc))+
  # geom_point(aes(x=Th,y=conc))+
  # geom_smooth(method="lm",aes(x=Th,y=conc))+
  # geom_point(aes(x=S,y=conc))+
  # geom_smooth(method="lm",aes(x=S,y=conc))+
  facet_grid(rows=vars(hm),cols=vars(Site),scales="free")

ggsave("04_figures/ugly_seagrass_contams.jpg")


# look at relationship by pollution level
ggplot(data=sg2%>%
         filter(!is.na(mod_pol)))+
  geom_point(aes(x=total.shd,y=conc))+
  geom_smooth(method="lm",aes(x=total.shd,y=conc))+
  # geom_point(aes(x=Th,y=conc))+
  # geom_smooth(method="lm",aes(x=Th,y=conc))+
  # geom_point(aes(x=S,y=conc))+
  # geom_smooth(method="lm",aes(x=S,y=conc))+
  facet_grid(rows=vars(hm),cols=vars(pol.level),scales="free")

ggsave("04_figures/ugly_pol_seagrass_contams.jpg")

# heavy metals and seagrass

glmm.resids<-function(model){
  t1 <- simulateResiduals(model)
  print(testDispersion(t1))
  plot(t1)
}

as.lm<-glmmTMB(conc~total.shd+(1|Site),data=sg2%>%filter(hm=="As"))
glmm.resids(as.lm)
summary(as.lm)
# positive relationship between seagrass density and arsenic concentrations

cr.lm<-glmmTMB(conc~total.shd+(1|Site),data=sg2%>%
                 filter(hm=="Cr")%>%
                 filter(conc<300))
glmm.resids(cr.lm)
summary(cr.lm)
# no significant relationship between seagrass density and chromium concentrations

cu.lm<-glmmTMB(conc~total.shd+(1|Site),data=sg2%>%
                 #filter(conc>heavy_pol)%>%
                 filter(hm=="Cu"))
glmm.resids(cu.lm)
summary(cu.lm)
#significant positive relationship

fe.lm<-glmmTMB(conc~total.shd+(1|Site),data=sg2%>%filter(hm=="Fe"))
glmm.resids(fe.lm)
summary(fe.lm)
# no significant relationship

mn.lm<-glmmTMB(conc~total.shd+(1|Site),data=sg2%>%filter(hm=="Mn"))
glmm.resids(mn.lm)
summary(mn.lm)
# no significant relationship

ni.lm<-glmmTMB(conc~total.shd+(1|Site),data=sg2%>%filter(hm=="Ni"))
glmm.resids(ni.lm)
summary(ni.lm)
# significant positive relationship


pb.lm<-glmmTMB(log(conc)~total.shd+(1|Site),
               data=sg2%>%
                 filter(hm=="Pb"))
glmm.resids(pb.lm)
summary(pb.lm)
# no significant relationship


v.lm<-glmmTMB(conc~total.shd+(1|Site),data=sg2%>%filter(hm=="V"))
glmm.resids(v.lm)
summary(v.lm)
# no significant relationship


zn.lm<-glmmTMB(log(conc)~total.shd+(1|Site),
               data=sg2%>%
                 filter(hm=="Zn")%>%
                 filter(conc<1000))
glmm.resids(zn.lm)
summary(zn.lm)
# no significant relationship

# make figures
sites<-read_xlsx("01_odata/sites.xlsx",sheet = "december")%>%
  mutate(Site=Site2)
hm2.colors<-data.frame(Site=unique(sg2$Site))%>%
  left_join(sites)
hm2.colors$Site<-factor(hm2.colors$Site,levels=c("Treasure Cay",
                                                 "Hills Creek",
                                                 "Camp Abaco",
                                                 "Snake Cay",
                                                 "BOR",
                                                 "Jerry's"))




theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           axis.title=element_text(size=14)))
sglab<-expression(paste("Seagrass shoots m"^"-2"))
hmlab<-"mg/kg"

#Arsenic
as.sig<-expression(paste(beta," = 0.008, z = 1.93, p = 0.05"))

asg<-ggpredict(as.lm,terms = "total.shd")

(asp<-ggplot()+
    geom_hline(data=sg2%>%
                 filter(hm=="As"),
               aes(yintercept=mod_pol),
               linetype="dashed",color="grey")+
    geom_hline(data=sg2%>%
                 filter(hm=="As"),
               aes(yintercept=PEL),
               linetype="dashed",color="red")+
  geom_point(data=sg2%>%
               filter(hm=="As"),
             aes(x=total.shd,y=conc,color=Site),size=3, alpha=.3)+
  geom_line(data=asg,aes(x=x,y=predicted))+
  geom_ribbon(data=asg,aes(x=x,ymin=conf.low,ymax=conf.high),color="grey",alpha=.2)+
    geom_text(aes(x=525,y=115),label=as.sig,size=7)+
    geom_text(aes(x=1500,y=115),label="Arsenic",size=10)+
    xlab(sglab)+
    ylab(hmlab))

#Chromium
cr.sig<-expression(paste(beta," =  - 0.002, z = -0.38, p = 0.71"))

crg<-ggpredict(cr.lm,terms = "total.shd")

(crp<-ggplot()+
    geom_hline(data=sg2%>%
                 filter(hm=="Cr"),
               aes(yintercept=mod_pol),
               linetype="dashed",color="grey")+
    geom_hline(data=sg2%>%
                 filter(hm=="Cr"),
               aes(yintercept=PEL),
               linetype="dashed",color="red")+
    geom_point(data=sg2%>%
                 filter(hm=="Cr")%>%
                 filter(conc<300),
               aes(x=total.shd,y=conc,color=Site),size=3, alpha=.3)+
    geom_line(data=crg,aes(x=x,y=predicted))+
    geom_ribbon(data=crg,aes(x=x,ymin=conf.low,ymax=conf.high),color="grey",alpha=.2)+
    geom_text(aes(x=1500,y=165),label="Chromium",size=10)+
    geom_text(aes(x=200,y=165),label="p = 0.71",size=7)+
    xlab(sglab)+
    ylab(hmlab))

#Copper
cu.sig<-expression(paste(beta," =  0.02, z = 1.97, p = 0.04"))

cug<-ggpredict(cu.lm,terms = "total.shd")

(cup<-ggplot()+
    geom_hline(data=sg2%>%
                 filter(hm=="Cu"),
               aes(yintercept=mod_pol),
               linetype="dashed",color="grey")+
    geom_hline(data=sg2%>%
                 filter(hm=="Cu"),
               aes(yintercept=PEL),
               linetype="dashed",color="red")+
    geom_point(data=sg2%>%
                 filter(hm=="Cu"),
               aes(x=total.shd,y=conc,color=Site),size=3, alpha=.3)+
    geom_line(data=cug,aes(x=x,y=predicted))+
    geom_ribbon(data=cug,aes(x=x,ymin=conf.low,ymax=conf.high),color="grey",alpha=.2)+
    geom_text(aes(x=1500,y=175),label="Copper",size=10)+
    geom_text(aes(x=525,y=175),label=cu.sig,size=7)+
    xlab(sglab)+
    ylab(hmlab))

#Nickel
ni.sig<-expression(paste(beta," = 0.001, z = 0.48, p = 0.63"))

nig<-ggpredict(ni.lm,terms = "total.shd")

(nip<-ggplot()+
    geom_hline(data=sg2%>%
                 filter(hm=="Ni"),
               aes(yintercept=mod_pol),
               linetype="dashed",color="grey")+
    geom_hline(data=sg2%>%
                 filter(hm=="Ni"),
               aes(yintercept=PEL),
               linetype="dashed",color="red")+
    geom_point(data=sg2%>%
                 filter(hm=="Ni"),
               aes(x=total.shd,y=conc,color=Site),size=3, alpha=.3)+
    geom_line(data=nig,aes(x=x,y=predicted))+
    geom_ribbon(data=nig,aes(x=x,ymin=conf.low,ymax=conf.high),color="grey",alpha=.2)+
    geom_text(aes(x=1500,y=105),label="Nickel",size=10)+
    geom_text(aes(x=200,y=105),label="p = 0.63",size=7)+
    xlab(sglab)+
    ylab(hmlab))

#lead
pb.sig<-expression(paste(beta," = - 0.0001, z = -0.72, p = 0.47"))

pbg<-ggpredict(pb.lm,terms = "total.shd")

(pbp<-ggplot()+
    geom_hline(data=sg2%>%
                 filter(hm=="Pb"),
               aes(yintercept=mod_pol),
               linetype="dashed",color="grey")+
    geom_hline(data=sg2%>%
                 filter(hm=="Pb"),
               aes(yintercept=PEL),
               linetype="dashed",color="red")+
    geom_point(data=sg2%>%
                 filter(hm=="Pb"),
               aes(x=total.shd,y=conc,color=Site),
               size=3,alpha=.3)+
    geom_line(data=pbg,aes(x=x,y=predicted))+
    geom_ribbon(data=pbg,aes(x=x,ymin=conf.low,ymax=conf.high),color="grey",alpha=.2)+
    geom_text(aes(x=1500,y=345),label="Lead",size=10)+
    geom_text(aes(x=200,y=345),label="p = 0.47",size=7)+
    xlab(sglab)+
    ylab(hmlab))


#zinc
zn.sig<-expression(paste(beta," = - 0.0001, z = -0.69, p = 0.49"))

zng<-ggpredict(zn.lm,terms = "total.shd")

(znp<-ggplot()+
    geom_hline(data=sg2%>%
                 filter(hm=="Zn"),
               aes(yintercept=mod_pol),
               linetype="dashed",color="grey")+
    geom_hline(data=sg2%>%
                 filter(hm=="Zn"),
               aes(yintercept=PEL),
               linetype="dashed",color="red")+
    geom_point(data=sg2%>%
                 filter(hm=="Zn")%>%
                 filter(conc<1000),
               aes(x=total.shd,y=conc,color=Site),
               size=3,alpha=.3)+
    geom_line(data=zng,aes(x=x,y=predicted))+
    geom_ribbon(data=zng,aes(x=x,ymin=conf.low,ymax=conf.high),color="grey",alpha=.2)+
    geom_text(aes(x=1500,y=800),label="Zinc",size=10)+
    geom_text(aes(x=200,y=800),label="p = 0.49",size=7)+
    xlab(sglab)+
    ylab(hmlab))


sghm<-(asp + crp + cup + nip + pbp + znp + plot_layout(guides = "collect"))

ggsave(plot=sghm,"04_figures/seagrass_heavymetal_relationships.png",width=21,height=10)


sigsghm<-(asp + cup + plot_layout(guides = "collect"))

ggsave(plot=sigsghm,"04_figures/seagrass_heavymetal_relationships_significant.png",width=21,height=10)


# look at pollution load index by searass density
ggplot(sg4,aes(x=total.shd,y=PLI))+
  geom_point()+
  geom_smooth(method="lm")

sg4<-sg4%>%
  mutate(dori=case_when(
    Site=="Treasure Cay"~6,
    Site=="Hills Creek"~5,
    Site=="Camp Abaco"~4,
    Site=="Snake Cay"~3,
    Site=="BOR"~2,
    Site=="Jerry's"~1),
    dumpi=case_when(
      Site=="Treasure Cay"~3,
      Site=="Hills Creek"~4,
      Site=="Camp Abaco"~5,
      Site=="Snake Cay"~6,
      Site=="BOR"~5,
      Site=="Jerry's"~4))

sg3<-sg3%>%
  mutate(dori=case_when(
    Site=="Treasure Cay"~6,
    Site=="Hills Creek"~5,
    Site=="Camp Abaco"~4,
    Site=="Snake Cay"~3,
    Site=="BOR"~2,
    Site=="Jerry's"~1),
    dumpi=case_when(
      Site=="Treasure Cay"~3,
      Site=="Hills Creek"~4,
      Site=="Camp Abaco"~5,
      Site=="Snake Cay"~6,
      Site=="BOR"~5,
      Site=="Jerry's"~4))

plilm<-glmmTMB(PLI~total.shd+dori,data=sg3)
glmm.resids(plilm)
summary(plilm)

# look at relationship with an index of dorian's intensity
dorig<-ggpredict(plilm,terms = "dori")
shdg<-ggpredict(plilm,term="total.shd")

ggplot()+
  geom_jitter(aes(color=total.shd,x=dori,y=PLI),data=sg3,width=.1,size=5,alpha=.4)+
  scale_color_viridis_c(option="B",end=.8,name="Seagrass \nshoot density")+
  geom_line(data=dorig,aes(x=x,y=predicted))+
  geom_ribbon(data=dorig,aes(x=x,ymin=conf.low,ymax=conf.high),alpha=.3)+
  theme(axis.title = element_text(size=14))+
  xlab("Index of Dorian's Intensity")+
  ylab("Pollution Load Index")+
  geom_text(aes(x=1.25,y=1.7),size=7,label="p = 0.04")
ggsave("04_figures/dorian_pli_model.jpg",width=7,height=5)

ggplot()+
  geom_jitter(aes(x=total.shd,color=dori,y=PLI),data=sg3,width=.1,size=5,alpha=.4)+
  scale_color_viridis_c(option="B",end=.8,name="Index of \nDorian's Intensity")+
  geom_line(data=shdg,aes(x=x,y=predicted))+
  geom_ribbon(data=shdg,aes(x=x,ymin=conf.low,ymax=conf.high),alpha=.3)+
  theme(axis.title = element_text(size=14))+
  xlab(sglab)+
  ylab("Pollution Load Index")+
  geom_text(aes(x=125,y=1.7),size=7,label="p = 0.19")

ggsave("04_figures/seagrass_pli_model.jpg",width=7,height=5)


