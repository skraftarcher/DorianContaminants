# pair contaminant and seagrass data

# Stephanie K Archer 7/8/2022

# bring in packages and data
source("03_scripts/install_packages_function.R")

lp("tidyverse")
lp("patchwork")

#heavy metal data
hm<-read.csv("01_odata/contam.csv")
sed<-read.csv("01_odata/sediment_whirlpack_dec2021.csv")[6:60,]%>%
  mutate(Plot=as.numeric(Plot),
         Site=ifelse(Site=="BOR 2","BOR2",Site))

# bring in seagrass density data
sg<-read.csv("02_wdata/USE_allyrs_shootdensity.csv")

summary(sg)

sg1<-sg%>%
  filter(yr==2021)%>%
  filter(mnth==12)%>%
  rename(Site=site,Plot=plot)

# organize contaminant info to get rid of # signs and triplicates
hm2<-hm%>%
  separate(Sample.ID,into=c("ID","WhirlpakID"),sep="#")%>%
  separate(WhirlpakID,into=c("WhirlpakID","rep"),sep=3)%>%
  select(-ID)%>%
  pivot_longer(Cr:Ba,names_to = "hm",values_to="conc")

# now join datasets together
sg2<-sg1%>%
  group_by(Site,Plot)%>%
  summarize(m.shd=mean(shd),sd.shd=sd(shd),
            max.shd=max(shd))%>%
  left_join(sed)%>%
  left_join(hm2)%>%
  filter(!is.na(hm))

ggplot(sg2,aes(x=m.shd,y=conc))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(rows=vars(hm),cols=vars(Site),scales="free")


