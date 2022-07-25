# respiromatry analysis

# load packages
source("03_scripts/install_packages_function.R")
lp("tidyverse")
lp("lubridate")
lp("viridis")
lp("patchwork")
lp("readxl")
colrs<-read.csv("01_odata/colrs.csv")
theme_set(theme_bw())

# load data
sd<-read.csv("02_wdata/USE_allyrs_shootdensity.csv")%>%
  filter(yr==2021)%>%
  filter(mnth==12)%>%
  group_by(site,yr,mnth)%>%
  summarize(mshd=mean(shd),shd.cil=mshd-(1.96*(sd(shd)/sqrt(n()))),shd.ciu=mshd+(1.96*(sd(shd)/sqrt(n()))))


#december
decr<-read_xlsx("01_odata/Respirometry_December_2021.xlsx")%>%
  filter(!is.na(change.02))%>%
  left_join(sd)


# plot up
colrs2<-filter(colrs,site %in% unique(decr$site))

decr$site<-factor(decr$site,levels=colrs2$site)
lab02<-expression(paste("Water column O"["2"]," flux (",mu,"mol h"^"-1",")"))

ggplot(decr%>%
         filter(sed.water=="water"))+
  geom_hline(aes(yintercept=0),linetype="dashed")+
  #geom_jitter(aes(x=mshd,y=change.02,color=site,pch=sed.water),size=3,alpha=.5)+
  scale_fill_manual(values=colrs2$sg.color,labels=colrs2$site.name,name="Site")+
  geom_boxplot(aes(fill=site,y=change.02,x=site))+
  ylab(lab02)+
  xlab("")+
  theme(panel.grid = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=14))

ggsave("04_figures/december_o2_water.jpg")
