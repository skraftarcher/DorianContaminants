# script to address the question - 
# How do water HM concentrations compare between marinas and seagrass beds

# Load packages and set preferences
source("03_scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")
lp("readxl")
lp("lmerTest")

theme_set(theme_bw()+theme(panel.grid=element_blank()))

# bing in data----
hm.wat<-read.csv("01_odata/water_contaminants.csv")
sites<-read_xlsx("01_odata/sites.xlsx",sheet = "december")%>%
  mutate(Site=Site2)

hm.wat$Site<-factor(hm.wat$Site,levels=c("Treasure Cay Marina",
                                         "Treasure Cay",
                                         "Hills Creek",
                                         "Marsh Harbour",
                                         "Rainbow Rentals",
                                         "Boat Harbour",
                                         "Fish House",
                                         "Jib Room",
                                         "Calcutta",
                                         "Camp Abaco",
                                         "Snake Cay",
                                         "BOR",
                                         "Jerry's",
                                         "Jerrys",
                                         "Crossing Rocks"))
sites$Site<-factor(sites$Site,levels=c("Treasure Cay Marina",
                                       "Treasure Cay",
                                       "Hills Creek",
                                       "Marsh Harbour",
                                       "Rainbow Rentals",
                                       "Boat Harbour",
                                       "Fish House",
                                       "Jib Room",
                                       "Calcutta",
                                       "Camp Abaco",
                                       "Snake Cay",
                                       "BOR",
                                       "Jerry's",
                                       "Jerrys",
                                       "Crossing Rocks"))

sites<-arrange(sites,Site)

#organize data----
hm.wat$sampling<-factor(hm.wat$sampling,levels=c("November2019","May2020","June2021","December2021"))

hm.wat2<-hm.wat%>%
  left_join(sites[,c(9,6:8)])%>%
  separate(sampling,into=c("mnth","yr"),sep="20",remove = FALSE)%>%
  mutate(mnth2=case_when(
    mnth=="June"~6,
    mnth=="December"~12),
    yr2=paste0(20,as.numeric(yr)),
    date=ym(paste(yr2,mnth2)),
    site.type=case_when(
      is.seagrass==1~"seagrass",
      is.marina==1~"marina"))

# subset down to sites that have samples in both June and December
hm.wat3<-hm.wat2%>%
  filter(!Site %in% c("Crossing Rocks","Treasure Cay"))

par(mfrow=c(2,2))

# Chromium
#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Cr,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - without snake cay
ggplot(data=hm.wat3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Cr,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Cr,fill=sampling))+
  facet_wrap(~site.type)
# pattern in water is diferent between samplings/site type
# but different between site types doesn't appear to depend on sampling

# which site has the high chromium?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=Cr,color=site.type),width=.1)
#Jerry's

cr.lm<-lmer(Cr~site.type+(1|sampling),data=hm.wat3)
plot(cr.lm)# residuals look good 
summary(cr.lm)

# not significant, but Morely not

# Nickel
#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Ni,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - without snake cay
ggplot(data=hm.wat3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Ni,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Ni,fill=sampling))+
  facet_wrap(~site.type)
# pattern in water is different between samplings/site type
# but different between site types doesn't appear to depend on sampling

# which site has the high Nickel?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=Ni,color=site.type),width=.1)
#Hills Creek

Ni.lm<-lmer(Ni~site.type+(1|sampling),data=hm.wat3%>%
              filter(Ni<1))
plot(Ni.lm)# residuals look ok 
summary(Ni.lm)
# not significant unless you get rid of the outlier in Hills Creek. 

# Copper
#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Cu,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - without snake cay
ggplot(data=hm.wat3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Cu,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Cu,fill=sampling))+
  facet_wrap(~site.type)
# pattern in water is diferent between samplings/site type
# but different between site types doesn't appear to depend on sampling

# which sites have the high copper?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=Cu,color=site.type),width=.1)

Cu.lm<-lmer(Cu~site.type+(1|sampling),data=hm.wat3)
plot(Cu.lm)# residuals look good 
summary(Cu.lm)

# copper is lower in seagrass beds

# Zinc
#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Zn,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - without snake cay
ggplot(data=hm.wat3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Zn,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Zn,fill=sampling))+
  facet_wrap(~site.type)
# pattern in water is diferent between samplings/site type
# but different between site types doesn't appear to depend on sampling

# which site has the high chromium?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=Zn,color=site.type),width=.1)

Zn.lm<-lmer(Zn~site.type+(1|sampling),data=hm.wat3)
plot(Zn.lm)# residuals look good 
summary(Zn.lm)

#not significant but close

# Arsenic
#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=As,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - without snake cay
ggplot(data=hm.wat3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=As,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=As,fill=sampling))+
  facet_wrap(~site.type)

# which site has the high arsenic?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=As,color=site.type),width=.1)

As.lm<-lm(As~site.type,data=hm.wat3)
plot(As.lm)# residuals look good 
summary(As.lm)


# Lead
#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Pb,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - without snake cay
ggplot(data=hm.wat3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Pb,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=Pb,fill=sampling))+
  facet_wrap(~site.type)

# which site has the high lead?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=Pb,color=site.type),width=.1)
#Hills Creek, Camp AMoco, Snake Cay

Pb.lm<-lmer(Pb~site.type+(1|sampling),data=hm.wat3)
plot(Pb.lm)# residuals look good 
summary(Pb.lm)
#not significant

# Chromium
#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=V,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - without snake cay
ggplot(data=hm.wat3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=V,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=V,fill=sampling))+
  facet_wrap(~site.type)

# which site has the high V?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=V,color=site.type),width=.1)
#Snake Cay

V.lm<-lmer(V~site.type+(1|sampling),data=hm.wat3)
plot(V.lm)# residuals look good 
summary(V.lm)
# not significant

# not look at Fe, Mn, Mo, Mo only in June

hm.wat4<-hm.wat3%>%
  filter(!is.na(Fe))

# Iron
#first visualize - with snake cay
ggplot(data=hm.wat4)+
  geom_boxplot(aes(y=Fe,fill=site.type))

#first visualize - without snake cay
ggplot(data=hm.wat4%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Fe,fill=site.type))+
  facet_wrap(~sampling)


# which site has the high chromium?
ggplot(data=hm.wat3)+
  geom_jitter(aes(x=Site,y=Fe,color=site.type),width=.1)
#Jerry's

Fe.lm<-lm(Fe~site.type,data=hm.wat4)
plot(Fe.lm)# residuals look good 
summary(Fe.lm)

# no significant difference

# Mn
#first visualize - with snake cay
ggplot(data=hm.wat4)+
  geom_boxplot(aes(y=Mn,fill=site.type))

#first visualize - without snake cay
ggplot(data=hm.wat4%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Mn,fill=site.type))


# which site has the high chromium?
ggplot(data=hm.wat4)+
  geom_jitter(aes(x=Site,y=Mn,color=site.type),width=.1)


Mn.lm<-lm(Mn~site.type,data=hm.wat4)
plot(Mn.lm)# residuals look good 
summary(Mn.lm)

# no difference

# Ba
#first visualize - with snake cay
ggplot(data=hm.wat4)+
  geom_boxplot(aes(y=Ba,fill=site.type))

#first visualize - without snake cay
ggplot(data=hm.wat4%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Ba,fill=site.type))


# which site has the high chromium?
ggplot(data=hm.wat4)+
  geom_jitter(aes(x=Site,y=Ba,color=site.type),width=.1)


Ba.lm<-lm(Ba~site.type,data=hm.wat4)
plot(Ba.lm)# residuals look good 
summary(Ba.lm)


# Mo
#first visualize - with snake cay
ggplot(data=hm.wat4)+
  geom_boxplot(aes(y=Mo,fill=site.type))

#first visualize - without snake cay
ggplot(data=hm.wat4%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Mo,fill=site.type))


# which site has the high chromium?
ggplot(data=hm.wat4)+
  geom_jitter(aes(x=Site,y=Mo,color=site.type),width=.1)


Mo.lm<-lm(Mo~site.type,data=hm.wat4)
plot(Mo.lm)# residuals look good 
summary(Mo.lm)

# do total HM

hm.wat3<-hm.wat3%>%
  mutate(total.hm=Cr+Ni+Cu+Zn+As+Pb+V)

# firt visualize
ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=total.hm,fill=site.type))+
  facet_grid(~sampling)


ggplot(data=hm.wat3)+
  geom_boxplot(aes(y=total.hm,fill=sampling))+
  facet_grid(~site.type)

total.hm.lm<-lm(total.hm~site.type,data=hm.wat3)
plot(total.hm.lm)# residuals look good 
summary(total.hm.lm)

# save model output for manuscript
write_rds(summary(As.lm)$coefficients,"02_wdata/water_marinavsg_arsenic.rds")
write_rds(summary(cr.lm)$coefficients,"02_wdata/water_marinavsg_chromium.rds")
write_rds(summary(Cu.lm)$coefficients,"02_wdata/water_marinavsg_copper.rds")
write_rds(summary(Ni.lm)$coefficients,"02_wdata/water_marinavsg_nickel.rds")
write_rds(summary(Pb.lm)$coefficients,"02_wdata/water_marinavsg_lead.rds")
write_rds(summary(Zn.lm)$coefficients,"02_wdata/water_marinavsg_zinc.rds")
write_rds(summary(V.lm)$coefficients,"02_wdata/water_marinavsg_vanadium.rds")
write_rds(summary(total.hm.lm)$coefficients,"02_wdata/water_marinavsg_totalhm.rds")
