# script to address the question - 
# How do sediment HM concentrations compare between marinas and seagrass beds

# Load packages and set preferences
source("03_scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")
lp("readxl")
lp("lmerTest")

theme_set(theme_bw()+theme(panel.grid=element_blank()))

# bing in data----
hm.sed<-read.csv("01_odata/sediment_contaminants.csv")
sites<-read_xlsx("01_odata/sites.xlsx",sheet = "december")%>%
  mutate(Site=Site2)

hm.sed$Site<-factor(hm.sed$Site,levels=c("Treasure Cay Marina",
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
hm.sed$sampling<-factor(hm.sed$sampling,levels=c("November2019","May2020","June2021","December2021"))

hm.sed2<-hm.sed%>%
  filter(!is.na(Site))%>%
  left_join(sites[,c(9,6:8)])%>%
  separate(sampling,into=c("mnth","yr"),sep="20",remove = FALSE)%>%
  mutate(mnth2=case_when(
    mnth=="June"~6,
    mnth=="December"~12,
    mnth=="November"~11),
    yr2=paste0(20,as.numeric(yr)),
    date=ym(paste(yr2,mnth2)),
    site.type=case_when(
      is.seagrass==1~"seagrass",
      is.marina==1~"marina"))

# subset down to sites that have samples in both June and December
hm.sed3<-hm.sed2%>%
  filter(!Site %in% c("Crossing Rocks"))%>%
  filter(sampling!="November2019")

par(mfrow=c(2,2))

# Chromium
#first visualize - with snake cay
ggplot(data=hm.sed3)+
  geom_boxplot(aes(y=Cr,fill=site.type))+
  facet_wrap(~sampling,scales="free")

#first visualize - without snake cay
ggplot(data=hm.sed3%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Cr,fill=site.type))+
  facet_wrap(~sampling,scales="free")

#first visualize - without treasure cay marina
ggplot(data=hm.sed3%>%
         filter(Site!="Treasure Cay Marina"))+
  geom_boxplot(aes(y=Cr,fill=site.type))+
  facet_wrap(~sampling,scales="free")

#first visualize - with snake cay
ggplot(data=hm.sed3)+
  geom_boxplot(aes(y=Cr,fill=sampling))+
  facet_wrap(~site.type,scales="free")

# concentrations are SO different between June and December
# which one is more "normal"?
# average crustal chromium is 100, but heavily pollute is over 75
# NOAA's sediment quality guidelines TEL is 52 and PEL 160 - in this case 
# December seems more realistic?

# subsetting down to only December. I know methods were the same there

hm.sed.dec<-hm.sed3%>%
  filter(sampling=="December2021")

# which site has the high chromium?
ggplot(data=hm.sed.dec)+
  geom_jitter(aes(x=Site,y=Cr,color=site.type),width=.1)
#treasure Cay Marina

cr.lm<-lm(log(Cr)~site.type,data=hm.sed.dec)
plot(cr.lm)
summary(cr.lm)

# very significant, seagrass is lower than marinas

# Nickel
#first visualize - with snake cay
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=Ni,fill=site.type))+
  facet_wrap(~sampling,scales="free")

#first visualize - without snake cay
ggplot(data=hm.sed.dec%>%
         filter(Site!="Snake Cay"))+
  geom_boxplot(aes(y=Ni,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - with snake cay
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=Ni,fill=sampling))+
  facet_wrap(~site.type)

# which site has the high Nickel?
ggplot(data=hm.sed.dec)+
  geom_jitter(aes(x=Site,y=Ni,color=site.type),width=.1)
#treasure cay marina

Ni.lm<-lm(log(Ni)~site.type,data=hm.sed.dec)
plot(Ni.lm)
summary(Ni.lm)

# very significant, seagrass is lower than marinas

# Copper
#first visualize - with snake cay
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=Cu,fill=site.type))+
  facet_wrap(~sampling,scales = "free")

# which sites have the high copper?
ggplot(data=hm.sed.dec)+
  geom_jitter(aes(x=Site,y=Cu,color=site.type),width=.1)

Cu.lm<-lm(log(Cu)~site.type,data=hm.sed.dec)
plot(Cu.lm)# residuals look good 
summary(Cu.lm)

# copper is lower in seagrass beds

# Zinc
#first visualize - with snake cay
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=Zn,fill=site.type))+
  facet_wrap(~sampling)

# which site has the high chromium?
ggplot(data=hm.sed.dec)+
  geom_jitter(aes(x=Site,y=Zn,color=site.type),width=.1)

Zn.lm<-lm(log(Zn)~site.type,data=hm.sed.dec)
plot(Zn.lm)# residuals look good 
summary(Zn.lm)

#much lower in seagrass

# Arsenic
#first visualize - with snake cay
ggplot(data=hm.sed3)+
  geom_boxplot(aes(y=As,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - only in december
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=As,fill=site.type))

# which site has the high arsenic?
ggplot(data=hm.sed.dec)+
  geom_jitter(aes(x=Site,y=As,color=site.type),width=.1)

As.lm<-lm(log(As)~site.type,data=hm.sed.dec)
plot(As.lm)# residuals look good 
summary(As.lm)
# much lower in seagrass

# Lead
#first visualize - with snake cay
ggplot(data=hm.sed3)+
  geom_boxplot(aes(y=Pb,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - only in December
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=Pb,fill=site.type))


# which site has the high lead?
ggplot(data=hm.sed.dec)+
  geom_jitter(aes(x=Site,y=Pb,color=site.type),width=.1)

Pb.lm<-lm(log(Pb)~site.type,data=hm.sed.dec)
plot(Pb.lm) 
summary(Pb.lm)

# Vanadium
#first visualize - with snake cay
ggplot(data=hm.sed3)+
  geom_boxplot(aes(y=V,fill=site.type))+
  facet_wrap(~sampling)

#first visualize - only december
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=V,fill=site.type))

# which site has the high V?
ggplot(data=hm.sed.dec)+
  geom_jitter(aes(x=Site,y=V,color=site.type),width=.1)

V.lm<-lm(log(V)~site.type,data=hm.sed.dec)
plot(V.lm)# residuals look good 
summary(V.lm)

# seagrass is much lower

# do total HM

hm.sed.dec<-hm.sed.dec%>%
  mutate(total.hm=Cr+Ni+Cu+Zn+As+Pb+V)

# firt visualize
ggplot(data=hm.sed.dec)+
  geom_boxplot(aes(y=total.hm,fill=site.type))

total.hm.lm<-lm(log(total.hm)~site.type,data=hm.sed.dec)
plot(total.hm.lm)# residuals look good 
summary(total.hm.lm)

# save model output for manuscript
write_rds(summary(As.lm)$coefficients,"02_wdata/sediment_marinavsg_arsenic.rds")
write_rds(summary(cr.lm)$coefficients,"02_wdata/sediment_marinavsg_chromium.rds")
write_rds(summary(Cu.lm)$coefficients,"02_wdata/sediment_marinavsg_copper.rds")
write_rds(summary(Ni.lm)$coefficients,"02_wdata/sediment_marinavsg_nickel.rds")
write_rds(summary(Pb.lm)$coefficients,"02_wdata/sediment_marinavsg_lead.rds")
write_rds(summary(Zn.lm)$coefficients,"02_wdata/sediment_marinavsg_zinc.rds")
write_rds(summary(V.lm)$coefficients,"02_wdata/sediment_marinavsg_vanadium.rds")
write_rds(summary(total.hm.lm)$coefficients,"02_wdata/sediment_marinavsg_totalhm.rds")
