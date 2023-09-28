#Does seagrass density influence sediment heavy metal concentrations?

# stephanie K archer

# Load packages and set preferences
source("03_scripts/install_packages_function.R")

lp("tidyverse")
lp("lubridate")
lp("readxl")
lp("lmerTest")
lp("AICcmodavg")

theme_set(theme_bw()+theme(panel.grid=element_blank()))
par(mfrow=c(2,2))
# bing in data----
hm.sed<-read.csv("01_odata/sediment_contaminants.csv")
sites<-read_xlsx("01_odata/sites.xlsx",sheet = "december")%>%
  mutate(Site=Site2)
site.vars<-read_xlsx("01_odata/sg_site_descriptions.xlsx")
sg<-read_csv("01_odata/December2021_seagrass.csv")%>%
  mutate(Site=ifelse(Site=="BOR2","BOR",Site),
         Site=ifelse(Site=="Hills Cr","Hills Creek",Site))%>%
  filter(Taxa %in% c("T","H","S"))%>%
  select(Site,Plot,Quadrat,Taxa,Canopy,Density)%>%
  group_by(Site,Plot)%>%
  mutate(Canopy=mean(Canopy,na.rm=TRUE))%>%
  pivot_wider(names_from=Taxa,values_from = Density,values_fill = 0)%>%
  mutate(Tt=`T`/.01,
         Hw=H/.01,
         Sf=S/.01,
         tsg=Tt+Hw+Sf)%>%
  select(-`T`,-H,-S)%>%
  pivot_longer(Tt:tsg,names_to="Taxa",values_to="density")%>%
  group_by(Site,Plot,Taxa)%>%
  summarize(m.dens=mean(density,na.rm=TRUE))%>%
  pivot_wider(names_from=Taxa,values_from = m.dens,values_fill = 0)

sed.whirls<-read_csv("01_odata/sediment_whirlpack_dec2021.csv")%>%
  rename(wSite=Site)

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

sg$Site<-factor(sg$Site,levels=c("Treasure Cay Marina",
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
  mutate(site.type=case_when(
      is.seagrass==1~"seagrass",
      is.marina==1~"marina"))%>%
  filter(sampling=="December2021")%>%
  separate(SampleID,into = c("WhirlpakID","ex"),sep="#")%>%
  mutate(WhirlpakID=ifelse(WhirlpakID=="",ex,WhirlpakID))%>%
  separate(WhirlpakID,into = c("WhirlpakID","ex2"),sep=3)%>%
  filter(site.type=="seagrass")%>%
  select(-ex,-ex2)%>%
  left_join(sed.whirls)%>%
  mutate(Plot=as.numeric(Plot))%>%
  filter(!is.na(Plot))

sg.site<-sg%>%
  ungroup()%>%
  group_by(Site)%>%
  summarize(mHw=mean(Hw),
            mSf=mean(Sf),
            mTt=mean(Tt),
            mtsg=mean(tsg),
            sdHw=sd(Hw),
            sdSf=sd(Sf),
            sdTt=sd(Tt),
            sdtsg=sd(tsg))

hm.sed.dec.site<-hm.sed2%>%
  pivot_longer(Pb:V,names_to="hm",values_to="conc")%>%
  group_by(Site,hm)%>%
  summarize(mconc=mean(conc))%>%
  pivot_wider(names_from=hm,values_from=mconc)

site.sg.hm<-left_join(sg.site,hm.sed.dec.site)%>%
  left_join(site.vars)

# link whirlpacks to plots here then start analysis at site level----
hm.sg.sed<-left_join(sg,hm.sed2[,c(1,2,6:12,19)])%>%
  filter(!is.na(WhirlpakID))%>%
  pivot_longer(Pb:V,names_to="hm",values_to="conc")%>%
  group_by(Site,Plot,Hw,Sf,tsg,Tt,WhirlpakID,hm)%>%
  summarize(m.conc=mean(conc))%>%
  distinct()%>%
  pivot_wider(names_from=hm,values_from=m.conc)%>%
  left_join(site.vars)%>%
  left_join(sg.site)

#save this dataset----
write_csv(hm.sg.sed,"02_wdata/linked_seagrass_sediment_dataset.csv")

#at the site level ----
# make a dataset thats more convenient to plot with 
site.sg.hm.long<-site.sg.hm%>%
  pivot_longer(As:Zn,names_to="hm",values_to="conc")

ggplot(data=site.sg.hm.long,aes(x=mTt,y=conc,color=hm))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~hm,scales="free")+
  ggtitle("Thalassia")

ggplot(data=site.sg.hm.long,aes(x=mHw,y=conc,color=hm))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~hm,scales="free")+
  ggtitle("Halodule")

ggplot(data=site.sg.hm.long,aes(x=mSf,y=conc,color=hm))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~hm,scales="free")+
  ggtitle("Syringodium")

ggplot(data=site.sg.hm.long,aes(x=mtsg,y=conc,color=hm))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~hm,scales="free")+
  ggtitle("total seagrass")

ggplot(data=site.sg.hm.long,aes(x=sdTt,y=conc,color=hm))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~hm,scales="free")+
  ggtitle("Thalassia variability")

ggplot(data=site.sg.hm.long,aes(x=sdtsg,y=conc,color=hm))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~hm,scales="free")+
  ggtitle("total seagrass variability")

site.sg.hm$Site<-factor(site.sg.hm$Site,levels = c("Treasure Cay",
                                                   "Hills Creek",
                                                   "Camp Abaco",
                                                   "Snake Cay",
                                                   "BOR",
                                                   "Jerrys"))

hm.sg.sed$Site<-factor(hm.sg.sed$Site,levels = c("Treasure Cay",
                                                   "Hills Creek",
                                                   "Camp Abaco",
                                                   "Snake Cay",
                                                   "BOR",
                                                   "Jerrys"))
# try some models
# first rescale variables
# look to see which site variables are correlated
(vsite<-cor(site.vars[,-1]))
# lots are correlated

hm.sg.sed<-hm.sg.sed%>%
  ungroup()%>%
  mutate(struct.sc=scale(structures),
         pave.sc=scale(paved_spaces),
         docks.sc=scale(docks),
         dredge.sc=scale(dredged_area),
         blue.sc=scale(blue_hole),
         golf.sc=scale(golf_course),
         dump.dist.sc=scale(dist_dump_km),
         dump.area.sc=scale(area_dump_m2),
         airport.sc=scale(airport_dist),
         power.sc=scale(power_dist),
         debris.sc=scale(dist_debris_stage),
         Tt.sc=scale(Tt),
         tsg.sc=scale(tsg),
         mTt.sc=scale(mTt),
         mtsg.sc=scale(mtsg),
         sdTt.sc=scale(sdTt),
         sdtsg.sc=scale(sdtsg))
# analysis at the site level first

# site AS----
as.site.sg<-lm(As~mTt.sc,
               data=hm.sg.sed)
as.site.dump<-lm(As~dump.dist.sc*dump.area.sc,
               data=hm.sg.sed)
as.site.debris<-lm(As~debris.sc,
                   data=hm.sg.sed)
as.site.power<-lm(As~power.sc,
               data=hm.sg.sed)
as.site.air<-lm(As~airport.sc,
               data=hm.sg.sed)
as.site.struct<-lm(As~struct.sc,
               data=hm.sg.sed)
as.site.gw<-lm(As~blue.sc+dredge.sc,
               data=hm.sg.sed)
as.site.dump.sg<-lm(As~dump.dist.sc*dump.area.sc+
                   debris.sc+
                     mTt.sc,
                 data=hm.sg.sed)

as.site.power.sg<-lm(As~power.sc+
                       mTt.sc,
                  data=hm.sg.sed)
as.site.air.sg<-lm(As~airport.sc+
                  mTt.sc,
                data=hm.sg.sed)
as.site.struct.sg<-lm(As~struct.sc+
                     mTt.sc,
                   data=hm.sg.sed)
as.site.gw.sg<-lm(As~blue.sc+
                    dredge.sc+
                    mTt.sc,
               data=hm.sg.sed)


# Model selection
as.cand.mod.names <- c("as.site.sg",
                       "as.site.dump",
                       "as.site.power",
                       "as.site.air",
                       "as.site.struct",
                       "as.site.gw",
                       "as.site.dump.sg",
                       "as.site.power.sg",
                       "as.site.air.sg",
                       "as.site.struct.sg",
                       "as.site.gw.sg")

as.cand.mods <- list()

# This function fills the list by model names
for (i in 1:length(as.cand.mod.names)) {
  as.cand.mods[[i]] <- get(as.cand.mod.names[i])
}

# Function aictab does the AICc-based model comparison
(as.aic<-data.frame(aictab(
  cand.set = as.cand.mods,
  modnames = as.cand.mod.names
)))


# Site Cr ----
Cr.site.sg<-lm(Cr~mTt.sc,
               data=hm.sg.sed)
Cr.site.dump<-lm(Cr~dump.dist.sc*dump.area.sc+
                   debris.sc,
                 data=hm.sg.sed)
Cr.site.power<-lm(Cr~power.sc,
                  data=hm.sg.sed)
Cr.site.air<-lm(Cr~airport.sc,
                data=hm.sg.sed)
Cr.site.struct<-lm(Cr~struct.sc,
                   data=hm.sg.sed)
Cr.site.gw<-lm(Cr~blue.sc+dredge.sc,
               data=hm.sg.sed)
Cr.site.dump.sg<-lm(Cr~dump.dist.sc*dump.area.sc+
                      debris.sc+
                      mTt.sc,
                    data=hm.sg.sed)
Cr.site.power.sg<-lm(Cr~power.sc+
                       mTt.sc,
                     data=hm.sg.sed)
Cr.site.air.sg<-lm(Cr~airport.sc+
                     mTt.sc,
                   data=hm.sg.sed)
Cr.site.struct.sg<-lm(Cr~struct.sc+
                        mTt.sc,
                      data=hm.sg.sed)
Cr.site.gw.sg<-lm(Cr~blue.sc+
                    dredge.sc+
                    mTt.sc,
                  data=hm.sg.sed)


# Model selection
Cr.cand.mod.names <- c("Cr.site.sg",
                       "Cr.site.dump",
                       "Cr.site.power",
                       "Cr.site.air",
                       "Cr.site.struct",
                       "Cr.site.gw",
                       "Cr.site.dump.sg",
                       "Cr.site.power.sg",
                       "Cr.site.air.sg",
                       "Cr.site.struct.sg",
                       "Cr.site.gw.sg")

Cr.cand.mods <- list()

# This function fills the list by model names
for (i in 1:length(Cr.cand.mod.names)) {
  Cr.cand.mods[[i]] <- get(Cr.cand.mod.names[i])
}

# Function aictab does the AICc-bCred model comparison
(Cr.aic<-data.frame(aictab(
  cand.set = Cr.cand.mods,
  modnames = Cr.cand.mod.names
)))

# Site Cu ----

Cu.site.sg<-lm(Cu~mTt.sc,
               data=hm.sg.sed)
Cu.site.dump<-lm(Cu~dump.dist.sc*dump.area.sc+
                   debris.sc,
                 data=hm.sg.sed)
Cu.site.power<-lm(Cu~power.sc,
                  data=hm.sg.sed)
Cu.site.air<-lm(Cu~airport.sc,
                data=hm.sg.sed)
Cu.site.struct<-lm(Cu~struct.sc,
                   data=hm.sg.sed)
Cu.site.gw<-lm(Cu~blue.sc+dredge.sc,
               data=hm.sg.sed)
Cu.site.dump.sg<-lm(Cu~dump.dist.sc*dump.area.sc+
                      debris.sc+
                      mTt.sc,
                    data=hm.sg.sed)
Cu.site.power.sg<-lm(Cu~power.sc+
                       mTt.sc,
                     data=hm.sg.sed)
Cu.site.air.sg<-lm(Cu~airport.sc+
                     mTt.sc,
                   data=hm.sg.sed)
Cu.site.struct.sg<-lm(Cu~struct.sc+
                        mTt.sc,
                      data=hm.sg.sed)
Cu.site.gw.sg<-lm(Cu~blue.sc+
                    dredge.sc+
                    mTt.sc,
                  data=hm.sg.sed)


# Model selection
Cu.cand.mod.names <- c("Cu.site.sg",
                       "Cu.site.dump",
                       "Cu.site.power",
                       "Cu.site.air",
                       "Cu.site.struct",
                       "Cu.site.gw",
                       "Cu.site.dump.sg",
                       "Cu.site.power.sg",
                       "Cu.site.air.sg",
                       "Cu.site.struct.sg",
                       "Cu.site.gw.sg")

Cu.cand.mods <- list()

# This function fills the list by model names
for (i in 1:length(Cu.cand.mod.names)) {
  Cu.cand.mods[[i]] <- get(Cu.cand.mod.names[i])
}

# Function aictab does the AICc-bCued model comparison
(Cu.aic<-data.frame(aictab(
  cand.set = Cu.cand.mods,
  modnames = Cu.cand.mod.names
)))

# Site Ni ----
Ni.site.sg<-lm(Ni~mTt.sc,
               data=hm.sg.sed)
Ni.site.dump<-lm(Ni~dump.dist.sc*dump.area.sc+
                   debris.sc,
                 data=hm.sg.sed)
Ni.site.power<-lm(Ni~power.sc,
                  data=hm.sg.sed)
Ni.site.air<-lm(Ni~airport.sc,
                data=hm.sg.sed)
Ni.site.struct<-lm(Ni~struct.sc,
                   data=hm.sg.sed)
Ni.site.gw<-lm(Ni~blue.sc+dredge.sc,
               data=hm.sg.sed)
Ni.site.dump.sg<-lm(Ni~dump.dist.sc*dump.area.sc+
                      debris.sc+
                      mTt.sc,
                    data=hm.sg.sed)
Ni.site.power.sg<-lm(Ni~power.sc+
                       mTt.sc,
                     data=hm.sg.sed)
Ni.site.air.sg<-lm(Ni~airport.sc+
                     mTt.sc,
                   data=hm.sg.sed)
Ni.site.struct.sg<-lm(Ni~struct.sc+
                        mTt.sc,
                      data=hm.sg.sed)
Ni.site.gw.sg<-lm(Ni~blue.sc+
                    dredge.sc+
                    mTt.sc,
                  data=hm.sg.sed)


# Model selection
Ni.cand.mod.names <- c("Ni.site.sg",
                       "Ni.site.dump",
                       "Ni.site.power",
                       "Ni.site.air",
                       "Ni.site.struct",
                       "Ni.site.gw",
                       "Ni.site.dump.sg",
                       "Ni.site.power.sg",
                       "Ni.site.air.sg",
                       "Ni.site.struct.sg",
                       "Ni.site.gw.sg")

Ni.cand.mods <- list()

# This function fills the list by model names
for (i in 1:length(Ni.cand.mod.names)) {
  Ni.cand.mods[[i]] <- get(Ni.cand.mod.names[i])
}

# Function aictab does the AICc-bNied model comparison
(Ni.aic<-data.frame(aictab(
  cand.set = Ni.cand.mods,
  modnames = Ni.cand.mod.names
)))

## Site Pb ----
Pb.site.sg<-lm(Pb~mTt.sc,
               data=hm.sg.sed)
Pb.site.dump<-lm(Pb~dump.dist.sc*dump.area.sc+
                   debris.sc,
                 data=hm.sg.sed)
Pb.site.power<-lm(Pb~power.sc,
                  data=hm.sg.sed)
Pb.site.air<-lm(Pb~airport.sc,
                data=hm.sg.sed)
Pb.site.struct<-lm(Pb~struct.sc,
                   data=hm.sg.sed)
Pb.site.gw<-lm(Pb~blue.sc+dredge.sc,
               data=hm.sg.sed)
Pb.site.dump.sg<-lm(Pb~dump.dist.sc*dump.area.sc+
                      debris.sc+
                      mTt.sc,
                    data=hm.sg.sed)
Pb.site.power.sg<-lm(Pb~power.sc+
                       mTt.sc,
                     data=hm.sg.sed)
Pb.site.air.sg<-lm(Pb~airport.sc+
                     mTt.sc,
                   data=hm.sg.sed)
Pb.site.struct.sg<-lm(Pb~struct.sc+
                        mTt.sc,
                      data=hm.sg.sed)
Pb.site.gw.sg<-lm(Pb~blue.sc+
                    dredge.sc+
                    mTt.sc,
                  data=hm.sg.sed)


# Model selection
Pb.cand.mod.names <- c("Pb.site.sg",
                       "Pb.site.dump",
                       "Pb.site.power",
                       "Pb.site.air",
                       "Pb.site.struct",
                       "Pb.site.gw",
                       "Pb.site.dump.sg",
                       "Pb.site.power.sg",
                       "Pb.site.air.sg",
                       "Pb.site.struct.sg",
                       "Pb.site.gw.sg")

Pb.cand.mods <- list()

# This function fills the list by model names
for (i in 1:length(Pb.cand.mod.names)) {
  Pb.cand.mods[[i]] <- get(Pb.cand.mod.names[i])
}

# Function aictab does the AICc-bPbed model comparison
(Pb.aic<-data.frame(aictab(
  cand.set = Pb.cand.mods,
  modnames = Pb.cand.mod.names
)))

# Site V ----
V.site.sg<-lm(V~mTt.sc,
               data=hm.sg.sed)
V.site.dump<-lm(V~dump.dist.sc*dump.area.sc+
                   debris.sc,
                 data=hm.sg.sed)
V.site.power<-lm(V~power.sc,
                  data=hm.sg.sed)
V.site.air<-lm(V~airport.sc,
                data=hm.sg.sed)
V.site.struct<-lm(V~struct.sc,
                   data=hm.sg.sed)
V.site.gw<-lm(V~blue.sc+dredge.sc,
               data=hm.sg.sed)
V.site.dump.sg<-lm(V~dump.dist.sc*dump.area.sc+
                      debris.sc+
                      mTt.sc,
                    data=hm.sg.sed)
V.site.power.sg<-lm(V~power.sc+
                       mTt.sc,
                     data=hm.sg.sed)
V.site.air.sg<-lm(V~airport.sc+
                     mTt.sc,
                   data=hm.sg.sed)
V.site.struct.sg<-lm(V~struct.sc+
                        mTt.sc,
                      data=hm.sg.sed)
V.site.gw.sg<-lm(V~blue.sc+
                    dredge.sc+
                    mTt.sc,
                  data=hm.sg.sed)


# Model selection
V.cand.mod.names <- c("V.site.sg",
                       "V.site.dump",
                       "V.site.power",
                       "V.site.air",
                       "V.site.struct",
                       "V.site.gw",
                       "V.site.dump.sg",
                       "V.site.power.sg",
                       "V.site.air.sg",
                       "V.site.struct.sg",
                       "V.site.gw.sg")

V.cand.mods <- list()

# This function fills the list by model names
for (i in 1:length(V.cand.mod.names)) {
  V.cand.mods[[i]] <- get(V.cand.mod.names[i])
}

# Function aictab does the AICc-bVed model comparison
(V.aic<-data.frame(aictab(
  cand.set = V.cand.mods,
  modnames = V.cand.mod.names
)))


# Site Zn ----
Zn.site.sg<-lm(Zn~mTt.sc,
               data=hm.sg.sed)
Zn.site.dump<-lm(Zn~dump.dist.sc*dump.area.sc+
                   debris.sc,
                 data=hm.sg.sed)
Zn.site.power<-lm(Zn~power.sc,
                  data=hm.sg.sed)
Zn.site.air<-lm(Zn~airport.sc,
                data=hm.sg.sed)
Zn.site.struct<-lm(Zn~struct.sc,
                   data=hm.sg.sed)
Zn.site.gw<-lm(Zn~blue.sc+dredge.sc,
               data=hm.sg.sed)
Zn.site.dump.sg<-lm(Zn~dump.dist.sc*dump.area.sc+
                      debris.sc+
                      mTt.sc,
                    data=hm.sg.sed)
Zn.site.power.sg<-lm(Zn~power.sc+
                       mTt.sc,
                     data=hm.sg.sed)
Zn.site.air.sg<-lm(Zn~airport.sc+
                     mTt.sc,
                   data=hm.sg.sed)
Zn.site.struct.sg<-lm(Zn~struct.sc+
                        mTt.sc,
                      data=hm.sg.sed)
Zn.site.gw.sg<-lm(Zn~blue.sc+
                    dredge.sc+
                    mTt.sc,
                  data=hm.sg.sed)


# Model selection
Zn.cand.mod.names <- c("Zn.site.sg",
                       "Zn.site.dump",
                       "Zn.site.power",
                       "Zn.site.air",
                       "Zn.site.struct",
                       "Zn.site.gw",
                       "Zn.site.dump.sg",
                       "Zn.site.power.sg",
                       "Zn.site.air.sg",
                       "Zn.site.struct.sg",
                       "Zn.site.gw.sg")

Zn.cand.mods <- list()

# This function fills the list by model names
for (i in 1:length(Zn.cand.mod.names)) {
  Zn.cand.mods[[i]] <- get(Zn.cand.mod.names[i])
}

# Function aictab does the AICc-bZned model comparison
(Zn.aic<-data.frame(aictab(
  cand.set = Zn.cand.mods,
  modnames = Zn.cand.mod.names
)))


# examine best fit models
#best site as----
plot(as.site.dump)
summary(as.site.dump)

#best site cr----
plot(Cr.site.sg)
summary(as.site.sg)

#best site cu----
plot(Cu.site.dump.sg)
summary(Cu.site.dump.sg)

#best site Ni ----
plot(Ni.site.dump)
summary(Ni.site.dump)


#best site Pb----
plot(Pb.site.gw)
summary(Pb.site.gw)

#best site v----
plot(V.site.dump)
summary(V.site.dump)

#best site Zn----
plot(Zn.site.power)
summary(Zn.site.power)

## within site SG and HM relationships
