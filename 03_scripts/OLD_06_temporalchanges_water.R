# change between june and december in marsh harbour water samples

source("03_scripts/install_packages_function.R")
# source("03_scripts/01_pull_google_drive_data-EX.R") #only run if data has been updated
lp("tidyverse")
lp("readxl")
lp("emmeans")
lp("car")
lp("multcomp")
lp("contrast")
lp("vegan")

# bing in data----
hm.wat<-read.csv("01_odata/water_contaminants.csv")

#organize data----
hm.wat$sampling<-factor(hm.wat$sampling,levels=c("November2019","May2020","June2021","December2021"))

hm.wat$sm<-factor(paste0(hm.wat$Site,hm.wat$month))
levels(hm.wat$sm)
# we care of overall the contaminant went down, and if the pattern of increase/decrease
# is consistent across sites

# set up contrasts
conts<-list(boatharbour=c(1,-1,0,0,0,0,0,0,0,0),
            calcutta=c(0,0,-1,1,0,0,0,0,0,0),
            fish=c(0,0,0,0,1,-1,0,0,0,0),
            jib=c(0,0,0,0,0,0,1,-1,0,0),
            rainbow=c(0,0,0,0,0,0,0,0,1,-1),
            marsh=c(0,0,0,0,1,-1,1,-1,1,-1))

par(mfrow=c(2,2))

#chromium
cr.lm<-lm(Cr~sm,data=hm.wat)
plot(cr.lm)
anova(cr.lm)
cr.emm<-emmeans(cr.lm,"sm")
emmeans::contrast(cr.emm,conts,adjust="sidak")
#chromium went up at boat harbour but didn't change anywhere else

#nickle
ni.lm<-lm(Ni~sm,data=hm.wat)
plot(ni.lm)
anova(ni.lm)
#overall anova isn't significant for nickle

#copper
cu.lm<-lm(Cu~sm,data=hm.wat)
plot(cu.lm)
anova(cu.lm)
cu.emm<-emmeans(cu.lm,"sm")
emmeans::contrast(cu.emm,conts,adjust="sidak")
#copper went down significantly in marsh harbour between June and December
# but up in Calcutta

#zinc
zn.lm<-lm(Zn~sm,data=hm.wat)
plot(zn.lm)
anova(zn.lm)
zn.emm<-emmeans(zn.lm,"sm")
emmeans::contrast(zn.emm,conts,adjust="sidak")
# no significant contrasts 

#arsenic
as.lm<-lm(As~sm,data=hm.wat)
plot(as.lm)
anova(as.lm)
as.emm<-emmeans(as.lm,"sm")
emmeans::contrast(as.emm,conts,adjust="sidak")

#arsenic went up in boat harbour, and down in Marsh Harbour, potentially driven
# by decreases at jib

#lead 
pb.lm<-lm(Pb~sm,data=hm.wat)
plot(pb.lm)
anova(pb.lm)
# no significant changes in lead

#Vanadium
v.lm<-lm(V~sm,data=hm.wat)
plot(v.lm)
anova(v.lm)
# no significant changes in vanadium


# do a multivariate comparison of sites by month
con.env<-hm.wat[,c(1,2,10,11,12)]
con<-hm.wat[,c(3:9)]

con.nmds<-metaMDS(con)

# start here plotting - probably better do this with seagrass beds too but for now...
