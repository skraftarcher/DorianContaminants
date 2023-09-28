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
lp("circular")
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

# find sites where water was measured in both June and December

table(hm.wat$Site,hm.wat$sampling)

# every site but treasure cay and crossing rocks -
# there should be treasure cay (and that should probably be treasure Cay Marina??) in December

# subset down to only sites with sampling in both periods
hm.wat2<-hm.wat%>%
  filter(!Site %in% c("Crossing Rocks","Treasure Cay"))

# look at multivariate view first

hm.wat.env<-hm.wat2[,1:4]%>%
  left_join(sites)
hm.wat.conc<-hm.wat2[,c(5,7,8,9,10,11,12)]

# need to standardize concentrations
hm.wat.conc.std<-decostand(hm.wat.conc,method="hellinger")

# do a mds
hm.wat.rda<-rda(hm.wat.conc.std,data=hm.wat.env)

plot(hm.wat.rda)

#plot this up
hm.wat.scores<-data.frame(scores(hm.wat.rda,c(1,2),"sites"))
hm.wat.env<-bind_cols(hm.wat.env,hm.wat.scores)

ggplot(data=hm.wat.env)+
  geom_point(aes(x=PC1,y=PC2,color=Site,shape=sampling),size=4)

# hard to see anything here- look at centroids and their change over time
hm.wat.env2<-hm.wat.env%>%
  group_by(Site,sampling)%>%
  summarize(PC1.cent=mean(PC1),
            PC2.cent=mean(PC2),
            PC1.cil=mean(PC1)-(1.96*(sd(PC1)/sqrt(n()))),
            PC1.cih=mean(PC1)+(1.96*(sd(PC1)/sqrt(n()))),
            PC2.cil=mean(PC2)-(1.96*(sd(PC2)/sqrt(n()))),
            PC2.cih=mean(PC2)+(1.96*(sd(PC2)/sqrt(n()))))%>%
  pivot_wider(names_from=sampling,values_from=c(PC1.cent,PC2.cent,
                                                PC1.cil,PC1.cih,
                                                PC2.cil,PC2.cih))%>%
  left_join(sites)

sites.wat<-sites%>%
  filter(Site %in% unique(hm.wat.env2$Site))

ggplot(data=hm.wat.env2)+
  geom_errorbar(aes(x=PC1.cent_June2021,
                    ymin=PC2.cil_June2021,
                    ymax=PC2.cih_June2021,
                    color=Site))+
  geom_errorbarh(aes(y=PC2.cent_June2021,
                    xmin=PC1.cil_June2021,
                    xmax=PC1.cih_June2021,
                    color=Site))+
  geom_errorbar(aes(x=PC1.cent_December2021,
                    ymin=PC2.cil_December2021,
                    ymax=PC2.cih_December2021,
                    color=Site))+
  geom_errorbarh(aes(y=PC2.cent_December2021,
                     xmin=PC1.cil_December2021,
                     xmax=PC1.cih_December2021,
                     color=Site))+
  geom_point(aes(x=PC1.cent_June2021,y=PC2.cent_June2021,color=Site))+
  geom_point(aes(x=PC1.cent_December2021,y=PC2.cent_December2021,color=Site))+
  geom_segment(aes(x=PC1.cent_June2021,y=PC2.cent_June2021,
                   xend=PC1.cent_December2021,yend=PC2.cent_December2021,
                   color=Site),
               arrow=arrow(length=unit(0.5,"cm")),show.legend=FALSE)+
  scale_color_manual(values=sites.wat$colr)+
  ylab("PC1")+
  xlab("PC2")+
  facet_wrap(~Site,scales="free")

# look at stats around centroid shifts
hm.wat.env3<-hm.wat.env2%>%
  dplyr::select(Site,is.marsh,is.seagrass,is.marina,
         PC1.cent_June2021,PC1.cent_December2021,
         PC2.cent_June2021,PC2.cent_December2021)%>%
  ungroup()%>%
  mutate(dx=PC1.cent_December2021-PC1.cent_June2021,
         dy=PC2.cent_December2021-PC2.cent_June2021,
         dx=ifelse(abs(dx)<0.0000000001,0,dx),
         dy=ifelse(abs(dy)<0.0000000001,0,dy),
         vlength = sqrt(dx^2 +dy^2),
         vlength.rsc = scales::rescale(vlength,to = c(0,1),from = range(vlength)),
         angle = atan2(dy,dx),
         angle.d=angle*180/pi,
         angle.d=ifelse(angle.d<0,360+angle.d,angle.d),
         plot.start = 0,
         plot.end.x = cos(angle)*vlength.rsc,
         plot.end.y = sin(angle)*vlength.rsc,
         ang.circ=circular(angle,units="radians"),
         ang.circ.d=circular(angle.d,units="degrees"))

circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100) {
  r <- diameter / 2
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circ <- circleFun(center = c(0, 0), diameter = mean(hm.wat.env3$vlength), npoints = 500)

# make single plot
ggplot(data=hm.wat.env3)+
  geom_path(data = circ, aes(x, y), lty = 2, alpha = 0.7)+
  geom_segment(aes(x=plot.start,
                   y=plot.start,
                   xend=dx,
                   yend=dy,
                   color=as.factor(is.seagrass)),
               arrow=arrow(length=unit(0.5,"cm")))+
  scale_color_viridis_d(option="A",begin=.2,end=.8,name="Site Type",labels=c("Marina or Harbour","Seagrass bed"))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.position = c(.1,.9),
        legend.box.margin = margin(2,2,2,2),
        legend.box.background = element_rect(color="black"))+
  coord_equal()

ggsave("04_figures/multivariate_shift_water.png")


# now look at individual metals
hm.wat3<-hm.wat2[,c(1,2,3,4,5,7,8,9,10,11,12)]%>%
  pivot_longer(Cr:V,names_to="hm",values_to="conc")
