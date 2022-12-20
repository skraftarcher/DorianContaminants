# first attempt at compositional vector analysis for fish
source("03_scripts/install_packages_function.R")
source("03_scripts/01_pull_google_drive_data-EX.R")
lp("tidyverse")
lp("patchwork")
lp("readxl")
lp("vegan")


# bring in and organize data

hm<-read.csv("01_odata/contam.csv")

hm$Site<-factor(hm$Site,levels=c("Treasure Cay",
                                   "Treasure Cay Marina",
                                   "Hills Creek",
                                   "Jib Room",
                                   "Fish House",
                                   "Rainbow Rentals",
                                   "Boat Harbour",
                                   "Calcutta",
                                   "Camp Abaco",
                                   "Snake Cay",
                                   "BOR",
                                   "Jerry's"))



# select down to a data set that only has contaminant data and one that 
# just has the site info

hm.hm<-hm[,-1:-2]
# pull out site data and add in seagrass bed or marina

hm.env<-hm[,1:2]%>%
  mutate(sg.m=ifelse(Site %in% c("Treasure Cay Marina",
                                  "Jib Room",
                                  "Fish House",
                                  "Rainbow Rentals",
                                 "Boat Harbour",
                                 "Calcutta"),"Marina","Seagrass Bed"))

# do hellinger transformation then RDA
# normally RDA is used for “constrained ordination” (ordination w/covariates or predictor)
# without predictors, RDA is the same as PCA
hm.hel<-decostand(hm.hm,"hellinger")

hm.pca<-rda(hm.hel)

# now extract coordinates for each row in the dataset and join this to environmental data

hm.env2<-bind_cols(hm.env,data.frame(scores(hm.pca,choices=c(1,2),display ="sites")))

# then extract the coordinates for each heavy metal
hm.hm2<-data.frame(scores(hm.pca,choices=c(1,2),display ="species"))%>%
  mutate(hm=row.names(.))

# finally pull out how much variation each axis explain
(hm.var<-round(summary(hm.pca)$cont$importance[2,1:2],3)*100)

theme_set(theme_bw()+theme(panel.grid = element_blank()))

# make plot with all samples displayed
ggplot()+
  geom_hline(aes(yintercept=0),linetype="dashed",alpha=.5)+
  geom_vline(aes(xintercept=0),linetype="dashed",alpha=.5)+
  geom_point(data=hm.hm2,aes(x=PC1,y=PC2),size=3)+
  geom_text(data=hm.hm2,aes(x=PC1,y=PC2,label=hm),nudge_x = .04,nudge_y=.01)+
  geom_point(data=hm.env2,aes(x=PC1,y=PC2,color=Site,shape=sg.m),size=5,alpha=.5)+
  scale_shape_manual(values = c(15,17),name="")+
  #scale_color_viridis_d(option="B",end=.8)
  scale_color_manual(values=c("#7A0403","#891200FF","#B51C01","#440154FF",
                              "#414487FF","#2A788EFF","#7AD151FF","#FDE725FF",
                              "#E8D539","#BCF434","#38F490","#18D9C6"))+
  ylab(paste0("PC2 ( ",hm.var[2],"% )"))+
  xlab(paste0("PC1 ( ",hm.var[1],"% )"))

# make plot with centroid for each site displayed along with 
# error bars in each direction
hm.env3<-hm.env2%>%
  group_by(Site,sg.m)%>%
  summarize(pc1=mean(PC1),
            pc2=mean(PC2),
            pc1.sd=sd(PC1),
            pc2.sd=sd(PC2))

ggplot()+
  geom_hline(aes(yintercept=0),linetype="dashed",alpha=.5)+
  geom_vline(aes(xintercept=0),linetype="dashed",alpha=.5)+
  geom_point(data=hm.hm2,aes(x=PC1,y=PC2),size=3)+
  geom_text(data=hm.hm2,aes(x=PC1,y=PC2,label=hm),nudge_x = .04,nudge_y=.01)+
  geom_point(data=hm.env3,aes(x=pc1,y=pc2,color=Site,shape=sg.m),size=5,alpha=.75)+
  geom_errorbar(data=hm.env3,aes(x=pc1,ymin=pc2-pc2.sd,ymax=pc2+pc2.sd,color=Site))+
  geom_errorbarh(data=hm.env3,aes(y=pc2,xmin=pc1-pc1.sd,xmax=pc1+pc1.sd,color=Site))+
  scale_shape_manual(values = c(15,17),name="")+
  #scale_color_viridis_d(option="B",end=.8)
  scale_color_manual(values=c("#7A0403","#891200FF","#B51C01","#440154FF",
                              "#414487FF","#2A788EFF","#7AD151FF","#FDE725FF",
                              "#E8D539","#BCF434","#38F490","#18D9C6"))+
  ylab(paste0("PC2 ( ",hm.var[2],"% )"))+
  xlab(paste0("PC1 ( ",hm.var[1],"% )"))
