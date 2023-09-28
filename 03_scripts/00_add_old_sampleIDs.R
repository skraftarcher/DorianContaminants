# organize seagrass heavy metal data
# some samples are missing IDs so need to link it to sampleID list
library(tidyverse)
hm.sg<-read.csv("01_odata/seagrass_contaminants.csv")
sg.labs<-read.csv("01_odata/old_sample_IDs.csv")

# only need to work with November 2019
# andget rid of site column and split off SG- then join on sg.labs
hm.sg.n19<-filter(hm.sg,sampling=="November2019")%>%
  separate(SampleID,into=c("ex","SampleID"),sep="-")%>%
  select(-Site)%>%
  left_join(sg.labs)%>%
  filter(!is.na(Site))%>%
  select(-ex,-sample.type,-Date)%>%
  rename(sample.type=sample.type.use)

hm.sg.oth<-filter(hm.sg,sampling!="November2019")

hm.sg<-rbind(hm.sg.oth,hm.sg.n19)

# there are some macroalgae samples so pull those out and store separately
hm.alg<-filter(hm.sg,sample.type=="macroalgae")
hm.sg2<-filter(hm.sg,sample.type!="macroalgae")

# save these datasets
write.csv(hm.alg,"02_wdata/macroalgae_contaminants.csv")
write.csv(hm.sg2,"02_wdata/seagrass_contaminants.csv")           
