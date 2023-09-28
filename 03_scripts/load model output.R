# script to load model results for manuscript draft
source("03_scripts/install_packages_function.R")

lp("tidyverse")

#water
w.msg.as<-read_rds("02_wdata/water_marinavsg_arsenic.rds")
w.msg.cr<-read_rds("02_wdata/water_marinavsg_chromium.rds")
w.msg.cu<-read_rds("02_wdata/water_marinavsg_copper.rds")
w.msg.ni<-read_rds("02_wdata/water_marinavsg_nickel.rds")
w.msg.pb<-read_rds("02_wdata/water_marinavsg_lead.rds")
w.msg.zn<-read_rds("02_wdata/water_marinavsg_zinc.rds")
w.msg.v<-read_rds("02_wdata/water_marinavsg_vanadium.rds")
w.msg.totalhm<-read_rds("02_wdata/water_marinavsg_totalhm.rds")

#sediment

sed.msg.as<-read_rds("02_wdata/sediment_marinavsg_arsenic.rds")
sed.msg.cr<-read_rds("02_wdata/sediment_marinavsg_chromium.rds")
sed.msg.cu<-read_rds("02_wdata/sediment_marinavsg_copper.rds")
sed.msg.ni<-read_rds("02_wdata/sediment_marinavsg_nickel.rds")
sed.msg.pb<-read_rds("02_wdata/sediment_marinavsg_lead.rds")
sed.msg.zn<-read_rds("02_wdata/sediment_marinavsg_zinc.rds")
sed.msg.v<-read_rds("02_wdata/sediment_marinavsg_vanadium.rds")
sed.msg.totalhm<-read_rds("02_wdata/sediment_marinavsg_totalhm.rds")