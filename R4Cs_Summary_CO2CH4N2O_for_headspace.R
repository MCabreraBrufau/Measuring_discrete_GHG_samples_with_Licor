# Summary for headspaces

#This script creates a summary for CO2, CH4 and N2O data from headspaces and atmosphere exetainers to be able to calculate water concentrations. 


#Description----
# ---
# Authors: Miguel Cabrera Brufau
# Project: "RESTORE4Cs"
# date: "September 2025"
#Github: Measure discrete samples with licor 
# ---



#Clean WD
rm(list=ls())


#Packages
library(tidyverse)
library(readxl)


#Directories
folder_data<- "C:/Users/Miguel/Dropbox/Licor_N2O/"
folder_results<- paste0(folder_data,"Results_ppm/")
folder_samplelist<- paste0(folder_data, "Samplelist/")
folder_output<- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/GHG/Headspaces/"



repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}




#---1. Import & format----
#Import N2O
N2Oppmfiles<-list.files(folder_results, pattern = "^ppm_samples_N2O", recursive = T, full.names = T)

for(i in N2Oppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==N2Oppmfiles[1]){n2o<- a}else {n2o<- rbind(n2o,a)}
  if(i==N2Oppmfiles[length(N2Oppmfiles)]){ rm(i,a,N2Oppmfiles)}
}

#Import CO2
CO2ppmfiles<-list.files(folder_results, pattern = "^ppm_samples_CO2", recursive = T, full.names = T)

for(i in CO2ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CO2ppmfiles[1]){co2<- a}else {co2<- rbind(co2,a)}
  if(i==CO2ppmfiles[length(CO2ppmfiles)]){ rm(i,a,CO2ppmfiles)}
}

#Import CH4
CH4ppmfiles<-list.files(folder_results, pattern = "^ppm_samples_CH4", recursive = T, full.names = T)

for(i in CH4ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4ppmfiles[1]){ch4<- a}else {ch4<- rbind(ch4,a)}
  if(i==CH4ppmfiles[length(CH4ppmfiles)]){ rm(i,a,CH4ppmfiles)}
}

#Join datasets
all<- rbind(n2o,co2, ch4)
rm(n2o,co2,ch4)







#Load S4-fieldsheet exetainers:
S4fieldsheets<- read_csv(file = paste0(folder_samplelist, "S4field_exetainers.csv"),show_col_types = F)

s4_atmhs_details<- S4fieldsheets %>% filter(exe_type%in%c("headspace","atmosphere"))

#Filter for Headspaces and atmospheres only (discards chambers, cores and standards)

atmhs<- all %>% 
  filter(sample%in%s4_atmhs_details$exetainer_ID)



#----2. Quality check & filter ----

#Take atm and headspace exetainers only and calculate deviation stats
atmhs<- all %>% 
  mutate(exetainer_ID=sample) %>% 
  left_join(y=S4fieldsheets, by=c("exetainer_ID")) %>% 
  filter(exe_type%in%c("atmosphere","headspace")) %>% #get only atm and hs
  separate(peak_id, into=c("d1","d2","peak_num"),sep = "_",remove = F) %>% 
  select(-c(d1,d2,exetainer_ID)) %>% 
  mutate(peak_id=paste0(gas,"_",peak_id)) %>% 
  mutate(sample_volume=paste(sample,ml_injected, sep = "_")) %>% 
  group_by(sample_volume,gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm=sd(ppm, na.rm=T),
         cv_ppm=(sd_ppm/avg_ppm)*100,
         n_ppm=sum(!is.na(ppm)))


#Check CV distribution
atmhs %>% 
  filter(gas=="co2") %>% 
  select(sample_volume, cv_ppm, n_ppm) %>% 
  distinct() %>% 
  ggplot(aes(x=cv_ppm, fill=factor(n_ppm)))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 50*6)+
  scale_x_continuous(breaks = seq(0,40,by=1))

#Filter obs with cv >7
toflag<- atmhs %>% 
  filter(cv_ppm>7)


#Plot obs large dispersion:
toflag %>% 
  filter(gas=="co2") %>% 
  filter(!sample%in%c("S4-DA-R2-12","S4-CA-A1-16","S4-DA-R2-9","S4-DA-R2-6","S4-CU-A2-21")) %>%
  filter(!peak_id%in%discardpeaks) %>%
  filter(!sample_volume%in%discardsamples) %>%
  group_by(gas,sample) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.05) %>%
  arrange(desc(cv_ppm)) %>% 
  ggplot(aes(x=factor(round(cv_ppm,3)), y=ppm, col=sample))+
  geom_point()+
  geom_label(aes(label=peak_id))+
  facet_wrap(facets=gas~.,scales="free")


#Check lab-notes to decide:
#

discardsamples<- c()
discardpeaks<-c("n2o_S4-CU-P1-15_0.8_2","n2o_S4-RI-P1-15_0.8_2","n2o_S4-RI-P2-12_0.5_1","n2o_S4-DU-R1-7_0.5_3","n2o_S4-CA-A2-23_0.5_1", #N2o peaks to discard
                "co2_S4-CU-A1-3_0.8_2","co2_S4-CU-A1-3_0.8_3","co2_S4-CA-R2-22_0.5_2","co2_S4-DA-A2-3_0.8_3","co2_S4-RI-P1-15_0.8_1", #CO2 peaks to discard
                "co2_S4-CA-R2-27_0.5_1","co2_S4-CA-R2-27_0.5_2","co2_S4-CA-R2-27_0.5_3","co2_S4-CA-R2-27_0.5_4","co2_S4-CA-R2-27_0.5_5","co2_S4-CA-R2-27_0.5_8","co2_S4-CA-R2-27_0.5_9", #Co2 no-peaks from this sample, very low, only 2 real peaks. 
                "ch4_S4-CA-R1-18_0.8_3","ch4_S4-CA-P1-2_0.8_1" #CH4 peaks to discard
                )



#re-calculte summmary after outlier removal
atmhs_good<- atmhs %>% 
  filter(!sample_volume%in%discardsamples) %>% 
  filter(!peak_id%in%discardpeaks) %>% 
  group_by(sample_volume,gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm=sd(ppm, na.rm=T),
         cv_ppm=sd_ppm/avg_ppm*100,
         n_ppm=sum(!is.na(ppm)))

#check cv is good now:
atmhs_good %>% 
  select(sample_volume, cv_ppm, n_ppm,gas) %>% 
  distinct() %>% 
  ggplot(aes(x=cv_ppm, fill=factor(n_ppm)))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 50*6)+
  scale_x_continuous(breaks = seq(0,40,by=1))

rm(atmhs,toflag)


#----3. Summarise for waterGHG----

#Create table with PPM data for waterGHG calculation keep cv and n of samples to calculate significance

atmhs_summary<- atmhs_good %>% 
  rename(exetainer_ID=sample) %>% 
  group_by(gas,exetainer_ID) %>%
  summarise(avg_ppm=mean(ppm, na.rm=T)) %>% 
  pivot_wider(names_from = gas, values_from = avg_ppm,names_prefix = "ppm_")


atmhs_details_with_ppm<- s4_atmhs_details %>% 
  left_join(atmhs_summary, by="exetainer_ID")

#Save summary for GHGwater calculation: 
write.csv(atmhs_details_with_ppm, file = paste0(folder_output, "S4_Headspace&atm_co2_ch4_n2o.csv"),row.names = F)



