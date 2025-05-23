#Export all CO2 and CH4 concentrations (ppm) to RESTORE4Cs dropbox


#This script loads all Licor-derived concentrations in TF_Results_ppm folder, filters for injections from cores, and saves all data into Restore4Cs folder for cores.


#Clean WD
rm(list=ls())


#Packages
library(tidyverse)
library(readxl)
library(ggpmisc)

#Directories
folder_root <- "C:/Users/Miguel/Dropbox/Licor_cores_UVEG/"
folder_resuts<- paste0(folder_root,"TF_Results_ppm/")

folder_export<- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/Cores/UVEG_concentrations/"


##---1. Import & format----
#Import N2O
N2Oppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_N2O", recursive = T, full.names = T)

for(i in N2Oppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==N2Oppmfiles[1]){n2o<- a}else {n2o<- rbind(n2o,a)}
  if(i==N2Oppmfiles[length(N2Oppmfiles)]){ rm(i,a,N2Oppmfiles)}
}


#Import CO2
CO2ppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_CO2", recursive = T, full.names = T)

for(i in CO2ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CO2ppmfiles[1]){co2<- a}else {co2<- rbind(co2,a)}
  if(i==CO2ppmfiles[length(CO2ppmfiles)]){ rm(i,a,CO2ppmfiles)}
}


#Import CH4
CH4ppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_CH4", recursive = T, full.names = T)

for(i in CH4ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4ppmfiles[1]){ch4<- a}else {ch4<- rbind(ch4,a)}
  if(i==CH4ppmfiles[length(CH4ppmfiles)]){ rm(i,a,CH4ppmfiles)}
}



#Check: same peaks for both gasses?
unique(co2$peak_id%in%ch4$peak_id)
unique(ch4$peak_id%in%co2$peak_id)



#Format to join (create column gas and rename ppm)
# n2o<- n2o %>% rename(ppm=N2O_ppm) %>% mutate(gas="n2o")
co2<- co2 %>% rename(ppm=co2_ppm) %>% mutate(gas="co2")
ch4<- ch4 %>% rename(ppm=ch4_ppm) %>% mutate(gas="ch4")


#Join datasets
all<- rbind(co2, ch4)
rm(co2,ch4)

#Filter for core injections only
cores<- all %>% 
  filter(grepl("^S",sample)) %>% # keep only R4Cs samples
  filter(grepl("i|f", sample)) %>%  # keep only cores (t0 ends in "i", tf ends in "f")
  separate(peak_id, into=c("sample","ml_text","peak_num"),sep = "_", remove = F) %>% 
  mutate(remark=paste0(sample,"_",ml_text)) %>% 
  filter(!is.na(ml_injected)) %>% 
  filter(ml_injected<1) %>% 
  select(-c(ml_text,peak_num))

#Check what other sample  codes are in "all"
notcores<- all %>% filter(!sample%in%cores$sample)




##---2. Inspect & clean----

#Here we perform the selection of injections based on their deviation and on the inspection plots, we will keep the average peakbaseline value when no peak is detected.

#Peaks will be visually inspected to determine whether we use them as peaks ( (peaksum/ml_injected*factor) + peakbase) or we keep the value of the baseline (one of peakbase_ppm, nopeakbase_avg_ppm,remark_avg_ppm). 



test<- cores %>% 
  mutate(season=substr(sample,1,2)) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm))


test %>% 
  ggplot(aes(x=cv_ppm, fill=gas)) +
  geom_histogram()+
  facet_grid(season~gas, scales="free")


test %>% 
  select(season, sample, gas, cv_ppm) %>%
  distinct() %>% 
  group_by(gas, season) %>%
  mutate(lowvar=cv_ppm<0.05) %>% 
  summarise(good=sum(lowvar,na.rm = T),
            total=n(),
            percent=good/total*100) %>% 
  select(season, gas, percent, good, total) %>% 
  arrange(season,gas)
  

#CH4 inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  mutate(sampling=substr(sample, 1,5)) %>% 
  filter(gas=="ch4") %>% 
  filter(sampling=="S3-VA") %>% 
  filter(!peak_id%in%ch4_peakout) %>% 
  filter(!sample%in%c(ch4_samplesinspected,ch4_customprocess,ch4_samples4peakbase)) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.05) %>%
  arrange(desc(cv_ppm)) %>% 
  ggplot(aes(x=factor(round(cv_ppm,3)), y=ppm, col=(peakSNR>3)))+
  geom_point()+
  geom_label(aes(label=peak_id))


ch4_peakout<- c("S1-CA-R2-2f_0.5_1","S1-CA-R2-3f_0.5_1","S1-CA-A1-3f_0.5_2","S1-CA-A1-3f_0.5_4","S1-CA-A1-4f_0.5_1","S1-CA-P2-6f_0.5_1","S1-CA-R1-3f_0.5_1",
                "S1-CU-R2-2f_0.5_1","S1-CU-R1-1f_0.5_5","S1-CU-P1-2f_0.5_2","S1-CU-P2-6f_0.5_1","S1-CU-R1-5f_0.5_1",
                "S1-DA-R1-6f_0.5_1","S1-DA-R1-6f_0.5_2","S1-DA-P1-3f_0.5_2","S1-DA-P2-2f_0.5_1","S1-DA-R1-1f_0.5_1","S1-DA-R1-1f_0.5_2","S1-DA-P2-4f_0.5_1","S1-DA-A2-4f_0.5_1","S1-DA-R2-2f_0.5_1","S1-DA-R2-6f_0.5_1","S1-DA-A1-5f_0.5_1","S1-DA-A1-2f_0.5_4","S1-DA-A2-5f_0.5_1",
                "S1-DU-A2-1f_0.5_1","S1-DU-A1-5f_0.5_1","S1-DU-P1-5f_0.5_3",
                "S1-RI-P1-5f_0.5_1","S1-RI-P1-6f_0.5_1",
                "S1-VA-A2-5f_0.5_1","S1-VA-P1-1f_0.5_1","S1-VA-P2-3f_0.5_2","S1-VA-P2-1f_0.5_1",
                "S2-CA-A1-5f_0.5_1","S2-CA-R2-3f_0.5_3","S2-CA-A2-1f_0.5_1","S2-CA-R1-2f_0.5_1","S2-CA-P2-1f_0.5_1",
                "S2-CU-A2-3f_0.5_1","S2-CU-A2-5f_0.5_1","S2-CU-A2-6f_0.5_1","S2-CU-A2-4f_0.5_1","S2-CU-A1-6f_0.5_1","S2-CU-A1-1f_0.5_1","S2-CU-R1-3f_0.5_1","S2-CU-A1-4f_0.5_1","S2-CU-P2-5f_0.5_1","S2-CU-P2-1f_0.5_1","S2-CU-R1-6f_0.5_3","S2-CU-R2-1f_0.5_1","S2-CU-P2-2f_0.5_1","S2-CU-R1-5f_0.5_3","S2-CU-A2-1f_0.5_1","S2-CU-P2-3f_0.5_1",
                "S2-DA-R1-5f_0.5_1","S2-DA-R1-3f_0.5_1","S2-DA-R1-6f_0.5_1","S2-DA-P2-3f_0.5_1","S2-DA-P2-4f_0.5_1","S2-DA-P2-1f_0.5_1","S2-DA-A1-6f_0.5_3","S2-DA-A1-3f_0.5_3","S2-DA-A1-4f_0.5_1","S2-DA-A1-4f_0.5_2","S2-DA-P2-6f_0.5_2","S2-DA-R1-1f_0.5_2","S2-DA-A1-1f_0.5_3","S2-DA-P2-2f_0.5_1","S2-DA-P1-3f_0.5_1",
                "S2-DU-R2-6f_0.5_1","S2-DU-R2-4f_0.5_1","S2-DU-R1-1f_0.5_2","S2-DU-P1-5f_0.5_1","S2-DU-A2-1f_0.5_1","S2-DU-A2-6f_0.5_1",
                "S2-RI-A1-5f_0.5_1","S2-RI-A1-6f_0.5_1","S2-RI-P1-1f_0.5_4","S2-RI-P1-4f_0.5_4",
                "S2-VA-R1-1f_0.5_1","S2-VA-R1-5f_0.5_1","S2-VA-R1-4f_0.5_1","S2-VA-R1-3f_0.5_1","S2-VA-P2-6f_0.5_1",
                "S3-CA-R2-5f_0.5_2",
                "S3-DA-P2-1f_0.5_1","S3-DA-P2-1f_0.5_2","S3-DA-P2-1f_0.5_3","S3-DA-A1-1f_0.5_1","S3-DA-A1-1f_0.5_2","S3-DA-A1-1f_0.5_3","S3-DA-A1-6f_0.5_1","S3-DA-A1-6f_0.5_2","S3-DA-P2-2f_0.5_1","S3-DA-P2-2f_0.5_2","S3-DA-A2-2f_0.5_1","S3-DA-A2-2f_0.5_2","S3-DA-P1-1f_0.5_1","S3-DA-P2-3f_0.5_1","S3-DA-A1-5f_0.5_1","S3-DA-P1-2f_0.5_1",
                "S3-DU-R1-1f_0.5_2","S3-DU-A2-6f_0.5_1","S3-DU-P2-5f_0.5_1","S3-DU-A1-6f_0.5_1",
                "S3-RI-P1-1f_0.5_1","S3-RI-P1-1f_0.5_2","S3-RI-R2-6f_0.5_3","S3-RI-P1-5f_0.5_3","S3-RI-A1-2f_0.5_4","S3-RI-R2-2f_0.5_1","S3-RI-R2-2f_0.5_2","S3-RI-R2-5f_0.5_1",
                "S3-VA-A2-2f_0.5_2","S3-VA-R1-2f_0.5_3","S3-VA-P1-3f_0.5_1","S3-VA-P1-3f_0.5_2","S3-VA-R1-3f_0.5_1","S3-VA-R1-6f_0.5_1","S3-VA-A2-1f_0.5_1","S3-VA-A2-1f_0.5_2","S3-VA-A1-2f_0.5_1","S3-VA-A1-2f_0.5_2","S3-VA-A1-2f_0.5_3")#peaks that cannot be used (for anything)

ch4_samples4peakbase<- c("S1-CA-A1-2f","S1-CA-P2-6f","S1-DU-P1-1f","S1-DU-P1-2f","S1-DU-P2-1f","S1-DU-P1-4f","S1-DU-A1-6f","S1-DU-A1-5f","S1-DU-P1-3f","S1-DU-P1-5f","S1-DU-A1-3f","S1-DU-P1-6f","S1-DU-A1-2f","S1-VA-P1-1f","S1-VA-P1-3f","S1-VA-P1-5f","S2-CA-P2-2f","S2-DU-A1-2f","S2-DU-R1-2f","S2-DU-A1-4f","S2-DU-A1-5f","S2-DU-P1-1f","S2-DU-A1-6f","S2-DU-A2-4f","S2-DU-R1-6f","S2-DU-A1-3f","S2-DU-A2-1f","S2-DU-R1-3f","S2-DU-A2-3f","S2-DU-P1-6f","S2-DU-P1-4f","S2-DU-A2-2f","S2-RI-A1-5f","S2-RI-P1-3f","S2-RI-P1-2f","S2-RI-A1-2f","S3-DU-A2-3f","S3-RI-R2-3f","S3-RI-R2-1f","S1-DU-A1-4f") #Samples for which we will take the average basepeak of the non-outlier peaks

ch4_samples4remarkbase<- c("S1-CA-R1-3f") #Samples for which we will take the average of the whole remark as representative of sample

ch4_customprocess<- c("S1-DA-P1-1f","S1-DA-A1-3f") #Samples for custom process: without any assigned peak (nothing detected in remark for co2 or ch4), with only 1 valid peak for peakbase and not good-enough remarkbaseline. To decide and process.

ch4_samplesinspected<- c()#non-important, only to avoid clogging the graph with samples slightly bad (i.e. cv good but larger than 0.05)
ch4_samplinginspected<- c("S1-CA","S1-CU","S1-DA","S1-DU","S1-RI","S1-VA",
                          "S2-CA","S2-CU","S2-DA","S2-DU","S2-RI","S2-VA",
                          "S3-CA","S3-DA","S3-DU","S3-RI","S3-VA")

#No samples S3-CU CH4, all rest of injections ch4 are inspected and cleaned. 




#CO2 inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  mutate(sampling=substr(sample, 1,5)) %>% 
  filter(gas=="co2") %>% 
  filter(sampling=="S1-DA") %>% 
  filter(!peak_id%in%co2_peakout) %>% 
  # filter(!sample%in%c(co2_samplesinspected,co2_samples4peakbase,co2_samples4remarkbase)) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm),
         n_ppm=sum(!is.na(ppm))) %>% 
  filter(cv_ppm>0.05) %>%
  filter(n_ppm>2) %>% 
  # filter(cv_ppm>0.5) %>%
  arrange((cv_ppm)) %>% 
  ggplot(aes(x=factor(round(cv_ppm,3)), y=ppm, col=(peakSNR>3)))+
  geom_point()+
  geom_label(aes(label=peak_id))


co2_peakout<- c("S1-CA-R2-3f_0.5_3","S1-CA-P1-4f_0.5_1","S1-CA-P1-4f_0.5_2","S1-CA-A1-4f_0.5_2","S1-CA-P1-6f_0.5_3","S1-CA-A1-5f_0.5_3","S1-CA-A1-5f_0.5_1","S1-CA-R1-4f_0.5_1","S1-CA-P1-3f_0.5_1","S1-CA-A2-1f_0.5_1","S1-CA-R2-6f_0.5_3","S1-CA-R2-6f_0.5_2","S1-CA-R1-7f_0.5_3","S1-CA-R2-5f_0.5_2","S1-CA-P2-5f_0.5_1","S1-CA-P1-2f_0.5_2","S1-CA-P2-4f_0.5_1","S1-CA-A2-6f_0.5_2","S1-CA-R2-1f_0.5_3","S1-CA-P2-3f_0.5_2","S1-CA-A2-5f_0.5_1","S1-CA-A1-3f_0.5_1","S1-CA-A1-3f_0.5_2","S1-CA-A2-3f_0.5_2","S1-CA-P2-6f_0.5_2","S1-CA-R2-2f_0.5_3","S1-CA-R1-2f_0.5_1","S1-CA-A2-2f_0.5_3","S1-CA-R1-5f_0.5_2","S1-CA-P1-1f_0.5_2",
                "S1-CU-P1-3f_0.5_3","S1-CU-R1-1f_0.5_1","S1-CU-R1-1f_0.5_5","S1-CU-A2-2f_0.5_1","S1-CU-P2-5f_0.5_1","S1-CU-R1-5f_0.5_3","S1-CU-R2-5f_0.5_2","S1-CU-P2-2f_0.5_1","S1-CU-P2-1f_0.5_2","S1-CU-R2-1f_0.5_1","S1-CU-P2-6f_0.5_2","S1-CU-P2-4f_0.5_2","S1-CU-P2-3f_0.5_2",
                "S1-DA-R1-4f_0.5_2","S1-DA-R1-5f_0.5_2","S1-DA-R1-5f_0.5_3","S1-DA-P1-3f_0.5_2","S1-DA-P1-6f_0.5_1","S1-DA-R1-6f_0.5_1","S1-DA-R1-6f_0.5_3","S1-DA-R1-6f_0.5_5","S1-DA-R1-1f_0.5_4","S1-DA-R1-1f_0.5_3","S1-DA-R2-5f_0.5_1","S1-DA-R2-5f_0.5_2","S1-DA-P2-4f_0.5_1","S1-DA-R2-6f_0.5_1","S1-DA-R2-4f_0.5_1","S1-DA-R1-2f_0.5_2",
                "S1-DU-A1-6f_0.5_2","S1-DU-A1-6f_0.5_3","S1-DU-A1-1f_0.5_1","S1-DU-R2-2f_0.5_1","S1-DU-P1-2f_0.5_2","S1-DU-R2-4f_0.5_1","S1-DU-R2-6f_0.5_3","S1-DU-R2-1f_0.5_2","S1-DU-R2-3f_0.5_3","S1-DU-R1-1f_0.5_1","S1-DU-A1-2f_0.5_1","S1-DU-P1-5f_0.5_3",
                "S1-RI-P1-5f_0.5_1","S1-RI-P1-2f_0.5_1","S1-RI-A1-6f_0.5_3","S1-RI-P1-2f_0.5_1","S1-RI-P1-2f_0.5_3","S1-RI-R2-5f_0.5_1","S1-RI-R2-5f_0.5_2","S1-RI-P1-6f_0.5_3","S1-RI-A2-1f_0.5_3","S1-RI-P2-5f_0.5_2","S1-RI-A2-5f_0.5_1","S1-RI-P1-4f_0.5_2","S1-RI-R1-3f_0.5_3","S1-RI-R2-6f_0.5_1","S1-RI-A1-5f_0.5_3","S1-RI-R1-4f_0.5_1","S1-RI-R1-2f_0.5_3","S1-RI-R2-3f_0.5_3",
                "S1-VA-R1-1f_0.5_3","S1-VA-R2-5f_0.5_1","S1-VA-R1-4f_0.5_3","S1-VA-A1-6f_0.5_3","S1-VA-R2-3f_0.5_2","S1-VA-R1-3f_0.5_1","S1-VA-A2-3f_0.5_3","S1-VA-A2-5f_0.5_3","S1-VA-A2-1f_0.5_2","S1-VA-R2-6f_0.5_1","S1-VA-A2-4f_0.5_3","S1-VA-A1-4f_0.5_2","S1-VA-A1-5f_0.5_1","S1-VA-P1-2f_0.5_2",
                "S2-CA-A1-5f_0.5_1","S2-CA-R2-1f_0.5_1","S2-CA-P1-2f_0.5_2","S2-CA-P1-3f_0.5_1","S2-CA-P1-3f_0.5_3","S2-CA-P1-6f_0.5_1","S2-CA-P1-4f_0.5_2","S2-CA-A2-3f_0.5_3","S2-CA-R2-4f_0.5_1","S2-CA-R2-4f_0.5_3","S2-CA-P1-1f_0.5_1","S2-CA-A2-4f_0.5_2","S2-CA-A2-1f_0.5_3","S2-CA-A2-5f_0.5_1","S2-CA-A2-2f_0.5_1","S2-CA-A2-6f_0.5_3","S2-CA-R2-6f_0.5_3","S2-CA-R2-1f_0.5_3","S2-CA-R2-3f_0.5_3","S2-CA-A1-6f_0.5_1",
                "S2-CU-P1-2f_0.5_3","S2-CU-A2-2f_0.5_2","S2-CU-P2-5f_0.5_1","S2-CU-P1-1f_0.5_3","S2-CU-A1-5f_0.5_3","S2-CU-P1-5f_0.5_1","S2-CU-A1-2f_0.5_1","S2-CU-P1-6f_0.5_1","S2-CU-P2-3f_0.5_1","S2-CU-P1-3f_0.5_3","S2-CU-A1-3f_0.5_1","S2-CU-P1-4f_0.5_1","S2-CU-A1-6f_0.5_2","S2-CU-R1-4f_0.5_2","S2-CU-R2-2f_0.5_3","S2-CU-R2-5f_0.5_3","S2-CU-R1-5f_0.5_3","S2-CU-R1-6f_0.5_3","S2-CU-A2-5f_0.5_1",
                "S2-DA-R1-1f_0.5_3","S2-DA-R1-1f_0.5_1","S2-DA-P1-1f_0.5_2","S2-DA-A2-3f_0.5_2","S2-DA-A2-3f_0.5_4","S2-DA-R1-5f_0.5_1","S2-DA-R1-2f_0.5_2","S2-DA-R1-6f_0.5_2","S2-DA-A2-5f_0.5_2","S2-DA-P1-2f_0.5_1","S2-DA-P1-6f_0.5_1","S2-DA-A1-2f_0.5_2","S2-DA-A1-4f_0.5_4","S2-DA-P2-6f_0.5_2",
                "S2-DU-R1-5f_0.5_2","S2-DU-R1-5f_0.5_3","S2-DU-R1-4f_0.5_1","S2-DU-A1-4f_0.5_1","S2-DU-A1-4f_0.5_2","S2-DU-A1-3f_0.5_5","S2-DU-A1-3f_0.5_4","S2-DU-A2-4f_0.5_3","S2-DU-A1-5f_0.5_1","S2-DU-R2-2f_0.5_1","S2-DU-R2-1f_0.5_1","S2-DU-A2-2f_0.5_2","S2-DU-R2-3f_0.5_1","S2-DU-A2-1f_0.5_3","S2-DU-P1-3f_0.5_1","S2-DU-P2-6f_0.5_1","S2-DU-R2-4f_0.5_1",
                "S2-RI-R1-2f_0.5_2","S2-RI-R1-2f_0.5_3","S2-RI-A1-6f_0.5_2","S2-RI-A1-5f_0.5_1","S2-RI-R1-6f_0.5_1","S2-RI-R2-6f_0.5_5","S2-RI-P1-1f_0.5_1","S2-RI-R2-3f_0.5_2","S2-RI-R2-3f_0.5_5","S2-RI-R2-1f_0.5_3","S2-RI-P1-3f_0.5_1","S2-RI-R2-2f_0.5_3","S2-RI-R2-2f_0.5_1","S2-RI-P2-4f_0.5_3","S2-RI-P1-2f_0.5_3","S2-RI-A2-5f_0.5_1","S2-RI-R2-3f_0.5_1","S2-RI-A2-4f_0.5_1","S2-RI-P1-4f_0.5_3","S2-RI-P1-2f_0.5_4","S2-RI-A2-6f_0.5_2","S2-RI-P1-5f_0.5_3","S2-RI-R2-5f_0.5_2","S2-RI-A1-1f_0.5_1",
                "S3-DU-A1-1f_0.5_1","S3-DU-A1-1f_0.5_2","S3-DU-A1-1f_0.5_5","S3-DU-A2-1f_0.5_1","S3-DU-A2-1f_0.5_3","S3-DU-A1-4f_0.5_1","S3-DU-A2-6f_0.5_2","S3-DU-A2-6f_0.5_3","S3-DU-R2-3f_0.5_1","S3-DU-P2-1f_0.5_1","S3-DU-P2-4f_0.5_1","S3-DU-A1-5f_0.5_2","S3-DU-R2-4f_0.5_3","S3-DU-P1-2f_0.5_2","S3-DU-A2-3f_0.5_2","S3-DU-A1-6f_0.5_1","S3-DU-P1-3f_0.5_2","S3-DU-A2-5f_0.5_3","S3-DU-R1-6f_0.5_2","S3-DU-R1-4f_0.5_1","S3-DU-P1-5f_0.5_3","S3-DU-A1-2f_0.5_2","S3-DU-R1-1f_0.5_2")#peaks that cannot be used (for anything)

co2_samples4peakbase<- c() #Samples for which we will take the average basepeak of the non-outlier peaks

co2_samples4remarkbase<- c("S1-CA-A1-2f","S1-CA-A1-6f","S1-CA-A1-1f",
                           "S1-CU-P1-2f","S1-CU-A2-4f","S1-CU-R1-3f","S1-CU-R2-4f","S1-CU-R2-6f","S1-CU-R2-2f","S1-CU-A2-1f","S1-CU-A2-6f","S1-CU-R1-2f","S1-DA-P1-2f","S1-DA-P1-3f","S1-DA-R1-6f",
                           "S1-RI-P2-3f","S1-RI-P2-2f",
                           "S1-VA-P2-3f","S1-VA-R2-4f","S1-VA-P2-4f","S1-VA-A1-1f","S1-VA-A1-2f","S1-VA-A1-3f","S1-VA-P2-2f","S1-VA-A2-6f","S1-VA-R2-1f","S1-VA-R2-2f","S1-VA-P2-5f","S1-VA-P2-1f",
                           "S2-CU-P2-1f","S2-CU-P2-2f","S2-CU-P2-4f","S2-CU-R2-1f",
                           "S2-DA-A2-6f","S2-DA-A2-4f")#Samples for which we will take the average of the whole remark

co2_customprocess<- c("S1-DA-P1-1f","S1-DA-A1-3f") #Samples for custom process: without any assigned peak (nothing detected in remark for co2 or ch4), with only 1 valid peak for peakbase and not good-enough remarkbaseline. To decide and process.

co2_samplesinspected<- c("S3-DU-R2-5f","S3-DU-R2-1f","S3-DU-R2-2f","S3-DU-R2-3f","S2-DU-P1-1f","S2-DU-A1-3f","S2-DU-A2-3f")#non-important, only to avoid clogging the graph with samples slightly bad (i.e. cv good but larger than 0.05)
co2_samplinginspected<- c("S1-CA",#Extremely noisy, not sure about outliers/keepers
                          "S1-CU",#Extremely noisy, not sure about outliers/keepers for some
                          "S1-DA", #More peaks clearer than S1-CA & S1-CU
                          "S1-DU", #Super clear peaks and outliers
                          "S1-RI", #Super clear peaks and outliers
                          "S1-VA", #Very noisy, unclear for many samples
                          "S2-CA", #Clear peaks 
                          "S2-CU", #clear peaks
                          "S2-DA", #Clear peaks
                          "S2-DU", #Clear peaks
                          "S2-RI", #Clear peaks
                          "S3-DU" #Clear peaks
                          ) 


##3. UB-UVEG Check####

#As we did not have calibration data for UVEG licor, we used the UB calibration based on repeated standard injections. We adapted manually the calibration factors to minimize the discrepancy in ppm between samples analyzed with UB method and UVEG method. 

#Best UVEG injections: samples with detected peaks (SNR >2) without outlier peaks or weird remarks
uveg_best<- cores %>% 
  #Remove outlier peaks and samples without good peaks for CH4
  filter(!(peak_id%in%ch4_peakout&gas=="ch4")) %>% 
  filter(!(sample%in%c(ch4_samples4peakbase,ch4_customprocess)&gas=="ch4")) %>% 
  #Remove outlier peaks and samples without good peaks for CO2
  filter(!(peak_id%in%co2_peakout&gas=="co2")) %>% 
  filter(!(sample%in%c(co2_customprocess,co2_samples4peakbase, co2_samples4remarkbase)&gas=="co2")) %>% 
  filter(peakSNR>2) %>% 
  select(sample,gas,dayofanalysis,ppm) %>% 
  group_by(sample, gas,dayofanalysis) %>% 
  summarise(avg_ppm=mean(ppm, na.rm=T),
            sd_ppm= sd(ppm, na.rm=T),
            cv_ppm=abs(sd_ppm/avg_ppm),
            n_injections=sum(!is.na(ppm))) %>% 
  ungroup() %>% 
  filter(n_injections>=2) %>% 
  filter(cv_ppm<0.1) %>% 
  mutate(samplegas=paste(sample, gas, sep = "_")) %>% 
  select(samplegas, gas, avg_ppm, sd_ppm,cv_ppm, n_injections) %>% 
  filter(!grepl("^S1",samplegas))


#Load UB-data
ub<- read.csv(file = "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/Cores/UB_concentrations/N2O_CO2_CH4_ppm_exetainer_avg_sd_n.csv")


ub_match<- ub %>% 
  mutate(samplegas=paste(sample, gas, sep = "_")) %>% 
  filter(samplegas%in%uveg_best$samplegas)%>% 
  select(samplegas, gas, avg_ppm, sd_ppm,cv_ppm, n_injections) %>% 
  rename(avg_ppmub=avg_ppm, sd_ppmub=sd_ppm, cv_ppmub=cv_ppm, n_injectionsub=n_injections)


#Join data from both methods and log-transform
compare<- uveg_best %>% 
  left_join(ub_match, by=c("samplegas","gas")) %>% 
  filter(!(avg_ppmub>600&gas=="ch4")) %>%
  mutate(log_ppmub=log(avg_ppmub), log_ppmuveg=log(avg_ppm),
         percentreldif=(avg_ppm-avg_ppmub)/avg_ppmub*100) 

#Further filtering based on relative difference between the two methods. 80% central distribution
thresholds<- compare %>% 
  group_by(gas) %>% 
  summarise(lower= quantile(percentreldif, 0.90, na.rm=T), 
            upper= quantile(percentreldif, 0.1, na.rm=T))

compare_good<- compare %>% 
  left_join(thresholds, by="gas") %>% 
  mutate(excluded=if_else(between(percentreldif, upper,lower),F,T))

#How many samples to evaluate the cross-calibration?
compare_good %>% 
  filter(excluded==F) %>% 
  group_by(gas) %>% 
  summarise(n=sum(!is.na(avg_ppmub)))


#Comparison in ppm
compare_good %>% 
  ggplot( aes(x=avg_ppmub, y= avg_ppm))+
  geom_point(aes(col=excluded))+
  geom_abline(slope = 1,intercept = 0)+
  geom_smooth(data=. %>% filter(excluded==F),method = "lm")+
  stat_poly_eq(data=. %>% filter(excluded==F),
               formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))+
  facet_wrap(~gas, scales="free")

#Comparson in log-transformed ppm
compare_good %>% 
  ggplot( aes(x=log_ppmub, y= log_ppmuveg))+
  geom_point(aes(col=excluded))+
  geom_abline(slope = 1,intercept = 0)+
  geom_smooth(data=. %>% filter(excluded==F),method = "lm")+
  stat_poly_eq(data=. %>% filter(excluded==F),
               formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))+
  facet_wrap(~gas, scales="free")


#Relative difference between methods
ggplot(compare_good, aes(x=gas, y= (avg_ppm-avg_ppmub)/avg_ppmub))+
  geom_boxplot(data=. %>% filter(excluded==F))+
  ggtitle("Relative difference between methods")

ggplot(compare_good, aes(x=gas, y= (avg_ppm-avg_ppmub)))+
  geom_boxplot(data=. %>% filter(excluded==F))+
  ggtitle("Absolute difference between methods")+
  facet_wrap(~gas, scales="free")


ggplot(compare_good, aes(x=log_ppmub, y= (avg_ppm-avg_ppmub)/avg_ppmub*100))+
  geom_point(aes(col=excluded))+
  scale_y_continuous(name="Relative difference (%)", breaks = seq(-100,100,by=10))+
  ggtitle("Relative difference between methods")+
  facet_wrap(~gas, scales="free")



#CO2 moldel between the two methods
#Fit linear model on log-transformed values
co2difmodel<- lm(log_ppmuveg~log_ppmub,data = compare_good %>% filter(gas=="co2"&excluded==F))

# predict in the range of observations and get average overestimation value (aproximation of intercept, in this case mean overestimation of uveg method)
range_co2_logppm<-compare_good %>% filter(gas=="co2"&excluded==F) %>% 
  summarise(max_log_ppmub=max(log_ppmub,na.rm=T),
            min_log_ppmub=min(log_ppmub,na.rm=T))

#Observe the overestimation from UVEG method based on the log-transformed regression
overestimation_co2<- data.frame(log_ppmub=seq(range_co2_logppm$min_log_ppmub,
                                              range_co2_logppm$max_log_ppmub, by=0.05)) %>% 
  mutate(predicted=predict(co2difmodel,newdata = .)) %>% 
  mutate(predicted_ppm=exp(predicted),
         observed_ppm=exp(log_ppmub),
         overestimate_ppm=predicted_ppm-observed_ppm)

#CO2 method difference with the adjusted calibration slope for UVEG: i.e. this represent the deviation of the log-log regression line from the 1:1 line.
ggplot(overestimation_co2, aes(x=predicted_ppm, y=overestimate_ppm/predicted_ppm*100))+
  geom_point()+
  scale_y_continuous(name="Relative method difference (% CO2)")+
  ggtitle("Modeled overestimation over the range measured")


#CH4 between the two methods
#Fit linear model on log-transformed values
ch4difmodel<- lm(log_ppmuveg~log_ppmub,data = compare_good %>% filter(gas=="ch4"&excluded==F))

# predict in the range of observations and get average overestimation value (aproximation of intercept, in this case mean overestimation of uveg method)
range_ch4_logppm<-compare_good %>% filter(gas=="ch4"&excluded==F) %>% 
  summarise(max_log_ppmub=max(log_ppmub,na.rm=T),
            min_log_ppmub=min(log_ppmub,na.rm=T))

#Observe the overestimation from UVEG method based on the log-transformed regression
overestimation_ch4<- data.frame(log_ppmub=seq(range_ch4_logppm$min_log_ppmub,
                                              range_ch4_logppm$max_log_ppmub, by=0.05)) %>% 
  mutate(predicted=predict(ch4difmodel,newdata = .)) %>% 
  mutate(predicted_ppm=exp(predicted),
         observed_ppm=exp(log_ppmub),
         overestimate_ppm=predicted_ppm-observed_ppm)

#CH4 is underestimated for low-values (-6.5% maximum underestimation with respect to UB injections) This is ok-ish
ggplot(overestimation_ch4, aes(x=predicted_ppm, y=overestimate_ppm/predicted_ppm*100))+
  geom_point()+
  scale_y_continuous(name="Relative method difference (% CH4)", limits = c(-5,50))+
  ggtitle("Relative CH4 overestimation over the range measured")




#Adjusted slopes are appropiate to approximate UVEG values for S1 measured with Licor and for Mixes. 

#Summary of comparison:
compare_good %>% 
  filter(excluded==F) %>% 
  group_by(gas) %>% 
  mutate(ppmdif=avg_ppm-avg_ppmub,
         ppmreldif=ppmdif/avg_ppmub*100) %>% 
  summarise(avg_percentreldif=mean(ppmreldif, na.rm=T),
            sd_percentreldif=sd(ppmreldif, na.rm=T),
            n_reldif=sum(!is.na(ppmreldif)),
            se_percentreldif=sd_percentreldif/sqrt(n_reldif),
            avg_ppmdif=mean(ppmdif, na.rm=T))

compare_good %>% 
  filter(excluded==F) %>% 
  mutate(ppmdif=avg_ppm-avg_ppmub) %>% 
  ggplot(aes(y=ppmdif))+
  geom_histogram()+
  facet_wrap(~gas, scales="free")


compare_good %>% 
  filter(excluded==F) %>% 
  ggplot( aes(x=avg_ppmub, y= avg_ppm))+
  geom_point()+
  geom_abline(slope = 1,intercept = 0)+
  geom_smooth(method = "lm")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))+
  facet_wrap(~gas, scales="free")


rm(compare, ub,ub_match, uveg_best, ch4difmodel,co2difmodel,overestimation_ch4, overestimation_co2, range_ch4_logppm, range_co2_logppm)




##---4. Export cleaned data ----

#Subset the UVEG data for S1 to keep good peaks only, 

#Calculate average concentrations for samples without good peaks (avg_peakbase or avg_remarkbase)
#Join concentrations avg, deviation (SE/SD? ), N_injections and origin of calculated concentration (peaks vs baseline). 

#Samples for which we trust the peak-derived concentration
cores_peaks<- cores %>% 
  #Remove peak-outliers and samples without reliable peaks
  #CH4
  filter(!(peak_id%in%ch4_peakout&gas=="ch4")) %>% 
  filter(!(sample%in%c(ch4_samples4peakbase,ch4_customprocess)&gas=="ch4")) %>% 
  #CO2
  filter(!(peak_id%in%co2_peakout&gas=="co2")) %>% 
  filter(!(sample%in%c(co2_customprocess,co2_samples4peakbase, co2_samples4remarkbase)&gas=="co2")) %>% 
  group_by(gas, sample) %>% 
  summarise(avg_ppm=mean(ppm, na.rm=T),
            sd_ppm= sd(ppm, na.rm=T),
            cv_ppm=abs(sd_ppm/avg_ppm),
            n_injections=sum(!is.na(ppm)),
            n_base=NA_real_,
            estimate="peak-integration")

#Samples for which we trust the peak-base as representative of sample concentration
cores_peakbase<-cores %>% 
  filter((sample%in%ch4_samples4peakbase&gas=="ch4")|(sample%in%c(co2_samples4peakbase)&gas=="co2")) %>% 
  group_by(gas,sample) %>% 
  summarise(avg_ppm=mean(peakbase_ppm, na.rm=T),
            sd_ppm= sd(peakbase_ppm, na.rm=T),
            cv_ppm=abs(sd_ppm/avg_ppm),
            n_injections=NA_real_,
            n_base=sum(!is.na(peakbase_ppm)),
            estimate="average baseline") 

#Samples for which we do not trust peak-derived concentrations, we think average remark concentration is more representative.
cores_averageremark<- cores %>% 
  filter((sample%in%ch4_samples4remarkbase&gas=="ch4")|(sample%in%c(co2_samples4remarkbase)&gas=="co2")) %>% 
  group_by(gas,sample) %>%
  summarise(avg_ppm=mean(remark_avg_ppm, na.rm=T),
            sd_ppm= mean(remark_sd_ppm, na.rm=T),
            cv_ppm=abs(sd_ppm/avg_ppm), 
            n_injections=NA_real_,
            n_base=mean(remark_n, na.rm=T),
            estimate="average baseline")
           

#Join all estimates
allcores<- rbind(cores_peaks, cores_peakbase, cores_averageremark)


# TF cores S1
s1cores<- allcores %>% 
  filter(grepl("^S1", sample))


# Add custom process results: (excell in parent UVEG cores folder), add results from samples which necessitated custom process: 
# S1-DA-P1-1f
# S1-DA-A1-3f
# S1-CU-A1-1f
# S1-CU-A1-2f
# S1-CU-A1-3f
# S1-CU-A1-4f
# S1-CU-A1-5f
# S1-CU-A1-6f

custom_results<-  read_xlsx(path = paste0(folder_root,"TF_cores_custom_process.xlsx"),sheet = "summary",na = "NA") %>% 
  select(gas,sample, avg_ppm, sd_ppm, cv_ppm, n_injections,n_base, estimate)


s1cores_all<- rbind(s1cores, custom_results) %>% arrange(gas,sample)

#Check taht all samples have only 1 estimate for CH4 and CO2
s1cores_all %>% 
  group_by(gas,sample) %>% 
  summarise(n_sample=n()) %>% 
  filter(n_sample!=1)



#Complete dataset (estimate from injections, baselines, peak_bases)
write.csv(s1cores_all, file=paste0(folder_export, "CO2_CH4_ppm_core_avg_sd_n.csv"), row.names = F)


#Miscelanea
#Inspect variability for cores
#CO2
s1cores_all %>% 
  filter(gas=="co2") %>% 
  # filter(grepl("i",sample)) %>% 
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  # filter(site=="DA") %>% 
  ggplot(aes(x=time, y=avg_ppm,col=season))+
  geom_point()+
  facet_grid(site~subsite, scales="free")+
  ggtitle("CO2 (ppm)")

#CH4
s1cores_all %>% 
  filter(gas=="ch4") %>% 
  # filter(grepl("i",sample)) %>% 
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  # filter(site=="DA") %>% 
  ggplot(aes(x=time, y=avg_ppm,col=season))+
  geom_point()+
  facet_grid(site~subsite, scales="free")+
  ggtitle("CH4 (ppm)")


