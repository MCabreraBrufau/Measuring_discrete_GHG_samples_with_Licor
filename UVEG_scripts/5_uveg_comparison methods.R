#Compare results of CH4 and CO2 from S2-S3 between UVEG and UB methods

#THIS SCRIPT IS NOT UPDATED, the UB-UVEG complarison is only in uveg_summary_cores script,



#We have to addapt the script to take per-core summaries (already cleaned) to compare average ppm results from UVEG injections and UB injections. 


#This script loads all Licor-derived concentrations in Results_ppm folder, filters for injections from cores, and saves all data into Restore4Cs folder for cores.


#Clean WD
rm(list=ls())


#Packages
library(tidyverse)
library(readxl)


#Directories
folder_data<- "C:/Users/Miguel/Dropbox/Licor_N2O/"
folder_resuts<- paste0(folder_data,"Results_ppm/")
# folder_samplelist<- paste0(folder_data, "Samplelist/")

folder_export<- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/Cores/UB_concentrations/"


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


#CHECK MISSMATCHES between CO2 and CH4: detect integration errors for CO2
print(co2[!co2$peak_id%in%ch4$peak_id,],n=50)
#S4-CA-R2-27 CH4 too high, co2 peak detection failed. Headspace sample most likely, 2025-02-05
#S4-CU-A1-3 CH4 too high and noisy baseline, co2 peak detection failed. Headspace sample most likely, 2025-02-06
#S4-DA-A2-6f CH4 too high, co2 peak detection failed. 2025-02-20
#S4-DA-R1-5f CH4 too high, co2 peak detection failed. 2025-02-20


#Format to join (create column gas and rename ppm)
n2o<- n2o %>% rename(ppm=N2O_ppm) %>% mutate(gas="n2o")
co2<- co2 %>% rename(ppm=CO2_ppm) %>% mutate(gas="co2")
ch4<- ch4 %>% rename(ppm=CH4_ppm) %>% mutate(gas="ch4")


#Join datasets
all<- rbind(n2o,co2, ch4)
rm(n2o,co2,ch4)


#Filter for core injections only
cores<- all %>% 
  filter(grepl("^S",sample)) %>% # keep only R4Cs samples
  filter(grepl("i|f", sample)) %>%  # keep only cores (t0 ends in "i", tf ends in "f")
  separate(peak_id, into=c("sample","ml_text","peak_num"),sep = "_", remove = F) %>% 
  mutate(remark=paste0(sample,"_",ml_text)) %>% 
  select(-c(ml_text,peak_num))


##---2. Inspect & clean----

test<- cores %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm))


test %>% 
  ggplot(aes(x=cv_ppm, fill=gas)) +
  geom_histogram()+
  facet_wrap(~gas, scales="free")

test %>% 
  ggplot(aes(x=dayofanalysis, y=cv_ppm))+
  geom_point()+
  scale_y_continuous(limits = c(0,0.2))+
  facet_wrap(~gas, scales="free")


#N2O inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  filter(gas=="n2o") %>% 
  # filter(dayofanalysis=="2025-02-07") %>% 
  filter(!peak_id%in%n2o_peakout) %>% 
  filter(!dayofanalysis%in%n2o_daysinspected) %>%
  filter(!remark%in%n2o_negative_remarks) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.05) %>%
  ggplot(aes(x=sample, y=ppm, col=factor(dayofanalysis)))+
  geom_point()+
  geom_label(aes(label=peak_id))

#N2O data already inspected (per day of injection)
n2o_peakout<- c("S2-CU-A2-2f_0.1_1","S2-CU-A2-2f_0.1_3","S2-CU-A2-2f_0.1_5", "S2-CU-A1-5f_0.8_1", #2025-02-11
                "S3-CU-R1-2f_0.4_1","S3-CU-A1-6f_0.8_3","S3-CU-P1-1f_0.8_2","S4-DU-A2-2f_0.8_2","S4-DU-A2-3f_0.8_2", #2025-02-07
                "S3-DU-A1-1f_0.8_1","S3-DU-A2-5f_0.8_1",#2025-02-10
                "S2-CA-A2-4f_0.8_1","S2-CA-R2-5f_0.8_2","S2-DA-P2-1i_1_2",# 2025-02-12
                #2025-02-13 N2O all good
                "S2-RI-R1-5i_1_2",#2025-02-14
                #2025-02-17 all good
                #2025-02-18 all good
                "S3-VA-R1-2f_0.8_1",#2025-02-19
                "S4-CA-A2-4f_0.2_1","S4-CA-A2-4f_0.6_2","S4-CA-A2-4f_0.4_1","S4-DA-A2-5f_0.1_2","S4-DA-A2-5f_0.1_3","S4-DA-A2-5f_0.2_1","S4-DA-R1-3f_0.4_1","S4-DA-R1-3f_0.2_3","S4-DA-R1-4f_0.8_1","S4-DA-R1-4f_0.2_1","S4-DA-R1-5f_0.4_1","S4-DA-R1-5f_0.2_1","S4-DA-R1-5f_0.8_1","S4-DA-R1-6f_0.4_1",#2025-02-20
                "S4-CA-P2-2f_0.8_1","S4-VA-P1-1i_1_2",#2025-02-21
                "S4-CU-A2-5i_1_3","S4-RI-P1-4f_0.8_1","S4-RI-P2-3i_1_2")#2025-02-24
#2025-02-26 good

n2o_negative_remarks<- c("S4-DA-A2-6f_0.8","S4-DA-A2-6f_0.6","S4-DA-A2-6f_0.4","S4-DA-A2-6f_0.2","S4-DA-A2-6f_0.1")

n2o_daysinspected<- c("2025-02-11","2025-02-10","2025-02-07","2025-02-12","2025-02-13","2025-02-14","2025-02-17","2025-02-18","2025-02-19","2025-02-20","2025-02-21","2025-02-24","2025-02-26")




#CH4 inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  filter(gas=="ch4") %>% 
  filter(!peak_id%in%ch4_peakout) %>% 
  filter(!dayofanalysis%in%ch4_daysinspected) %>% 
  # filter(!sample%in%c("S4-DA-A2-1i","S4-DA-A2-1f","S4-DA-A2-3f","S4-DA-A2-4f","S4-DA-P2-1i","S4-DA-A2-2f","S4-DA-R1-5f")) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.05) %>%
  arrange(desc(cv_ppm)) %>% 
  ggplot(aes(x=sample, y=ppm, col=factor(dayofanalysis)))+
  geom_point()+
  geom_label(aes(label=peak_id))

#CH4 data already inspected (per day of injection)
ch4_peakout<- c("S2-CU-A1-5f_0.8_1",#2025-02-11
                "S3-CU-A1-6f_0.8_3","S3-CU-A2-5f_0.8_1","S3-CU-P1-1f_0.8_2","S3-CU-R1-3f_0.8_1","S3-CU-R1-4f_0.8_1","S4-DU-A2-2f_0.8_2","S4-DU-A2-6f_0.8_4",  #2025-02-07
                "S3-DU-A1-1f_0.8_1","S3-DU-A2-5f_0.8_1",#2025-02-10
                "S2-CA-A1-5f_0.8_1","S2-DA-P2-1i_1_2",# 2025-02-12
                "S2-DA-A1-1i_1_3","S2-DU-A1-6f_0.8_1",#2025-02-13
                "S2-RI-R1-5i_1_2",#2025-02-14
                #2025-02-17 all good
                "S3-VA-P1-3f_0.8_1", #2025-02-18
                #2025-02-19 good
                "S4-DA-P2-1i_1_1","S4-DA-R1-3f_0.4_1","S4-DA-R1-3f_0.8_1","S4-CA-A2-1f_0.8_1","S4-DA-R1-6f_0.8_1","S4-DA-R1-6f_0.4_1","S4-DA-R1-1f_0.8_1","S4-DA-R1-1f_0.4_1","S4-CA-A2-5f_0.8_1","S4-DA-R1-4f_0.2_1","S4-DA-R1-4f_0.4_1","S4-CA-A2-4f_0.2_1","S4-CA-A2-4f_0.6_3","S4-CA-A2-4f_0.4_1","S4-DA-R1-2f_0.8_1","S4-DA-P1-1f_0.8_1","S4-DA-A2-6f_0.8_1", "S4-DA-A2-6f_0.4_1","S4-DA-R1-5f_0.6_2","S4-DA-P1-2f_0.8_1","S4-DA-A2-5f_0.2_1","S4-DA-A2-4f_0.8_1",#2025-02-20
                "S4-CA-R2-5f_0.8_1","S4-VA-R1-1f_0.8_4","S4-VA-R1-6f_0.8_4","S4-CA-P2-2f_0.8_1","S4-VA-P1-1i_1_2",#2025-02-21
                "S4-CU-A2-5i_1_3","S4-RI-P1-4f_0.8_1","S4-RI-P2-3i_1_2")#2025-02-24
#2025-02-26 good

ch4_daysinspected<- c("2025-02-11","2025-02-10","2025-02-07","2025-02-12","2025-02-13","2025-02-14", "2025-02-17","2025-02-18","2025-02-19","2025-02-20","2025-02-21","2025-02-24","2025-02-26")





#CO2 inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  filter(gas=="co2") %>%
  filter(!peak_id%in%co2_peakout) %>% 
  filter(!dayofanalysis%in%co2_daysinspected) %>%
  filter(!remark%in%co2_negative_remarks) %>% 
  # filter(!sample%in%c("S4-DA-R1-3f","S4-DA-P1-1f","S4-DA-A2-4f")) %>%
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.05) %>%
  arrange(desc(cv_ppm)) %>% 
  ungroup() %>% 
  # filter(cv_ppm==max(cv_ppm)) %>% 
  # filter(!sample%in%c("S3-DU-A1-5f","S4-DU-P1-1f")) %>% 
  ggplot(aes(x=sample, y=ppm, col=factor(dayofanalysis)))+
  geom_point()+
  geom_label(aes(label=peak_id))



#CO2 data already inspected (per day of injection)
co2_peakout<- c("S2-CU-A1-5f_0.8_1",#2025-02-11
                "S3-DU-A1-1f_0.8_1","S3-DU-A2-5f_0.8_1",#2025-02-10
                "S3-CU-A1-6f_0.8_3","S3-CU-P1-1f_0.8_2","S3-CU-R1-2f_0.4_1","S3-CU-R1-4f_0.8_1","S4-DU-A2-2f_0.8_2","S4-DU-P1-1f_0.8_4",#2025-02-07
                "S2-CA-R1-6f_0.8_3","S2-DA-P2-1i_1_2","S2-DA-R1-5f_0.4_1",# 2025-02-12
                "S2-DU-R2-1i_1_3",#2025-02-13
                "S2-DA-A2-4f_0.8_3","S2-RI-P2-1f_0.8_3", "S2-RI-R1-5i_1_2",#2025-02-14
                "S3-RI-A1-1i_1_3",#2025-02-19
                "S4-DA-A2-4f_0.4_1","S4-CA-A2-1f_0.4_3","S4-CA-A2-5f_0.4_1","S4-DA-R1-3f_0.2_1","S4-DA-R1-3f_0.2_3",#2025-02-20
                "S4-CA-R2-5f_0.8_1","S4-VA-R1-6f_0.8_4","S4-VA-R1-1f_0.8_4","S4-VA-P1-5i_1_3","S4-CA-P2-2f_0.8_1","S4-VA-P1-1i_1_3","S4-VA-P1-3i_1_3","S4-VA-P1-5i_1_4",#2025-02-21
                "S4-RI-P1-4f_0.8_1","S4-CU-A2-5i_1_3","S4-RI-P2-3i_1_2","S4-RI-R2-1i_1_3","S4-RI-R2-5i_1_3")#2025-02-24
#2025-02-26 good

#Exclude remarks with negative co2 peaks
co2_negative_remarks<- c("S4-DA-A2-3f_0.8","S4-DA-A2-4f_0.8","S4-DA-A2-5f_0.8","S4-DA-A2-5f_0.4","S4-DA-A2-5f_0.2","S4-DA-A2-6f_0.8","S4-DA-A2-6f_0.4","S4-DA-A2-6f_0.2","S4-DA-A2-6f_0.1","S4-DA-A2-6f_0.6","S4-DA-P1-1f_0.8","S4-DA-R1-1f_0.8","S4-DA-R1-1f_0.4","S4-DA-R1-2f_0.8","S4-DA-R1-3f_0.8","S4-DA-R1-3f_0.4","S4-DA-R1-4f_0.8","S4-DA-R1-4f_0.4","S4-DA-R1-4f_0.2","S4-DA-R1-4f_0.6","S4-DA-R1-5f_0.8","S4-DA-R1-5f_0.4","S4-DA-R1-5f_0.2","S4-DA-R1-5f_0.6","S4-DA-R1-6f_0.8","S4-DA-R1-6f_0.4","S4-CA-A2-1f_0.8","S4-CA-A2-4f_0.8","S4-CA-A2-4f_0.4","S4-CA-A2-4f_0.2","S4-CA-A2-4f_0.6","S4-CA-A2-5f_0.8")#2025-02-20


#CO2 of days 2025-02-17 and 2025-02-18 have very high deviations, nothing we can do. still, the differences between initial and final seem consistent
co2_daysinspected<- c("2025-02-11","2025-02-10","2025-02-07","2025-02-12","2025-02-13","2025-02-14","2025-02-17","2025-02-18","2025-02-19","2025-02-20","2025-02-21","2025-02-24","2025-02-26")



#Create cores clean with all injections, set outliers to NA, also co2_negative_remakrs mutate(ppm=case_when())
cores_clean_all<- cores %>% 
  mutate(ppm=case_when(gas=="n2o"&peak_id%in%n2o_peakout~NA_real_,
                       gas=="n2o"&remark%in%n2o_negative_remarks~NA_real_,
                       gas=="ch4"&peak_id%in%ch4_peakout~NA_real_,
                       gas=="co2"&peak_id%in%co2_peakout~NA_real_,
                       gas=="co2"&remark%in%co2_negative_remarks~NA_real_,
                       TRUE~ppm)) %>% 
  select(dayofanalysis, sample, gas, peak_id, ppm)

#Create cleaned dataset, summarising injections per sample and gas
cores_clean<- cores_clean_all %>% 
  select(sample,gas,dayofanalysis,ppm) %>% 
  group_by(sample, gas,dayofanalysis) %>% 
  summarise(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm),
         n_injections=sum(!is.na(ppm)))


#Re-Check the cv
cores_clean %>% 
  ggplot(aes(x=cv_ppm, fill=gas)) +
  geom_histogram()+
  facet_wrap(~gas, scales="free")


cores_clean_all %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.10) %>% 
  ggplot(aes(x=sample, y=ppm,col=factor(dayofanalysis)))+
  geom_point()+
  geom_label(aes(label=peak_id))+
  facet_wrap(.~gas, scales="free")



##---3. Export cleaned data ----

#Complete dataset (all injections with NAs for outliers)
write.csv(cores_clean_all, file=paste0(folder_export, "N2O_CO2_CH4_ppm_all_injections.csv"), row.names = F)

#Summary per exetainer (avg,sd,n_injection)
write.csv(cores_clean, file = paste0(folder_export, "N2O_CO2_CH4_ppm_exetainer_avg_sd_n.csv"),row.names = F)




#Inspect t0 variability for cores

#N2O
#Up to 20250211, intial cores are very homogeneous for CU & DU. S2-CA-P1 and S2-CA-P2 have variable t0s (it wouldnt be totally apropiate to do average of initial times, but tf are also very variable and low fluxes)
cores_clean_all %>% 
  filter(gas=="n2o") %>% 
  # filter(grepl("i",sample)) %>% 
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  # filter(site=="DA") %>% 
  ggplot(aes(x=time, y=ppm,col=season))+
  geom_point()+
  facet_grid(site~subsite, scales="free")+
  ggtitle("N2O (ppm)")

#CO2
#Up to 200250211, intial cores very homogeneous, average of t0 cores is appropiate
cores_clean_all %>% 
  filter(gas=="co2") %>% 
  filter(dayofanalysis%in%c("2025-02-17","2025-02-18")) %>% 
  # filter(grepl("i",sample)) %>% 
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  # filter(site=="DU") %>% 
  # filter(season=="S4") %>% 
  ggplot(aes(x=time, y=ppm,col=season))+
  geom_point()+
  # facet_grid(subsite~site, scales="free")
  facet_grid(site~subsite, scales="free")+
  ggtitle("CO2 (ppm)")

#CH4
#Up to 200250211 inspected, initial cores very homogeneous, average of t0 cores is appropriate for all samplings except for: 
#S3-CU-P1: core 3i has ch4 at 3.25ppm, 1i and 5i at 2.4ppm. 
#s2-ca-p2: core 5i has super high methane 45ppm, 1i and 3i at ~4ppm.

#Doing the average of initial cores for the above samplings would impact the fluxes calculated for tf. Decide if we remove high values of T0 (CH4 building in cores with very high flux, so it wont change too much).

cores_clean_all %>% 
  filter(gas=="ch4") %>% 
  # filter(grepl("i",sample)) %>% 
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  mutate(sampling=paste(season,site,subsite,sep = "-")) %>% 
  # filter(site=="DU") %>% 
  # filter(season=="S3") %>%
  # filter(sampling==unique(.$sampling)[27]) %>%
  ggplot(aes(x=time, y=ppm,col=season))+
  geom_point()+
  # scale_y_continuous(limits = c(0,10))+
  # facet_grid(subsite~site, scales="free")
  facet_grid(site~subsite, scales="free")+
  ggtitle("CO2 (ppm)")

#s3-cu-A1, A2, P2, R1, R2 OK
#S4-DU: all subsites OK
#S3-DU: all subsites OK
#S2-CU: P1 p2 a1 a2 r1,r2
#S2-CA: p1




#Inspect 3 gases per subsite 
cores_clean_all %>% 
  # filter(gas=="co2") %>% 
  filter(grepl("S3-CA-R2",sample)) %>%
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  # filter(site=="DU") %>% 
  # filter(season=="S4") %>% 
  ggplot(aes(x=time, y=ppm,col=season))+
  geom_point()+
  # facet_grid(subsite~site, scales="free")
  facet_grid(gas~subsite, scales="free")+
  ggtitle("CO2 (ppm)")



cores_clean%>% 
  filter(gas=="co2") %>% 
  filter(dayofanalysis%in%c("2025-02-17","2025-02-18")) %>% 
  # filter(grepl("i",sample)) %>% 
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  # filter(site=="DU") %>% 
  # filter(season=="S4") %>% 
  ggplot(aes(x=time, y=cv_ppm,col=season))+
  geom_point()+
  # facet_grid(subsite~site, scales="free")
  facet_grid(site~subsite, scales="free")+
  ggtitle("CO2 (ppm)")

cores_clean_all %>% 
  filter(gas=="co2") %>%
  filter(dayofanalysis%in%c("2025-02-17","2025-02-18")) %>% 
  separate(sample, into = c("season","site","subsite","core"), sep = "-",remove = F) %>% 
  mutate(time=case_when(grepl("i",sample)~"inicial",
                        grepl("f",sample)~core)) %>% 
  # filter(site=="DU") %>% 
  # filter(season=="S4") %>% 
  ggplot(aes(x=time, y=ppm,col=season))+
  geom_point()+
  # facet_grid(subsite~site, scales="free")
  facet_grid(site~subsite, scales="free")+
  ggtitle("CO2 (ppm)")

