# Sample summary


#Clean WD
rm(list=ls())


#Packages
library(tidyverse)
library(readxl)


#Directories
folder_data<- "C:/Users/Miguel/Dropbox/Licor_N2O/"
folder_resuts<- paste0(folder_data,"Results_ppm/")
folder_samplelist<- paste0(folder_data, "Samplelist/")


repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}


#Load all samples injected and integrated until now:

resultppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_N2O", recursive = T, full.names = T)

for(i in resultppmfiles){
  
  a<- read_csv(i,show_col_types = F)
  
  if(i==resultppmfiles[1]){A<- a}else {A<- rbind(A,a)}
  if(i==resultppmfiles[length(resultppmfiles)]){rm(a,i)}
}

#Load S4-fieldsheet exetainers:
S4fieldsheets<- read_csv(file = paste0(folder_samplelist, "S4field_exetainers.csv"),show_col_types = F)



#Take atm and chamber exetainers only and calculate deviation stats
n2o_atmchambers<- A %>% 
  mutate(exetainer_ID=sample) %>% 
  left_join(y=S4fieldsheets, by=c("exetainer_ID")) %>% 
  filter(exe_type%in%c("air trapped","atmosphere")) %>% 
  separate(peak_id, into=c("d1","d2","peak_num"),sep = "_",remove = F) %>% 
  select(-c(d1,d2,exetainer_ID)) %>% 
  mutate(sample_volume=paste(sample,ml_injected, sep = "_")) %>% 
  group_by(sample_volume) %>% 
  mutate(avg_N2Oppm=mean(N2O_ppm, na.rm=T),
         sd_N2Oppm=sd(N2O_ppm, na.rm=T),
         cv_N2Oppm=sd_N2Oppm/avg_N2Oppm*100,
         n_N2Oppm=sum(!is.na(N2O_ppm)))


#Check CV distribution
n2o_atmchambers %>% 
  select(sample_volume, cv_N2Oppm, n_N2Oppm) %>% 
  distinct() %>% 
  ggplot(aes(x=cv_N2Oppm, fill=factor(n_N2Oppm)))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 50*6)+
  scale_x_continuous(breaks = seq(0,40,by=1))

#Filter obs with cv >5
n2o_toflag<- n2o_atmchambers %>% 
  filter(cv_N2Oppm>5)

#Plot obs large dispersion:
n2o_toflag %>% 
  ggplot(aes(x=sample_volume, y=N2O_ppm,col=factor(dayofanalysis)))+
  geom_text(aes(label=peak_num))

n2o_toflag %>% 
  ggplot(aes(x=peak_num, y=N2O_ppm,col=factor(dayofanalysis)))+
  geom_text(aes(label=sample_volume))+
  geom_line(aes(group=sample_volume))

#Check lab-notes to decide:
#20241211 fixed
#20241220 is ok
#20250110 is ok
#20250120: S4-RI-P2-21_1 and S4-RI-P2-22_1 discard completely (syringe not closed properly), discard peak S4-RI-P2-25_1_4
#20250121: is ok
discardsamples<- c("S4-RI-P2-21_1","S4-RI-P2-22_1")
discardpeaks<-c("S4-RI-P2-25_1_4","S4-RI-R1-28_1_3")

#re-calculte N2O after outlier removal
n2o_atmchambers_good<- n2o_atmchambers %>% 
  filter(!sample_volume%in%discardsamples) %>% 
  filter(!peak_id%in%discardpeaks) %>% 
  group_by(sample_volume) %>% 
  mutate(avg_N2Oppm=mean(N2O_ppm, na.rm=T),
         sd_N2Oppm=sd(N2O_ppm, na.rm=T),
         cv_N2Oppm=sd_N2Oppm/avg_N2Oppm*100,
         n_N2Oppm=sum(!is.na(N2O_ppm)))

#check cv is good now:
n2o_atmchambers_good %>% 
  select(sample_volume, cv_N2Oppm, n_N2Oppm) %>% 
  distinct() %>% 
  ggplot(aes(x=cv_N2Oppm, fill=factor(n_N2Oppm)))+
  geom_histogram(position = "identity", alpha = 0.5, bins = 50*6)+
  scale_x_continuous(breaks = seq(0,40,by=1))

rm(n2o_atmchambers,n2o_toflag)


#check atm samples are comparable:
n2o_atmchambers_good %>% 
  filter(exe_type=="atmosphere") %>% 
  ggplot(aes(x=plot_ID, y=N2O_ppm))+
  geom_point()+
  facet_grid(pilot_site~subsite, scales="free")

#few cases where n2o increases significantly throughout the day: CA-R2, CU-P1. 
#For now, use average atm concentration for all flux calculations.

#Check dataset for obvious errors:
A %>% 
  mutate(exetainer_ID=sample) %>% 
  left_join(y=S4fieldsheets, by=c("exetainer_ID")) %>% 
  mutate(sample_volume=paste(sample, ml_injected,sep = "_")) %>% 
  filter(!sample_volume%in%discardsamples) %>% 
  filter(!peak_id%in%discardpeaks) %>% 
  filter(!is.na(pilot_site)) %>% 
  # filter(exe_type!="headspace") %>% 
  ggplot(aes(x=plot_ID, y=N2O_ppm,col=exe_type))+
  geom_point()+
  facet_grid(pilot_site~subsite, scales="free")

#Check potential mistakes in exe_type identity for S4-VA-R2, S4-CA-R1, S4-CA-R2, S4-DA-A2
A %>% 
  mutate(exetainer_ID=sample) %>% 
  left_join(y=S4fieldsheets, by=c("exetainer_ID")) %>% 
  filter(!is.na(pilot_site)) %>% 
  filter(grepl("S4-RI-P2",sample)) %>% 
  ggplot(aes(x=plot_ID, y=N2O_ppm, col=exe_type))+
  geom_point()+
  geom_label(aes(label=peak_id))+
  facet_grid(pilot_site~subsite, scales="free")
#No mistakes found after check for S4-VA-R2,  S4-CA-R1, S4-CA-R2, S4-DA-A2



#End of checks for N2O_ppm 

#to DO: sumarise samples into single avg, select only 1st of T/D chambers, calculate fluxes based on incubation time, "P", T, V, A. 

