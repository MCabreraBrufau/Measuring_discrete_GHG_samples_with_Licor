# Summary for flux

#This script creates a summary for N2O data to be able to calculate the chamber fluxes. 



#Clean WD
rm(list=ls())


#Packages
library(tidyverse)
library(readxl)


#Directories
folder_data<- "C:/Users/Miguel/Dropbox/Licor_N2O/"
folder_resuts<- paste0(folder_data,"Results_ppm_newperpeak/")
folder_samplelist<- paste0(folder_data, "Samplelist/")
folder_output<- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/GHG/N2O_fluxes/"



repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}



#----1. Import conc. and meta ----

#Load all samples injected and integrated until now:

resultppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_N2O", recursive = T, full.names = T)

for(i in resultppmfiles){
  
  a<- read_csv(i,show_col_types = F)
  
  if(i==resultppmfiles[1]){A<- a}else {A<- rbind(A,a)}
  if(i==resultppmfiles[length(resultppmfiles)]){rm(a,i)}
}

#Load S4-fieldsheet exetainers:
S4fieldsheets<- read_csv(file = paste0(folder_samplelist, "S4field_exetainers.csv"),show_col_types = F)



#----2. Quality check & filter ----

#Take atm and chamber exetainers only and calculate deviation stats
n2o_atmchambers<- A %>% 
  mutate(exetainer_ID=sample) %>% 
  left_join(y=S4fieldsheets, by=c("exetainer_ID")) %>% 
  filter(grepl("^S",sample)) %>% #get only samples
  filter(!grepl("i|f",sample)) %>% #exclude cores
  filter(!exe_type%in%c("headspace")) %>% #exclude headspaces
  separate(peak_id, into=c("d1","d2","peak_num"),sep = "_",remove = F) %>% 
  select(-c(d1,d2,exetainer_ID)) %>% 
  mutate(sample_volume=paste(sample,ml_injected, sep = "_")) %>% 
  group_by(sample_volume) %>% 
  mutate(avg_N2Oppm=mean(ppm, na.rm=T),
         sd_N2Oppm=sd(ppm, na.rm=T),
         cv_N2Oppm=(sd_N2Oppm/avg_N2Oppm)*100,
         n_N2Oppm=sum(!is.na(ppm)))

#two exetainers ambiguous code but correctly identified to plot. Nothing to fix! its OK.
n2o_atmchambers %>% filter(grepl("a|b",sample)) %>% select(sample, plot_ID, comment) %>% distinct()

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
  filter(!peak_id%in%discardpeaks) %>% 
  filter(!sample_volume%in%discardsamples) %>%
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.05) %>%
  arrange(desc(cv_ppm)) %>% 
  ggplot(aes(x=factor(round(cv_ppm,3)), y=ppm, col=sample))+
  geom_point()+
  geom_label(aes(label=peak_id))

n2o_toflag %>% 
  ggplot(aes(x=sample_volume, y=ppm,col=factor(dayofanalysis)))+
  geom_text(aes(label=peak_num))

n2o_toflag %>% 
  ggplot(aes(x=peak_num, y=ppm,col=factor(dayofanalysis)))+
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
  mutate(avg_N2Oppm=mean(ppm, na.rm=T),
         sd_N2Oppm=sd(ppm, na.rm=T),
         cv_N2Oppm=sd_N2Oppm/avg_N2Oppm*100,
         n_N2Oppm=sum(!is.na(ppm)))

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
  ggplot(aes(x=plot_ID, y=ppm))+
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
  ggplot(aes(x=plot_ID, y=ppm,col=exe_type))+
  geom_point()+
  facet_grid(pilot_site~subsite, scales="free")

#Check potential mistakes in exe_type identity for S4-VA-R2, S4-CA-R1, S4-CA-R2, S4-DA-A2
A %>% 
  mutate(exetainer_ID=sample) %>% 
  left_join(y=S4fieldsheets, by=c("exetainer_ID")) %>% 
  filter(!is.na(pilot_site)) %>% 
  filter(grepl("S4-RI-P2",sample)) %>% 
  ggplot(aes(x=plot_ID, y=ppm, col=exe_type))+
  geom_point()+
  geom_label(aes(label=peak_id))+
  facet_grid(pilot_site~subsite, scales="free")
#No mistakes found after check for S4-VA-R2,  S4-CA-R1, S4-CA-R2, S4-DA-A2



#End of checks for N2O_ppm 


#----3. Summarise for flux----
#to DO: sumarise samples into single avg, select only 1st of T/D chambers, calculate fluxes based on incubation time, "P", T, V, A. 
#Take only 1 average for atm per subsite.
#Do not propagate errors, to calculate LOD for fluxes we will use the cv of method for all + incubation duration


#Create database 1 atm per subsite with cv and n to calculate significance of flux afterwards. 
atm_avg<- n2o_atmchambers_good %>% 
  filter(exe_type=="atmosphere") %>% 
  group_by(subsite_ID) %>% 
  summarise(atm_avg=mean(ppm, na.rm=T),
            atm_cv=sd(ppm, na.rm = T)/atm_avg,
            atm_n=sum(!is.na(ppm)))

#Create table with n2o data for flux calculation keep cv and n of samples to calculate significance
n2o_summary_atmchambers<- n2o_atmchambers_good %>% 
  merge.data.frame(atm_avg, by="subsite_ID",all = T) %>% 
  filter(exe_type=="air trapped") %>% 
  rename(lightcondition=`transparent or dark`, strata=Strata) %>% 
  group_by(campaign,pilot_site, subsite, subsite_ID, plot_ID, strata,lightcondition, comment, sample) %>% 
  summarise(sample_avgN2Oppm=mean(ppm, na.rm=T),
            sample_cv=sd(ppm, na.rm=T)/sample_avgN2Oppm,
            sample_n= sum(!is.na(ppm)), 
            atm_avgN2Oppm=mean(atm_avg, na.rm=T), #keep the atm data by doing the mean
            atm_cv=mean(atm_cv, na.rm=T),#keep the atm data by doing the mean
            atm_n=mean(atm_n,na.rm=T)) %>% #keep the atm data by doing the mean
  separate(sample, into = c("d1","d2","d3","exenum"),sep="-", remove=F) %>% 
  select(-c(d1,d2,d3)) %>% 
  arrange(subsite_ID, plot_ID, exenum) %>% 
  mutate(UniqueID=tolower(paste0(subsite_ID,"-", plot_ID,"-", substr(strata,1,1),"-",substr(lightcondition,1,1)))) %>% 
  #Create UniqueID to match this dataset to the P, V, A, T data.
  ungroup() %>% 
  group_by(subsite_ID, plot_ID) %>% 
  mutate(incubationOrder=row_number()) %>% #add incubationOrder to filter out 2nd incubations in same plot (usually dark incubations after transparent) for which we cannot be sure if the chamber was ventilated or not. 
  ungroup() %>% 
  # mutate(deltaN2O=sample_N2O-atm_N2O) %>% 
  rename(exetainercode=sample, UniqueID_notime=UniqueID, plotincubation=incubationOrder,
         tf_avgN2Oppm=sample_avgN2Oppm, tf_cv=sample_cv, tf_n=sample_n, #Rename for clarity
         atm_avgN2Oppm=atm_avgN2Oppm, atm_cv=atm_cv, atm_n=atm_n) %>% #Rename for clarity
  select(UniqueID_notime, exetainercode, exenum, plotincubation, tf_avgN2Oppm,tf_cv, tf_n,atm_avgN2Oppm, atm_cv,atm_n) 


#Save summary for flux calculation: with UniqueID_notime to join with chamber data. 
write.csv(n2o_summary_atmchambers, file = paste0(folder_output, "S4_restore4cs_N2Oppm_atmchambers.csv"),row.names = F)



