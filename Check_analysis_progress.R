#Progress LiCOR N2O

#This script calculates the progress of analysis of Restore4Cs samples by crossreferencing samplelist from fieldsheets and ppm_sample_N2O...csv files. Aditionally some descriptive statistics.

#Clean WD
rm(list=ls())


#Packages
library(tidyverse)
library(readxl)


#Directories
folder_data<- "C:/Users/Miguel/Dropbox/Licor_N2O/"
folder_resuts<- paste0(folder_data,"Results_ppm/")
folder_samplelist<- paste0(folder_data, "Samplelist/")

#Load S4-samplelist to do:
S4field_toDO<- read_csv(file = paste0(folder_samplelist, "S4field_exetainers.csv"),show_col_types = F)



#Load all samples injected and integrated until now:

  resultppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_N2O", recursive = T, full.names = T)

for(i in resultppmfiles){
  
  a<- read_csv(i,show_col_types = F)
  
  if(i==resultppmfiles[1]){all_n2o<- a}else {all_n2o<- rbind(all_n2o,a)}
}
rm(a,i)

S4field_done<- all_n2o %>% 
  filter(grepl(pattern="^S4-", sample)) %>% 
  filter(!grepl(pattern="i|f", sample)) %>% #avoid including cores here
  group_by(dayofanalysis, sample) %>% 
  summarise(avg_N2Oppm=mean(N2O_ppm, na.rm=T),
            sd_N2Oppm=sd(N2O_ppm, na.rm=T),
            cv_N2Oppm=sd_N2Oppm/avg_N2Oppm,
            n_N2Oppm=sum(!is.na(N2O_ppm)))


#Progress fieldsamples: 
message(paste0(dim(S4field_done)[1], " fieldsamples analysed out of ", dim(S4field_toDO)[1], " fieldsamples taken (",round(dim(S4field_done)[1]/dim(S4field_toDO)[1]*100,2)," % DONE)" ))

message(paste0(dim(S4field_toDO)[1]-dim(S4field_done)[1]), " more fieldsamples to go.")

fieldsamplesperday<-S4field_done %>% group_by(dayofanalysis) %>%  summarise(n=n()) %>% ungroup() %>% summarise(mean(n,na.rm = T)) %>% pull() %>% round(.,2)

message(paste0(round((dim(S4field_toDO)[1]-dim(S4field_done)[1])/fieldsamplesperday), " more days of analysis left at current average rhythm of ",fieldsamplesperday))

message(paste0(round((dim(S4field_toDO)[1]-dim(S4field_done)[1])/50), " more days of analysis left at rhythm of  50 samples per day"))

#CV statistics of method for samples:
message(paste0("Average CV of fieldsamples analysed is ",round(mean(S4field_done$cv_N2Oppm)*100,2)," %"))


#Which samples are missing N2O data?
S4field_missing <-S4field_toDO[!S4field_toDO$exetainer_ID%in%S4field_done$sample,]

#Which N2O data does not match fieldsamples codes?
S4field_missmatch <-S4field_done[!S4field_done$sample%in%S4field_toDO$exetainer_ID,]


#General statistics: most fieldsamples very close to atmospheric composition
ggplot(S4field_done, aes(y=avg_N2Oppm))+
  geom_histogram()+
  annotate(geom="text", x=50, y=0.7, label=paste0("Average: ", round(mean(S4field_done$avg_N2Oppm),3)," ppm N2O"),
           color="red")


#Deviation asa function of dayofanalysis: 
ggplot(S4field_done, aes(x=dayofanalysis, y=cv_N2Oppm, group=dayofanalysis))+
  geom_boxplot()


#Progress cores:

# S2S3S4cores_todo<- data.frame(n=rep(NA, 3*6*6*9)) #3seasons x 6sites x 6subsitesx 9cores(3t0 + 6tf)
S2S3S4cores_todo<- read_xlsx(path = paste0(folder_samplelist, "Samplelist exetainers_RESTORE4Cs_February2025_rformat.xlsx"),sheet = "cores") %>% 
  filter(!grepl("UVEG",box)) %>% #Remove UVEG box (duplicated cores) 
  filter(!grepl("--",remark)) %>% #remove non-existing samples
  filter(!grepl("2i|4i|6i", remark))

#Is there any duplicate exetainer to do?
S2S3S4cores_todo%>%
  select(remark) %>% 
  group_by(remark) %>% 
  summarise(n=n()) %>% 
  filter(n!=1)


S2S3S4cores_done<- all_n2o %>% 
  filter(grepl(pattern="^S", sample)) %>% #keep only R4C samples
  filter(grepl(pattern="i|f", sample)) %>% #keep only cores
  group_by(dayofanalysis, sample) %>% 
  summarise(avg_N2Oppm=mean(N2O_ppm, na.rm=T),
            sd_N2Oppm=sd(N2O_ppm, na.rm=T),
            cv_N2Oppm=sd_N2Oppm/avg_N2Oppm,
            n_N2Oppm=sum(!is.na(N2O_ppm)))

message(paste0(dim(S2S3S4cores_done)[1], " coresamples analysed out of ", dim(S2S3S4cores_todo)[1], " coresamples taken (",round(dim(S2S3S4cores_done)[1]/dim(S2S3S4cores_todo)[1]*100,2)," % DONE)" ))

message(paste0(dim(S2S3S4cores_todo)[1]-dim(S2S3S4cores_done)[1]), " more coresamples to go.")

samplesperday<-S2S3S4cores_done %>% group_by(dayofanalysis) %>%  summarise(n=n()) %>% ungroup() %>% summarise(mean(n,na.rm = T)) %>% pull() %>% round(.,2)

message(paste0(round((dim(S2S3S4cores_todo)[1]-dim(S2S3S4cores_done)[1])/samplesperday,1), " more days of analysis left at current average rhythm of ",samplesperday))


#CV statistics of method for core samples:
message(paste0("Average CV of core samples analysed is ",round(mean(S2S3S4cores_done$cv_N2Oppm, na.rm = T)*100,2)," % for N2O"))


#Which cores samples are missing concentration data?
to_do_cores<-S2S3S4cores_todo[!S2S3S4cores_todo$remark%in%S2S3S4cores_done$sample,]
#Checked, these samples must have been misslabeled or lost. 



#Check CO2 variability: 


resultppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_CO2", recursive = T, full.names = T)

for(i in resultppmfiles){
  
  a<- read_csv(i,show_col_types = F)
  
  if(i==resultppmfiles[1]){all_co2<- a}else {all_co2<- rbind(all_co2,a)}
}
rm(a,i)


sumary_co2<- all_co2 %>% 
  # filter(grepl(pattern="^S4-", sample)) %>% 
  # filter(!grepl(pattern="i|f", sample)) %>% #avoid including cores here
  group_by(dayofanalysis, sample) %>% 
  summarise(avg_CO2ppm=mean(CO2_ppm, na.rm=T),
            sd_CO2ppm=sd(CO2_ppm, na.rm=T),
            cv_CO2=sd_CO2ppm/avg_CO2ppm,
            n_Co2=sum(!is.na(CO2_ppm)))



ggplot(sumary_co2, aes(x=dayofanalysis, y=cv_CO2))+
  geom_point()+
  geom_boxplot(aes(group=dayofanalysis))+
  scale_y_continuous(limits = c(0,0.6))




#Miscelanea quality checks (to re-do propperly adding also air standards to compare to baselines):

all_co2 %>% 
  filter(!grepl("^S4-", sample)) %>% 
  filter(dayofanalysis>=as.POSIXct("2024-11-13")) %>% 
  filter(grepl("^6ppm", sample)) %>% 
  ggplot(aes(x=dayofanalysis, y=(CO2_ppm-3000)/3000*100, group = dayofanalysis))+
  geom_boxplot()+
  scale_y_continuous(name = "Relative deivation from standard (%)")


all_n2o %>% 
  filter(!grepl("^S4-", sample)) %>% 
  filter(dayofanalysis>=as.POSIXct("2024-11-13")) %>% 
  filter(grepl("^6ppm", sample)) %>% 
  ggplot(aes(x=dayofanalysis, y=(N2O_ppm-6)/6*100, group = dayofanalysis))+
  geom_boxplot()+
  scale_y_continuous(name = "Relative deivation from standard (%)")





all_n2o %>% 
  filter(!grepl("^S4-", sample)) %>% 
  filter(dayofanalysis>=as.POSIXct("2024-11-13")) %>% 
  filter(grepl("^p", sample)) %>% 
  mutate(secondseq=abs(parse_number(sample))) %>% 
  ggplot(aes(x=secondseq, y=N2O_ppm, group = sample))+
  geom_point()



all_n2o %>% 
  filter(grepl("^S4-", sample)) %>% 
  filter(dayofanalysis>=as.POSIXct("2025-01-20")) %>% 
  mutate(subsite=substr(sample,1,8)) %>% 
  group_by(sample) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  summarise(n=n())



all_n2o %>% 
  filter(grepl("ptest", sample, ignore.case = T)) %>% 
  filter(dayofanalysis>=as.POSIXct("2025-01-24")) %>% 
  separate(peak_id, into = c("sample","ml","injno"),sep = "_") %>% 
  mutate(seconds=parse_number(str_extract(pattern = "[0-9]{1,2}s",sample))) %>% 
  ggplot(aes(x=seconds, y=N2O_ppm/6*100, colour = factor(injno)))+
  geom_point()+
  geom_abline(intercept = 100,slope = 0)+
  scale_y_continuous(breaks = seq(100,250,by=10))

all_n2o %>% 
  filter(grepl("ptest", sample, ignore.case = T)) %>% 
  filter(dayofanalysis>=as.POSIXct("2025-01-24")) %>% 
  separate(peak_id, into = c("sample","ml","injno"),sep = "_") %>% 
  mutate(seconds=parse_number(str_extract(pattern = "[0-9]{1,2}s",sample))) %>% 
  ggplot(aes(x=injno, y=N2O_ppm, colour = factor(seconds)))+
  geom_point()+
  geom_line(aes(group=sample))



all_n2o %>% 
  filter(grepl("6ppm", sample, ignore.case = T)) %>% 
  # filter(dayofanalysis>=as.POSIXct("2025-01-20")) %>% 
  separate(peak_id, into = c("sample","ml","injno"),sep = "_") %>% 
  ggplot(aes(x=factor(dayofanalysis), y=N2O_ppm/6*100, colour = factor(dayofanalysis>=as.POSIXct("2025-01-20"))))+
  geom_point()+
  geom_abline(intercept = 100,slope = 0)


all_n2o %>% 
  filter(grepl("6ppm", sample, ignore.case = T)) %>% 
  filter(dayofanalysis>=as.POSIXct("2024-11-13")) %>% 
  # filter(dayofanalysis>=as.POSIXct("2025-01-20")) %>% 
  separate(peak_id, into = c("sample","ml","injno"),sep = "_") %>% 
  mutate(after20jan=dayofanalysis>=as.POSIXct("2025-01-20")) %>% 
  ggplot()+
  geom_boxplot(aes(x=dayofanalysis, y=N2O_ppm/6*100, colour = after20jan,group=dayofanalysis))+
  geom_jitter(aes(x=dayofanalysis, y=N2O_ppm/6*100, colour = after20jan))+
  geom_abline(intercept = 100,slope = 0)+
  scale_y_continuous(breaks = seq(80,120,by=5))



#Miscelanea
ri_samplesdone<- all_n2o %>% 
  filter(grepl("RI",sample)) %>% 
  group_by(sample) %>% 
  summarise(avg_ppm=mean(N2O_ppm, na.rm = T))

ri_samplestodo<- S4field_toDO %>% 
  filter(grepl("RI",exetainer_ID))


ri_samplesdone$sample%in%ri_samplestodo$exetainer_ID

ri_samplestodo$exetainer_ID[ri_samplestodo$exetainer_ID%in%ri_samplesdone$sample]
