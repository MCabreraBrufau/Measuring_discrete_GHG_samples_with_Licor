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
  
  if(i==resultppmfiles[1]){A<- a}else {A<- rbind(A,a)}
}
rm(a,i)

S4field_done<- A %>% 
  filter(grepl(pattern="^S4-", sample)) %>% 
  filter(!grepl(pattern="t|T", sample)) %>% #avoid including cores here
  group_by(dayofanalysis, sample) %>% 
  summarise(avg_N2Oppm=mean(N2O_ppm, na.rm=T),
            sd_N2Oppm=sd(N2O_ppm, na.rm=T),
            cv_N2Oppm=sd_N2Oppm/avg_N2Oppm,
            n_N2Oppm=sum(!is.na(N2O_ppm)))


#Progress fieldsamples: 
message(paste0(dim(S4field_done)[1], " fieldsamples analysed out of ", dim(S4field_toDO)[1], " fieldsamples taken (",round(dim(S4field_done)[1]/dim(S4field_toDO)[1]*100,2)," % DONE)" ))

message(paste0(dim(S4field_toDO)[1]-dim(S4field_done)[1]), " more fieldsamples to go.")

fieldsamplesperday<-S4field_done %>% group_by(dayofanalysis) %>%  summarise(n=n()) %>% ungroup() %>% summarise(mean(n,na.rm = T)) %>% pull() %>% round(.,2)

message(paste0(round((dim(S4field_toDO)[1]-dim(S4field_done)[1])/fieldsamplesperday), " more days of analysis left at current average rithm of ",fieldsamplesperday))

message(paste0(round((dim(S4field_toDO)[1]-dim(S4field_done)[1])/50), " more days of analysis left at rithm of  50 samples per day"))

#CV statistics of method for samples:
message(paste0("Average CV of fieldsamples analysed is ",round(mean(S4field_done$cv_N2Oppm)*100,2)," %"))


#General statistics: most fieldsamples very close to atmospheric composition
ggplot(S4field_done, aes(y=avg_N2Oppm))+
  geom_histogram()+
  annotate(geom="text", x=50, y=0.7, label=paste0("Average: ", round(mean(S4field_done$avg_N2Oppm),3)," ppm N2O"),
           color="red")


#Deviation asa function of dayofanalysis: 
ggplot(S4field_done, aes(x=dayofanalysis, y=cv_N2Oppm, group=dayofanalysis))+
  geom_boxplot()


S4field_done %>% filter(dayofanalysis==as.POSIXct("2025-01-22"))%>% 
                          ggplot(aes(x=sample, y=cv_N2Oppm))+
  geom_point()+
  geom_label(aes(label=sample))


#Progress cores:

S2S3S4cores_todo<- data.frame(n=rep(NA, 6*6*6*3))
S2S3S4cores_done<- data.frame()

message(paste0(dim(S2S3S4cores_done)[1], " coresamples analysed out of ", dim(S2S3S4cores_todo)[1], " coresamples taken (",round(dim(S2S3S4cores_done)[1]/dim(S2S3S4cores_todo)[1]*100,2)," % DONE)" ))

message(paste0(dim(S2S3S4cores_todo)[1]-dim(S2S3S4cores_done)[1]), " more coresamples to go.")

samplesperday<-S2S3S4cores_done %>% group_by(dayofanalysis) %>%  summarise(n=n()) %>% ungroup() %>% summarise(mean(n,na.rm = T)) %>% pull() %>% round(.,2)

message(paste0(round((dim(S2S3S4cores_todo)[1]-dim(S2S3S4cores_done)[1])/samplesperday), " more days of analysis left at current average rithm of ",samplesperday))

message(paste0(round((dim(S2S3S4cores_todo)[1]-dim(S2S3S4cores_done)[1])/50), " more days of analysis left at rithm of  50 samples per day"))



#Miscelanea quality check (to re-do propperly adding also air standards to compare to baselines):

A %>% 
  filter(!grepl("^S4-", sample)) %>% 
  filter(dayofanalysis>=as.POSIXct("2024-11-13")) %>% 
  filter(grepl("^6ppm", sample)) %>% 
  ggplot(aes(x=dayofanalysis, y=N2O_ppm, group = dayofanalysis))+
  geom_boxplot()

