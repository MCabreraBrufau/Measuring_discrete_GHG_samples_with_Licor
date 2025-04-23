#Export TF mix CH4 concentrations (ppm) to RESTORE4Cs dropbox


#This script loads all Licor-derived concentrations in TFmix_Results_ppm folder, filters injections from cores to remove outliers and wrong injections, and saves avg ppm data into Restore4Cs folder for cores.

#This script works with the ppm concentrations
#Clean WD
rm(list=ls())


#Packages
library(tidyverse)
library(readxl)
library(ggpmisc)

#Directories
folder_root <- "C:/Users/Miguel/Dropbox/Licor_cores_UVEG/"
folder_resuts<- paste0(folder_root,"TFmix_Results_ppm/")

folder_export<- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/Cores/UVEG_concentrations/"


##---1. Import & format----

#Import CH4
CH4ppmfiles<-list.files(folder_resuts, pattern = "^ppm_samples_CH4", recursive = T, full.names = T)

for(i in CH4ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4ppmfiles[1]){ch4<- a}else {ch4<- rbind(ch4,a)}
  if(i==CH4ppmfiles[length(CH4ppmfiles)]){ rm(i,a,CH4ppmfiles)}
}




##---2. Inspect & clean----

#We will inspect the relative deviation of ppm between replicate injections and decide if any replicate injection is marked as outlier for removal

test<- ch4 %>% 
  mutate(season=substr(sample,1,2),
         gas="ch4") %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ch4_ppm, na.rm=T),
         sd_ppm= sd(ch4_ppm, na.rm=T),
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
  

###TO ADAPT#####
#Here we need to adapt the selection to discard outliers based on the ppm 


#CH4 inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  mutate(sampling=substr(sample, 1,5)) %>% 
  filter(gas=="ch4") %>% 
  filter(sampling=="S4-DA") %>% 
  filter(!peak_id%in%ch4_peakout) %>%
  filter(!sample%in%c(ch4_samplesinspected)) %>%
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ch4_ppm, na.rm=T),
         sd_ppm= sd(ch4_ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm)) %>% 
  filter(cv_ppm>0.0705) %>%
  arrange(desc(cv_ppm)) %>% 
  ggplot(aes(x=factor(round(cv_ppm,3)), y=ch4_ppm, col=(peakSNR>3)))+
  geom_point()+
  geom_label(aes(label=peak_id))


ch4_peakout<- c("S1-CA-A2-4m_0.5_3","S1-CA-R2-1m_0.5_1","S1-CA-R1-2m_0.5_1","S1-CA-R2-3m_0.5_1","S1-CA-R2-4m_0.5_2","S1-CA-A1-2m_0.5_1","S1-CA-R2-2m_0.5_1",
                "S1-CU-R2-5m_0.5_1","S1-CU-R2-4m_0.5_2","S1-CU-R2-6m_0.5_1","S1-CU-A2-1m_0.5_1","S1-CU-R2-2m_0.5_1","S1-CU-A2-2m_0.5_1",
                "S1-DA-R1-2m_0.5_3","S1-DA-R1-1m_0.5_3","S1-DA-R1-5m_0.5_3","S1-DA-R1-5m_0.5_4","S1-DA-R1-4m_0.5_3","S1-DA-R1-3m_0.5_3","S1-DA-R2-4m_0.5_1","S1-DA-R2-4m_0.5_2",                "S1-DA-R2-1m_0.5_1","S1-DA-R2-1m_0.5_2","S1-DA-R2-5m_0.5_1","S1-DA-R2-2m_0.5_1","S1-DA-R2-2m_0.5_2","S1-DA-R2-6m_0.5_1","S1-DA-R2-6m_0.5_2","S1-DA-A2-5m_0.5_1",
                "S1-VA-A2-1m_0.5_1","S1-VA-P2-3m_0.5_1",
                "S2-CA-A1-3m_0.5_3","S2-CA-R2-1m_0.5_1",
                "S2-CU-P1-1m_0.5_1","S2-CU-P1-1m_0.5_3","S2-CU-P1-4m_0.5_1","S2-CU-P2-5m_0.5_2","S2-CU-A1-2m_0.5_1",
                "S2-DA-A2-1m_0.5_2","S2-DA-A2-1m_0.5_3","S2-DA-P1-2m_0.2_3","S2-DA-A2-3m_0.5_3","S2-DA-A2-3m_0.5_2","S2-DA-P1-3m_0.2_3","S2-DA-R1-2m_0.2_2","S2-DA-R1-5m_0.2_1",
                "S2-VA-P1-5m_0.5_2","S2-VA-A2-3m_0.5_3","S2-VA-R1-1m_0.5_3","S2-VA-R1-3m_0.5_1","S2-VA-A2-6m_0.5_1","S2-VA-P2-1m_0.5_1","S2-VA-P2-3m_0.5_1","S2-VA-P2-2m_0.5_1",
                "S3-CA-A2-5m_0.5_2","S3-CA-A1-3m_0.5_3","S3-CA-A1-5m_0.5_3","S3-CA-A1-6m_0.5_3","S3-CA-A1-6m_0.5_2","S3-CA-R2-5m_0.5_1","S3-CA-P2-1m_0.5_1","S3-CA-P2-1m_0.5_2","S3-CA-P2-6m_0.5_1","S3-CA-P1-1m_0.5_1","S3-CA-R1-3m_0.5_1","S3-CA-R1-2m_0.5_1","S3-CA-R1-2m_0.5_2","S3-CA-P2-3m_0.5_1","S3-CA-P1-6m_0.5_1","S3-CA-P2-4m_0.5_3","S3-CA-P1-2m_0.5_1",
                "S3-DA-A2-1m_0.5_1","S3-DA-P1-5m_0.5_3","S3-DA-R1-4m_0.5_3","S3-DA-R1-4m_0.5_4","S3-DA-R1-3m_0.5_3","S3-DA-P1-3m_0.5_2","S3-DA-P1-2m_0.5_1","S3-DA-P1-1m_0.5_2","S3-DA-A2-4m_0.5_3","S3-DA-A2-2m_0.5_1","S3-DA-A2-2m_0.5_3","S3-DA-R1-2m_0.5_4","S3-DA-P2-6m_0.5_1","S3-DA-P2-6m_0.5_2","S3-DA-P2-6m_0.5_3","S3-DA-P1-6m_0.5_4",
                "S3-RI-A1-5m_0.5_1","S3-RI-A1-5m_0.5_2","S3-RI-A1-6m_0.5_3",
                "S3-VA-A2-6m_0.5_3","S3-VA-A2-3m_0.5_2","S3-VA-A2-4m_0.5_1","S3-VA-A2-2m_0.5_1","S3-VA-R1-6m_0.5_1","S3-VA-R1-2m_0.5_1","S3-VA-R1-3m_0.5_1","S3-VA-P2-1m_0.5_1","S3-VA-P2-6m_0.5_3","S3-VA-P1-5m_0.5_1","S3-VA-P2-3m_0.5_3","S3-VA-A1-3m_0.5_3",
                "S4-CU-R1-2m_0.5_2","S4-CU-R1-2m_0.5_3","S4-CU-R1-1m_0.5_1","S4-CU-R1-6m_0.5_2","S4-CU-A2-3m_0.5_3","S4-CU-A2-3m_0.5_4","S4-CU-R1-5m_0.5_3","S4-CU-P2-2m_0.5_3","S4-CU-R2-2m_0.5_3","S4-CU-R1-4m_0.5_3","S4-CU-R2-3m_0.5_1","S4-CU-A1-1m_0.5_1","S4-CU-A1-2m_0.5_1","S4-CU-P1-3m_0.5_3","S4-CU-P1-2m_0.5_1","S4-CU-P2-4m_0.5_3","S4-CU-P1-4m_0.5_4","S4-CU-P1-6m_0.5_1","S4-CU-P1-6m_0.5_5","S4-CU-R2-1m_0.5_1",
                "S4-DA-R1-1m_0.1_1","S4-DA-R1-1m_0.1_2","S4-DA-R1-2m_0.1_3","S4-DA-R1-2m_0.1_4","S4-DA-R1-2m_0.1_6","S4-DA-R1-3m_0.1_4","S4-DA-R1-3m_0.1_1","S4-DA-A2-6m_0.5_1","S4-DA-A2-6m_0.5_4","S4-DA-R1-5m_0.1_3","S4-DA-R1-5m_0.1_4","S4-DA-R1-6m_0.1_3","S4-DA-A2-5m_0.5_3","S4-DA-P1-4m_0.5_3","S4-DA-P1-4m_0.5_4","S4-DA-P2-4m_0.5_3","S4-DA-P2-4m_0.5_4","S4-DA-A2-2m_0.5_3","S4-DA-P2-6m_0.5_3","S4-DA-P2-6m_0.5_4","S4-DA-P2-6m_0.5_2","S4-DA-P2-3m_0.5_4","S4-DA-P1-6m_0.5_1","S4-DA-P1-1m_0.5_4","S4-DA-P1-1m_0.5_1","S4-DA-P2-5m_0.5_4","S4-DA-A2-1m_0.5_1","S4-DA-A2-4m_0.5_2","S4-DA-P2-1m_0.5_1","S4-DA-P1-2m_0.5_2")#peaks that cannot be used (for anything)


ch4_samplesinspected<- c("S1-CA-R2-6m","S1-CA-A2-4m","S1-CA-R2-3m","S1-CU-R2-3m","S1-CU-A2-2m","S1-CU-R2-1m","S1-CU-R1-1m","S1-DA-R1-4m","S1-DA-R1-1m","S1-DA-P2-3m","S1-DA-R2-4m","S1-DA-A2-3m","S2-CA-A1-3m","S2-CA-A1-2m","S2-CU-P2-3m","S2-CU-P1-6m","S2-CU-P1-4m","S2-DA-R1-4m","S2-DA-R1-3m","S2-DA-R1-1m","S2-VA-A2-3m","S3-CA-A2-6m","S3-CA-A2-4m","S3-DA-R1-5m","S3-VA-A2-5m","S3-VA-A2-6m","S3-VA-R1-4m","S3-VA-A2-1m","S4-CU-A1-4m","S4-CU-P2-4m","S4-CU-R1-1m","S4-DA-R1-4m","S4-DA-R1-2m","S4-DA-R1-5m","S4-DA-A2-5m","S4-DA-R1-3m","S4-DA-P1-4m")#non-important, only to avoid clogging the graph with samples slightly bad (i.e. cv good but larger than 0.05)

ch4_samplinginspected<- c("S1-CA","S1-CU","S1-DA","S1-DU","S1-RI","S1-VA",
                          "S2-CA","S2-CU","S2-DA","S2-DU","S2-RI","S2-VA",
                          "S3-CA","S3-DA","S3-RI","S3-VA",
                          "S4-CU","S4-DA")


ch4_samplings<- c("S1-CA","S1-CU","S1-DA","S1-DU","S1-RI","S1-VA",
                          "S2-CA","S2-CU","S2-DA","S2-DU","S2-RI","S2-VA",
                          "S3-CA","S3-DA","S3-DU","S3-RI","S3-VA")



##---3. Export cleaned data ----

#Remove peak-outliers and samples without reliable peaks
cores_peaks<- ch4 %>% 
  mutate(gas="ch4") %>% 
  filter(!(peak_id%in%ch4_peakout&gas=="ch4")) %>% 
  group_by(gas, sample) %>% 
  summarise(avg_ppm=mean(ch4_ppm, na.rm=T),
            sd_ppm= sd(ch4_ppm, na.rm=T),
            cv_ppm=abs(sd_ppm/avg_ppm),
            n_injections=sum(!is.na(ch4_ppm)),
            estimate="peak-integration")

write.csv(cores_peaks, file=paste0(folder_export, "CH4_ppm_coremix_avg_sd_cv_n.csv"), row.names = F)
