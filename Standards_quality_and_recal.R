#Progress LiCOR N2O

#Author: Miguel Cabrera Brufau
#Date: march 2025

#Description: 
#This script examinates the quality (variability and biass) of the internal standards analyzed alongside the Restore4Cs samples and calculates a corrected calibration factor. 6ppm standards and air standards. 

#0. Packages and directories-----
#Clean WD
rm(list=ls())


#Packages
library(tidyverse)


#Directories
folder_data<- "C:/Users/Miguel/Dropbox/Licor_N2O/"
folder_resuts<- paste0(folder_data,"Results_ppm_newperpeak/")
repo_root <- dirname(rstudioapi::getSourceEditorContext()$path)
folder_cal<- paste0(repo_root, "/Calibration/")





#1. Import data----

##1.1 INJECTIONS------
#Import N2O
N2Oppmfiles<-list.files(folder_resuts, pattern = "^integrated_injections_N2O", recursive = T, full.names = T)

for(i in N2Oppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==N2Oppmfiles[1]){n2o<- a}else {n2o<- rbind(n2o,a)}
  if(i==N2Oppmfiles[length(N2Oppmfiles)]){ rm(i,a,N2Oppmfiles)}
}


#Import CO2
CO2ppmfiles<-list.files(folder_resuts, pattern = "^integrated_injections_CO2", recursive = T, full.names = T)

for(i in CO2ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CO2ppmfiles[1]){co2<- a}else {co2<- rbind(co2,a)}
  if(i==CO2ppmfiles[length(CO2ppmfiles)]){ rm(i,a,CO2ppmfiles)}
}


#Import CH4
CH4ppmfiles<-list.files(folder_resuts, pattern = "^integrated_injections_CH4", recursive = T, full.names = T)

for(i in CH4ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4ppmfiles[1]){ch4<- a}else {ch4<- rbind(ch4,a)}
  if(i==CH4ppmfiles[length(CH4ppmfiles)]){ rm(i,a,CH4ppmfiles)}
}

#Rename & Join integrated injections
n2o<- n2o %>% mutate(gas="n2o")
co2<- co2 %>% mutate(gas="co2")
ch4<- ch4 %>% mutate(gas="ch4")
injections<- rbind(n2o,co2,ch4)
rm(n2o,co2,ch4)

##1.2. BASELINES----

#Import air-baselines

#N2O
N2Obasefiles<-list.files(folder_resuts, pattern = "^baselines_N2O", recursive = T, full.names = T)

for(i in N2Obasefiles){
  a<- read_csv(i,show_col_types = F)
  if(i==N2Obasefiles[1]){n2obase<- a}else {n2obase<- rbind(n2obase,a)}
  if(i==N2Obasefiles[length(N2Obasefiles)]){ rm(i,a,N2Obasefiles)}
}


#Import CO2
CO2basefiles<-list.files(folder_resuts, pattern = "^baselines_CO2", recursive = T, full.names = T)

for(i in CO2basefiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CO2basefiles[1]){co2base<- a}else {co2base<- rbind(co2base,a)}
  if(i==CO2basefiles[length(CO2basefiles)]){ rm(i,a,CO2basefiles)}
}


#Import CH4
CH4basefiles<-list.files(folder_resuts, pattern = "^baselines_CH4", recursive = T, full.names = T)

for(i in CH4basefiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4basefiles[1]){ch4base<- a}else {ch4base<- rbind(ch4base,a)}
  if(i==CH4basefiles[length(CH4basefiles)]){ rm(i,a,CH4basefiles)}
}

#Rename & Join baselines
n2obase<- n2obase %>% mutate(gas="n2o")
co2base<- co2base %>% mutate(gas="co2")
ch4base<- ch4base %>% mutate(gas="ch4")
bases<- rbind(n2obase,co2base,ch4base)
rm(n2obase,co2base,ch4base)


#2. Old calibration-----

#Calculate concentrations for integrated injections using the old calibration curve
#This calibration was based on multiple dilutions and injections from tedlar bag. 

#Load Old calibration
calibration_old <- read_csv(paste0(folder_cal, "Calibration_and_limit_of_detection_2024-12-12.csv"),show_col_types = F) %>% 
  rename(gas=Species) %>% 
  mutate(gas=tolower(gas)) %>% 
  select(gas, Intercept, Slope)

####HERE##-------

#AFTER THE NEW integration run for all, check variable names in integrated_samples to make sure everything works: 

#Add to the calculation the baseline (peaksum ~ (ppm-peak_baseppm)*ml_injected)

#So: ppm = ((peaksum-Intercept)/(Slope*ml_injected))+peak_baseppm

#CHECK HERE if it works well.
#POTENTIALLY use 0 to substitute negative peak_base
#IN any case, transform peak_base into ppm (currently)

#Calculate Concentration ppms 
injections_ppm <- injections %>% 
  left_join(calibration_old, by="gas") %>% 
  separate(peak_id, into = c("sample", "ml_injected","peak_no"), sep = "_",remove = F) %>% 
  mutate(peak_baseppm=peak_base/1000,
         ml_injected=as.numeric(gsub("[^0-9.]", "", ml_injected)),
         ppm = ((peaksum-Intercept)/(Slope*ml_injected))+peak_baseppm) %>% 
         # ppm = ((peaksum-Intercept))/(Slope*ml_injected)) %>% Old way of calibrate
  select(dayofanalysis, gas, sample, ml_injected, peak_id, ppm, peaksum, peak_baseppm, unixtime_ofmax) %>% 
  mutate(datetime=as.POSIXct(unixtime_ofmax))






#3 Filter & Format-----
#Filter for internal standards analyzed with two licors

#Extract dates with all 3 gases
dateswith3gases<-injections %>% select(dayofanalysis, gas) %>% 
  distinct() %>% 
  mutate(true=T) %>% 
  pivot_wider(id_cols=dayofanalysis,names_from = gas,values_from = true) %>% 
  filter(co2,ch4,n2o) %>% pull (dayofanalysis)


#Injections 
inj<- injections_ppm %>% 
  filter(grepl("air|ppm", sample)) %>% 
  filter(dayofanalysis%in%dateswith3gases) %>% 
  mutate(sampletype=case_when(grepl("air",sample)~"air",
                              grepl("ppm",sample)~"standard"),
         known_ppm=case_when(grepl("ppm",sample)&gas=="n2o"~6,
                             grepl("ppm",sample)&gas=="co2"~3000,
                             grepl("ppm",sample)&gas=="ch4"~15.29,
                             TRUE~NA_real_))
  
#baselines air
air_b<- bases %>% 
  filter(grepl("lab-1", label)) %>% 
  filter(base_date%in%dateswith3gases) %>% 
  mutate(known_ppm=base_avg/1000,
         known_ppm_sd=base_sd/1000,
         cv=base_cv*100) %>% 
  rename(dayofanalysis=base_date) %>% 
  select(dayofanalysis, known_ppm,known_ppm_sd,gas) %>% 
  mutate(sampletype="air")



#Join baselines, injections and theoretical concentrations

all_standards<- inj %>% 
  merge.data.frame(air_b, by=c("dayofanalysis","gas", "sampletype","known_ppm"),all = T) %>% 
  select(dayofanalysis, gas, sampletype, known_ppm, ml_injected, ppm, peak_id, known_ppm_sd, peaksum, peak_baseppm) %>% 
  group_by(dayofanalysis,gas,sampletype) %>% 
  mutate(known_ppm=mean(known_ppm,na.rm=T),
         known_ppm_sd=mean(known_ppm_sd,na.rm=T),
         peak_id=paste(dayofanalysis, peak_id, sep="_")) %>% 
  filter(!is.na(ml_injected))



#4. Inspect results-----

#Inspect the missmatch betwween the known concentrations (standard or baseline measures) and those obtained with the old calibration factor. 

#Known vs Injections 
ggplot(all_standards, aes(x=known_ppm, y=ppm))+
  geom_abline(slope = 1,intercept = 0)+
  geom_point()+
  facet_wrap(~gas,scales="free")

#Overestimation per sampletype
ggplot(all_standards, aes(x=gas, y=ppm/known_ppm*100, col=sampletype))+
  geom_violin()+
  geom_boxplot()

#Overestimatiion through time
ggplot(all_standards, aes(x=dayofanalysis, y=ppm/known_ppm*100))+
  geom_boxplot(aes(group=dayofanalysis))+
  geom_point(aes(col=sampletype))+
  geom_smooth(aes(col=sampletype), method="lm",se=T)+
  facet_wrap(~gas)


#Decission: the "air" injections are too variable for CO2 and CH4, most likely due to inconsistencies when filling exetainers during baseline measurement. 

#Old calibration procedure causes a consistent overestimation of samples from exetainers, We will re-calibrate using the standards analyzed throughout the analysis days.

#For re-calibration we will only use the "standard" injections of 1 ml (injections from exetainers filled with the standard mix of known concentration). Take 95% central distribution to calculate the 1-point calibration (intercept=0) following the Li-cor technical note

#Summary of overestimation using standard injections and old calibration curve
all_standards %>% 
  filter(!is.na(ppm*known_ppm)) %>% 
  filter(sampletype=="standard") %>% 
  group_by(gas) %>% 
  mutate(overestimation=ppm/known_ppm,
         lower = quantile(overestimation, 0.025),
         upper = quantile(overestimation, 0.975)
  ) %>% 
  mutate(kept=if_else(between(overestimation, lower, upper),"Good","excluded")) %>% 
  ggplot(aes(x=gas, y=overestimation, col=kept))+
  geom_violin(data=. %>% filter(kept=="Good"))+
  geom_boxplot(data=. %>% filter(kept=="Good"))+
  geom_point(data=. %>% filter(kept=="excluded"))
  

#Summary of over-estimation
all_standards %>% 
  # filter(sampletype=="standard") %>% 
  filter(!is.na(ppm*known_ppm)) %>% 
  group_by(gas, sampletype) %>% 
  mutate(overestimation=ppm/known_ppm,
         lower = quantile(overestimation, 0.025),
         upper = quantile(overestimation, 0.975)
         ) %>% 
  filter(between(overestimation, lower, upper)) %>% 
  summarise(avg_overest=mean(overestimation),
            sd_overest=sd(overestimation), 
            n_overest=sum(!is.na(overestimation)),
            se_overest=sd_overest/sqrt(n_overest))



#5. RE-CALIBRATION-------


#AFTER MUCH THINKING and testing, we realize that the baseline must be included always in the calibration and in the re-calculation of concentrations. THis is because the peak-area is not proportional to the concentration of sample BUT to the difference in concentration between sample and baseline. 

####CAUTION----
#We need to adapt the integration procedure so that we always report the baseline concentration along with the base-corrected peak area. 

#Theory for 1-point calibration: 

#Licors have a well-established cero, so that we can directly obtain a calibration factor based on a single value of Area for an injection of known volume and concentration. 
#The area of the peak is related to the concentration of sample in the following way: 

#AREA = factor * (GHGinjection_ppm - GHGbaseline_ppm) * ml_injection

#Where: 

# AREA is the sum of the signal from the instrument over the integration window of the detected peak minus the baseline (i.e. base-corrected peak).

# GHG_injection is the GHG concentration (in ppm) of the injected gas

# GHG_baseline is the known GHG concentration (in ppm) of the carrier gas.  

#Vcal is the injected volume (in ml)

#The formula takes into account that the area caused by the injection is proportional to the difference in concentration between the sample and the baseline, that is, that the peak is affected by the degree of dilution of the two concentrations. 

#WE NEED TO USE THE BASELINE CONCENTRATION FOR CALCULATING THE CAL FACTOR (little effect) and for obtaining the concentration of all samples (little effect with UB injections usign synthetic air but very high effect for UVEG injections)


#We calculate the factor using the Average (central 95%) AREA for each gas from the standard injections of 1ml performed over various weeks from exetainers directly filled with the calibration mix. 

new_calibration<- all_standards %>% 
  filter(sampletype=="standard") %>% 
  group_by(gas) %>% 
  mutate(lower = quantile(peaksum, 0.025),
         upper = quantile(peaksum, 0.975)
  ) %>% 
  filter(between(peaksum, lower, upper)) %>% 
  summarise(calibration_period=paste(min(dayofanalysis),"to",max(dayofanalysis)),
            area=mean(peaksum),
            sd_area=sd(peaksum), 
            n_injections=sum(!is.na(peaksum)),
            GHGinjection_ppm=mean(known_ppm),
            GHGbaseline_ppm=mean(peak_baseppm),
            ml_injection=1) %>% 
  mutate(factor = area / ((GHGinjection_ppm-GHGbaseline_ppm) * ml_injection),
         sd_factor = sd_area / ((GHGinjection_ppm-GHGbaseline_ppm) * ml_injection)) %>% 
  select(gas, factor, sd_factor, n_injections, GHGinjection_ppm, ml_injection,GHGbaseline_ppm, calibration_period)

#SAVE one-point calibration factor in calibration folder of repo
write.csv(new_calibration, file=paste0(folder_cal, "One-point_calibration_factor.csv"), row.names = F)




#Check that it works: using 95% central distribution of aire and standards (excluding the most-outliying 5% of each type of sample for each gas)
  all_standards %>% 
    left_join(new_calibration %>% select(gas,factor), by = "gas") %>% 
    group_by(gas, sampletype) %>% 
    mutate(lower = quantile(peaksum, 0.025),
           upper = quantile(peaksum, 0.975)
    ) %>% 
    filter(between(peaksum, lower, upper)) %>% 
  mutate(new_ppm=(peaksum/(ml_injected*factor))+peak_baseppm) %>% 
    ggplot(aes(x=gas, y=new_ppm/known_ppm, col=sampletype))+
    geom_violin()+
    geom_boxplot()

    
  
  #Summary of deviation with 95% central distributon of aire and standards
  all_standards %>% 
    left_join(new_calibration %>% select(gas,factor), by = "gas") %>% 
    mutate(new_ppm=(peaksum/(ml_injected*factor))+peak_baseppm) %>% 
    group_by(gas, sampletype) %>% 
    mutate(lower = quantile(peaksum, 0.025),
           upper = quantile(peaksum, 0.975)
    ) %>% 
    filter(between(peaksum, lower, upper)) %>% 
    summarise(avg=mean(new_ppm/known_ppm,na.rm=T),
              sd_=sd(new_ppm/known_ppm,na.rm=T),
              n_injections=sum(!is.na(peaksum)),
              cv_percent=sd_/sqrt(n_injections)*100)
  
 #WITH THE NEW 1-point calibration, we still over-estimate the concentration of ambient air samples (although less than with the old calibration, ~6% for CH4, 4% for N2O, for CO2, our ambient air exetainers are less reliable)
  
  
  
  library(ggpmisc)
  all_standards %>% 
    left_join(new_calibration %>% select(gas,factor), by = "gas") %>% 
    mutate(new_ppm=(peaksum/(ml_injected*factor))+peak_baseppm) %>% 
    group_by(gas, sampletype) %>% 
    mutate(lower = quantile(new_ppm, 0.1),
           upper = quantile(new_ppm, 0.9)
    ) %>% 
    filter(between(new_ppm, lower, upper)) %>% 
  ggplot(aes(x=known_ppm, y=new_ppm))+
    geom_abline(slope = 1,intercept = 0)+
    geom_point()+
    geom_smooth(method="lm")+
    stat_poly_eq(aes(label = paste(..eq.label.., 
                                   ..rr.label.., sep = "~~~")))+
    facet_wrap(~gas,scales="free")
  
  
  
