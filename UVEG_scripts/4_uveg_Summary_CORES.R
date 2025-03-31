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



##1.2.UB-UVEG Calibration####

#As we do not have calibration data for UVEG licor, we used the UB calibration based

#To test calibration, subset the dataset to samples with at least 2 injections of peakSNR>5 and cv_ppm <0.1 (10%)
uveg_best<- cores %>% 
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

thresholds<- compare %>% 
  group_by(gas) %>% 
  summarise(lower= quantile(percentreldif, 0.90, na.rm=T), 
            upper= quantile(percentreldif, 0.1, na.rm=T))

compare_good<- compare %>% 
  left_join(thresholds, by="gas") %>% 
  mutate(excluded=if_else(between(percentreldif, upper,lower),F,T))

#Comparison in ppm
compare_good %>% 
  ggplot( aes(x=avg_ppmub, y= avg_ppm, col=excluded))+
  geom_point()+
  geom_abline(slope = 1,intercept = 0)+
  geom_smooth(method = "lm")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))+
  facet_wrap(~gas, scales="free")

#Comparson in log-transformed ppm
compare_good %>% 
ggplot( aes(x=log_ppmub, y= log_ppmuveg, col=excluded))+
  geom_point()+
  geom_abline(slope = 1,intercept = 0)+
  geom_smooth(method = "lm")+
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")))+
  facet_wrap(~gas, scales="free")

#Relative difference between methods
ggplot(compare_good, aes(x=gas, y= (avg_ppm-avg_ppmub)/avg_ppmub))+
  geom_boxplot()+
  geom_point(data=. %>% filter(excluded==T),aes(col="excluded"))+
  ggtitle("Relative difference between methods")

ggplot(compare_good, aes(x=gas, y= (avg_ppm-avg_ppmub)))+
  geom_boxplot()+
  geom_point(data=. %>% filter(excluded==T),aes(col="excluded"))+
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

##---2. Inspect & clean----

#####HERE------
#Here we have to perform the selection of injections based on their deviation and on the inspection plots, we will keep the average peakbaseline value when no peak is detected.

#Peak detection will be based on peakSNR (peakarea/3*baseline_sd): custom threshold after exploration

#Peaks not detected with peakSNR will be visually inspected to determine whether we use them as peaks ( (peaksum/ml_injected*factor) + peakbase) or we keep the value of the baseline (one of peakbase_ppm, nopeakbase_avg_ppm,remark_avg_ppm). 

#With 


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


ch4_peakout<- c("S1-CA-R2-2f_0.5_1","S1-CA-R2-3f_0.5_1","S1-CA-A1-3f_0.5_2","S1-CA-A1-3f_0.5_4","S1-CA-A1-4f_0.5_1","S1-CA-P2-6f_0.5_1",
                "S1-CU-R2-2f_0.5_1","S1-CU-R1-1f_0.5_5","S1-CU-P1-2f_0.5_2","S1-CU-P2-6f_0.5_1","S1-CU-R1-5f_0.5_1",
                "S1-DA-R1-6f_0.5_1","S1-DA-R1-6f_0.5_2","S1-DA-P1-3f_0.5_2","S1-DA-P2-2f_0.5_1","S1-DA-R1-1f_0.5_1","S1-DA-R1-1f_0.5_2","S1-DA-P2-4f_0.5_1","S1-DA-A2-4f_0.5_1","S1-DA-R2-2f_0.5_1","S1-DA-R2-6f_0.5_1","S1-DA-A1-5f_0.5_1","S1-DA-A1-2f_0.5_4","S1-DA-A2-5f_0.5_1",
                "S1-DU-A2-1f_0.5_1","S1-DU-A1-5f_0.5_1","S1-DU-P1-5f_0.5_3",
                "S1-RI-P1-5f_0.5_1","S1-RI-P1-6f_0.5_1",
                "S1-VA-A2-5f_0.5_1","S1-VA-P1-1f_0.5_1","S1-VA-P2-3f_0.5_2","S1-VA-P2-1f_0.5_1",
                "S2-CA-A1-5f_0.5_1","S2-CA-R2-3f_0.5_3","S2-CA-A2-1f_0.5_1","S2-CA-R1-2f_0.5_1","S2-CA-P2-1f_0.5_1",
                "S2-CU-A2-3f_0.5_1","S2-CU-A2-5f_0.5_1","S2-CU-A2-6f_0.5_1","S2-CU-A2-4f_0.5_1","S2-CU-A1-6f_0.5_1","S2-CU-A1-1f_0.5_1","S2-CU-R1-3f_0.5_1","S2-CU-A1-4f_0.5_1","S2-CU-P2-5f_0.5_1","S2-CU-P2-1f_0.5_1","S2-CU-R1-6f_0.5_3","S2-CU-R2-1f_0.5_1","S2-CU-P2-2f_0.5_1","S2-CU-R1-5f_0.5_3","S2-CU-A2-1f_0.5_1","S2-CU-P2-3f_0.5_1",
                "S2-DA-R1-5f_0.5_1","S2-DA-R1-3f_0.5_1","S2-DA-R1-6f_0.5_1","S2-DA-P2-3f_0.5_1","S2-DA-P2-4f_0.5_1","S2-DA-P2-1f_0.5_1","S2-DA-A1-6f_0.5_3","S2-DA-A1-3f_0.5_3","S2-DA-P2-6f_0.5_2","S2-DA-R1-1f_0.5_2","S2-DA-A1-1f_0.5_3","S2-DA-P2-2f_0.5_1","S2-DA-P1-3f_0.5_1",
                "S2-DU-R2-6f_0.5_1","S2-DU-R2-4f_0.5_1","S2-DU-R1-1f_0.5_2","S2-DU-P1-5f_0.5_1","S2-DU-A2-1f_0.5_1","S2-DU-A2-6f_0.5_1",
                "S2-RI-A1-5f_0.5_1","S2-RI-A1-6f_0.5_1","S2-RI-P1-1f_0.5_4","S2-RI-P1-4f_0.5_4",
                "S2-VA-R1-1f_0.5_1","S2-VA-R1-5f_0.5_1","S2-VA-R1-4f_0.5_1","S2-VA-R1-3f_0.5_1","S2-VA-P2-6f_0.5_1",
                "S3-CA-R2-5f_0.5_2",
                "S3-DA-P2-1f_0.5_1","S3-DA-P2-1f_0.5_2","S3-DA-P2-1f_0.5_3","S3-DA-A1-1f_0.5_1","S3-DA-A1-1f_0.5_2","S3-DA-A1-1f_0.5_3","S3-DA-A1-6f_0.5_1","S3-DA-A1-6f_0.5_2","S3-DA-P2-2f_0.5_1","S3-DA-P2-2f_0.5_2","S3-DA-A2-2f_0.5_1","S3-DA-A2-2f_0.5_2","S3-DA-P1-1f_0.5_1","S3-DA-P2-3f_0.5_1","S3-DA-A1-5f_0.5_1","S3-DA-P1-2f_0.5_1",
                "S3-DU-R1-1f_0.5_2","S3-DU-A2-6f_0.5_1","S3-DU-P2-5f_0.5_1","S3-DU-A1-6f_0.5_1",
                "S3-RI-P1-1f_0.5_1","S3-RI-P1-1f_0.5_2","S3-RI-R2-6f_0.5_3","S3-RI-P1-5f_0.5_3","S3-RI-A1-2f_0.5_4","S3-RI-R2-2f_0.5_1","S3-RI-R2-2f_0.5_2","S3-RI-R2-5f_0.5_1",
                "S3-VA-A2-2f_0.5_2","S3-VA-R1-2f_0.5_3","S3-VA-P1-3f_0.5_1","S3-VA-P1-3f_0.5_2","S3-VA-R1-3f_0.5_1","S3-VA-R1-6f_0.5_1","S3-VA-A2-1f_0.5_1","S3-VA-A2-1f_0.5_2","S3-VA-A1-2f_0.5_1","S3-VA-A1-2f_0.5_2","S3-VA-A1-2f_0.5_3")#peaks that cannot be used (for anything)

ch4_samples4peakbase<- c("S1-CA-A1-2f","S1-CA-P2-6f","S1-DU-P1-1f","S1-DU-P1-2f","S1-DU-P2-1f","S1-DU-P1-4f","S1-DU-A1-6f","S1-DU-A1-5f","S1-DU-P1-3f","S1-DU-P1-5f","S1-DU-A1-3f","S1-DU-P1-6f","S1-DU-A1-2f","S1-VA-P1-1f","S1-VA-P1-3f","S1-VA-P1-5f","S2-CA-P2-2f","S2-DU-A1-2f","S2-DU-R1-2f","S2-DU-A1-4f","S2-DU-A1-5f","S2-DU-P1-1f","S2-DU-A1-6f","S2-DU-A2-4f","S2-DU-R1-6f","S2-DU-A1-3f","S2-DU-A2-1f","S2-DU-R1-3f","S2-DU-A2-3f","S2-DU-P1-6f","S2-DU-P1-4f","S2-DU-A2-2f","S2-RI-A1-5f","S2-RI-P1-3f","S2-RI-P1-2f","S2-RI-A1-2f","S3-DU-A2-3f","S3-RI-R2-3f","S3-RI-R2-1f") #Samples for which we will take the average basepeak of the non-outlier peaks

ch4_customprocess<- c("S1-DA-P1-1f","S1-DA-A1-3f","S1-CA-R1-3f","S1-DU-A1-4f","S2-DA-A1-4f") #Samples for custom process: without any assigned peak (nothing detected in remark for co2 or ch4), with only 1 valid peak for peakbase and not good-enough remarkbaseline. To decide and process.

ch4_samplesinspected<- c()#non-important, only to avoid clogging the graph with samples slightly bad (i.e. cv good but larger than 0.05)
ch4_samplinginspected<- c("S1-CA","S1-CU","S1-DA","S1-DU","S1-RI","S1-VA",
                          "S2-CA","S2-CU","S2-DA","S2-DU","S2-RI","S2-VA",
                          "S3-CA","S3-DA","S3-DU","S3-RI","S3-VA")

#No samples S3-CU CH4, all rest of injections ch4 are inspected and cleaned. 




#NOTHING INSPECTED&Decided for CO2: very difficult and noisy

#CO2 inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  mutate(sampling=substr(sample, 1,5)) %>% 
  filter(gas=="co2") %>% 
  filter(sampling=="S2-DU") %>% 
  filter(!peak_id%in%co2_peakout) %>% 
  filter(!sample%in%c(co2_samplesinspected,co2_samples4peakbase,co2_samples4remarkbase)) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_ppm=mean(ppm, na.rm=T),
         sd_ppm= sd(ppm, na.rm=T),
         cv_ppm=abs(sd_ppm/avg_ppm),
         n_ppm=sum(!is.na(ppm))) %>% 
  filter(cv_ppm>0.1) %>%
  filter(n_ppm>2) %>% 
  # filter(cv_ppm>0.5) %>%
  arrange(desc(cv_ppm)) %>% 
  ggplot(aes(x=factor(round(cv_ppm,3)), y=ppm, col=(peakSNR>3)))+
  geom_point()+
  geom_label(aes(label=peak_id))


co2_peakout<- c("S1-CA-R2-3f_0.5_3","S1-CA-P1-4f_0.5_1","S1-CA-P1-4f_0.5_2","S1-CA-A1-4f_0.5_2","S1-CA-P1-6f_0.5_3","S1-CA-A1-5f_0.5_3","S1-CA-A1-5f_0.5_1","S1-CA-R1-4f_0.5_1","S1-CA-P1-3f_0.5_1","S1-CA-A2-1f_0.5_1","S1-CA-R2-6f_0.5_3","S1-CA-R2-6f_0.5_2","S1-CA-R1-7f_0.5_3","S1-CA-R2-5f_0.5_2","S1-CA-P2-5f_0.5_1","S1-CA-P1-2f_0.5_2","S1-CA-P2-4f_0.5_1","S1-CA-A2-6f_0.5_2","S1-CA-R2-1f_0.5_3","S1-CA-P2-3f_0.5_2","S1-CA-A2-5f_0.5_1","S1-CA-A1-3f_0.5_1","S1-CA-A1-3f_0.5_2","S1-CA-A2-3f_0.5_2","S1-CA-P2-6f_0.5_2","S1-CA-R2-2f_0.5_3","S1-CA-R1-2f_0.5_1","S1-CA-A2-2f_0.5_3","S1-CA-R1-5f_0.5_2","S1-CA-P1-1f_0.5_2",
                "S1-CU-P1-3f_0.5_3","S1-CU-R1-1f_0.5_1","S1-CU-R1-1f_0.5_5","S1-CU-A2-2f_0.5_1","S1-CU-P2-5f_0.5_1","S1-CU-R1-5f_0.5_3","S1-CU-R2-5f_0.5_2","S1-CU-P2-2f_0.5_1","S1-CU-P2-1f_0.5_2","S1-CU-R2-1f_0.5_1","S1-CU-P2-6f_0.5_2","S1-CU-P2-4f_0.5_2","S1-CU-P2-3f_0.5_2",
                "S1-DA-R1-4f_0.5_2","S1-DA-R1-5f_0.5_2","S1-DA-R1-5f_0.5_3","S1-DA-P1-3f_0.5_2","S1-DA-P1-6f_0.5_1","S1-DA-R1-6f_0.5_1","S1-DA-R1-6f_0.5_3","S1-DA-R1-6f_0.5_5","S1-DA-R1-1f_0.5_4","S1-DA-R1-1f_0.5_3","S1-DA-R2-5f_0.5_1","S1-DA-R2-5f_0.5_2","S1-DA-P2-4f_0.5_1","S1-DA-R2-6f_0.5_1","S1-DA-R2-4f_0.5_1","S1-DA-R1-2f_0.5_2",
                "S1-DU-A1-6f_0.5_2","S1-DU-A1-6f_0.5_3","S1-DU-A1-1f_0.5_1","S1-DU-R2-2f_0.5_1","S1-DU-P1-2f_0.5_2","S1-DU-R2-4f_0.5_1","S1-DU-R2-6f_0.5_3","S1-DU-R2-1f_0.5_2","S1-DU-R2-3f_0.5_3","S1-DU-R1-1f_0.5_1","S1-DU-A1-2f_0.5_1","S1-DU-P1-5f_0.5_3",
                "S1-RI-P1-5f_0.5_1","S1-RI-P1-2f_0.5_1","S1-RI-A1-6f_0.5_3","S1-RI-P1-2f_0.5_1","S1-RI-P1-2f_0.5_3","S1-RI-R2-5f_0.5_1","S1-RI-R2-5f_0.5_2","S1-RI-P1-6f_0.5_3","S1-RI-A2-1f_0.5_3","S1-RI-P2-5f_0.5_2","S1-RI-A2-5f_0.5_1","S1-RI-P1-4f_0.5_2","S1-RI-R1-3f_0.5_3","S1-RI-R2-6f_0.5_1","S1-RI-A1-5f_0.5_3","S1-RI-R1-4f_0.5_1","S1-RI-R1-2f_0.5_3","S1-RI-R2-3f_0.5_3",
                "S1-VA-R1-1f_0.5_3","S1-VA-R2-5f_0.5_1","S1-VA-R1-4f_0.5_3","S1-VA-A1-6f_0.5_3","S1-VA-R2-3f_0.5_2","S1-VA-R1-3f_0.5_1","S1-VA-A2-3f_0.5_3","S1-VA-A2-5f_0.5_3","S1-VA-A2-1f_0.5_2","S1-VA-R2-6f_0.5_1","S1-VA-A2-4f_0.5_3","S1-VA-A1-4f_0.5_2","S1-VA-A1-5f_0.5_1","S1-VA-P1-2f_0.5_2")#peaks that cannot be used (for anything)

co2_samples4peakbase<- c() #Samples for which we will take the average basepeak of the non-outlier peaks

co2_samples4remarkbase<- c("S1-CA-A1-2f","S1-CA-A1-6f","S1-CA-A1-1f",
                           "S1-CU-P1-2f","S1-CU-A2-4f","S1-CU-R1-3f","S1-CU-R2-4f","S1-CU-R2-6f","S1-CU-R2-2f","S1-CU-A2-1f","S1-CU-A2-6f","S1-CU-R1-2f","S1-DA-P1-2f",
                           "S1-RI-P2-3f","S1-RI-P2-2f",
                           "S1-VA-P2-3f","S1-VA-R2-4f","S1-VA-P2-4f","S1-VA-A1-1f","S1-VA-A1-2f","S1-VA-A1-3f","S1-VA-P2-2f","S1-VA-A2-6f","S1-VA-R2-1f","S1-VA-R2-2f","S1-VA-P2-5f","S1-VA-P2-1f")#Samples for which we will take the average of the whole remark

co2_customprocess<- c("S1-DA-P1-1f","S1-DA-A1-3f","S1-CA-R1-3f","S1-DU-A1-4f") #Samples for custom process: without any assigned peak (nothing detected in remark for co2 or ch4), with only 1 valid peak for peakbase and not good-enough remarkbaseline. To decide and process.

co2_samplesinspected<- c("S1-VA-R1-1f","S1-VA-R2-5f","S1-VA-R1-6f","S1-VA-P1-4f","S1-VA-R2-3f","S1-RI-A1-6f","S1-VA-A1-6f","S1-VA-P1-6f")#non-important, only to avoid clogging the graph with samples slightly bad (i.e. cv good but larger than 0.05)
co2_samplinginspected<- c("S1-CA",#Extremely noisy, not sure about outliers/keepers
                          "S1-CU",#Extremely noisy, not sure about outliers/keepers for some
                          "S1-DA", #More peaks clearer than S1-CA & S1-CU
                          "S1-DU", #Super clear peaks and outliers
                          "S1-RI", #Super clear peaks and outliers
                          "S1-VA"  #Very noisy, unclear for many samples
                          )

#Weird:"S1-DA-P1-3f" clear negative Co2 peaks but resulting absolute Co2 ppm < 0 (calfactor issue)












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

