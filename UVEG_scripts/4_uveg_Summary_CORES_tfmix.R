#Export all TF mix CH4 concentrations (ppm) to RESTORE4Cs dropbox


#This script loads all Licor-derived concentrations in TFmix_Results_ppm folder, filters injections from cores to remove outliers and wrong injections, and saves all data into Restore4Cs folder for cores.

####CAUTION!!------
#CAUTION, DO NOT RUN UNTIL WE KNOW FOR SURE THE VOLUME OF THE TFmix injections, after overview of integration plots, I think some samples may have been injected with different volumes in the same remark. 


#This script works with the integrated injections. Peak_id selection is adjusted to work without knowing the injection volume, assuming all injections have the same volume. 

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
CH4ppmfiles<-list.files(folder_resuts, pattern = "^integrated_injections_CH4", recursive = T, full.names = T)

for(i in CH4ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4ppmfiles[1]){ch4<- a}else {ch4<- rbind(ch4,a)}
  if(i==CH4ppmfiles[length(CH4ppmfiles)]){ rm(i,a,CH4ppmfiles)}
}


#Format, adding tfpeakid (sample_peaknum, without injection volume) 
ch4<- ch4 %>% 
  separate(peak_id, into = c("sample","ml_injected","injnum"), sep = "_", remove = F) %>% 
  mutate(gas="ch4",
         tfpeakid=paste(sample, injnum, sep="_")
         )

#Check that tfpeakid is unique
length(unique(ch4$tfpeakid))==length(ch4$tfpeakid)


##---2. Inspect & clean----

#####HERE------

#We will inspect the relative deviation of peaksum between replicate injections and decide if any replicate injection is marked as outlier for removal

test<- ch4 %>% 
  mutate(season=substr(label,1,2)) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_peaksum=mean(peaksum, na.rm=T),
         sd_peaksum= sd(peaksum, na.rm=T),
         cv_peaksum=abs(sd_peaksum/avg_peaksum))


test %>% 
  ggplot(aes(x=cv_peaksum, fill=gas)) +
  geom_histogram()+
  facet_grid(season~gas, scales="free")


test %>% 
  select(season, sample, gas, cv_peaksum) %>%
  distinct() %>% 
  group_by(gas, season) %>%
  mutate(lowvar=cv_peaksum<0.05) %>% 
  summarise(good=sum(lowvar,na.rm = T),
            total=n(),
            percent=good/total*100) %>% 
  select(season, gas, percent, good, total) %>% 
  arrange(season,gas)
  

###TO ADAPT#####
#CH4 inspection: 
#Inspect samples with very high cv and clean individual peaks.
test %>% 
  mutate(sampling=substr(sample, 1,5)) %>% 
  filter(gas=="ch4") %>% 
  filter(sampling=="S2-CU") %>% 
  filter(!tfpeakid%in%ch4_peakout) %>% 
  filter(!sample%in%c(ch4_samplesinspected)) %>% 
  group_by(sample, gas) %>% 
  mutate(avg_peaksum=mean(peaksum, na.rm=T),
         sd_peaksum= sd(peaksum, na.rm=T),
         cv_peaksum=abs(sd_peaksum/avg_peaksum)) %>% 
  filter(cv_peaksum>0.1) %>%
  arrange(desc(cv_peaksum)) %>% 
  ggplot(aes(x=factor(round(cv_peaksum,3)), y=peaksum, col=(peakSNR>3)))+
  geom_point()+
  geom_label(aes(label=tfpeakid))


ch4_peakout<- c("S1-CA-A2-4m_3","S1-CA-R2-1m_1","S1-CA-R1-2m_1","S1-CA-R2-3m_1","S1-CA-R2-4m_2","S1-CA-A1-2m_1","S1-CA-R2-2m_1",
                "S1-CU-R2-5m_1","S1-CU-R2-4m_2","S1-CU-R2-6m_1","S1-CU-A2-1m_1","S1-CU-R2-2m_1","S1-CU-A2-2m_1",
                "S1-DA-R1-2m_3","S1-DA-R1-5m_3","S1-DA-R1-5m_4","S1-DA-R1-4m_3","S1-DA-R1-3m_3","S1-DA-R2-4m_1","S1-DA-R2-4m_2","S1-DA-R2-1m_1","S1-DA-R2-1m_2","S1-DA-R2-5m_1","S1-DA-R2-2m_1","S1-DA-R2-2m_2","S1-DA-R2-6m_1","S1-DA-R2-6m_2","S1-DA-A2-5m_1",
                "S1-VA-A2-1m_1","S1-VA-P2-3m_1",
                "S2-CA-A1-3m_3","S2-CA-R2-1m_1")#peaks that cannot be used (for anything)

ch4_samplesinspected<- c("S2-CU-P2-3m","S2-CA-A1-2m","S2-CA-A1-5m","S1-DA-R1-4m","S1-DA-A2-4m","S1-DA-P2-5m","S1-DA-P2-3m","S1-DA-P2-2m","S1-DA-A2-3m","S1-DA-R1-1m","S1-CA-R2-3m","S1-CA-P2-2m","S1-CA-A2-4m","S1-CU-R2-5m","S1-CU-R2-3m","S1-CU-A2-1m")#non-important, only to avoid clogging the graph with samples slightly bad (i.e. cv good but larger than 0.05)

ch4_samplinginspected<- c("S1-CA","S1-CU","S1-DA","S1-RI","S1-VA",
                          "S2-CA")

ch4_samplings<- c("S1-CA","S1-CU","S1-DA","S1-DU","S1-RI","S1-VA",
                          "S2-CA","S2-CU","S2-DA","S2-DU","S2-RI","S2-VA",
                          "S3-CA","S3-DA","S3-DU","S3-RI","S3-VA")










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




##---3. Export cleaned data ----

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
            estimate="peak-integration")

#Samples for which we trust the peak-base as representative of sample concentration
cores_peakbase<-cores %>% 
  filter((sample%in%ch4_samples4peakbase&gas=="ch4")|(sample%in%c(co2_samples4peakbase)&gas=="co2")) %>% 
  group_by(gas,sample) %>% 
  summarise(avg_ppm=mean(peakbase_ppm, na.rm=T),
            sd_ppm= sd(peakbase_ppm, na.rm=T),
            cv_ppm=abs(sd_ppm/avg_ppm),
            n_injections=NA_real_,
            estimate="average baseline") 

#Samples for which we do not trust peak-derived concentrations, we think average remark concentration is more representative.
cores_averageremark<- cores %>% 
  filter((sample%in%ch4_samples4remarkbase&gas=="ch4")|(sample%in%c(co2_samples4remarkbase)&gas=="co2")) %>% 
  group_by(gas,sample) %>%
  summarise(avg_ppm=mean(remark_avg_ppm, na.rm=T),
            sd_ppm= mean(remark_sd_ppm, na.rm=T),
            cv_ppm=abs(sd_ppm/avg_ppm), 
            n_injections=NA_real_,
            estimate="average baseline")
           

#Join all estimates
allcores<- rbind(cores_peaks, cores_peakbase, cores_averageremark)


# TF cores S1
s1cores<- allcores %>% 
  filter(grepl("^S1", sample))

#Complete dataset (all injections with NAs for outliers)
write.csv(s1cores, file=paste0(folder_export, "CO2_CH4_ppm_core_avg_sd_n.csv"), row.names = F)




#List of samples for which a customprocess is needed: bad peak detection & baseline not usable:  
customprocess<- c(paste0(ch4_customprocess, "_ch4"), paste0(co2_customprocess,"_co2"))




#Inspect variability for cores
#CO2
allcores %>% 
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
allcores %>% 
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


