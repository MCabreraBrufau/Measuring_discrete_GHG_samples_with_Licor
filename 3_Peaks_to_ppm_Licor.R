#Peaks to ppm 


#Description: this script uses integrated_injections files produced in the "Raw_to_peaks_..." script and calculates ppm for each peak based on the calibration, volume injected and peak baseline concentration measured. It outputs ppm data for each peak and for each sample

#Clean WD
rm(list=ls())


# ---- Directories ----

#Root
folder_root <- "C:/Users/Miguel/Dropbox/Licor_N2O" # You have to make sure this is pointing to the right folder in your local machine

#Here is the repo root, from which we get the calibration:
repo_root <- dirname(rstudioapi::getSourceEditorContext()$path)

#Data folders
folder_calibration <- paste0(repo_root,"/calibration")
folder_results<- paste0(folder_root,"/Results_ppm_newperpeak")



# ---- packages & functions ----
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(ggpmisc)


#Get extracted data
integratedfiles<- list.files(path = folder_results, pattern = "^integrated_injections_")
ppmfiles<- list.files(path = folder_results, pattern = "^.*ppm_samples_")

#Select integratedfiles without ppm data
integratedtoppm<- gsub(".csv","",gsub("integrated_injections_","",integratedfiles[
  !gsub(".csv","",gsub("integrated_injections_","",integratedfiles))%in%gsub(".csv","",gsub("^.*ppm_samples_","",ppmfiles))]))#  integrated files "rawcode" without corresponding ppmfiles "rawcode"
  


#Get 1-point calibration factor (based on repeated standard injections of standardbottle during 3 weeks of analysis, sampled from exetainers treated in the same way as the samples. This has been tested to be more accurate than the calibration curves produced from tedlar bags)
calibration<- read_csv(paste0(folder_calibration, "/One-point_calibration_factor.csv"),show_col_types = F)

for (i in integratedtoppm){
  #Take the correct calibration curve for the gas
  gasname <- tolower(substr(i, 1, 3))

  factor <- calibration %>% filter(gas == gasname) %>% select(factor) %>% pull()
  
  #Load integrated peaks of integratedfile i
  int<- read.csv(paste0(folder_results,"/","integrated_injections_",i,".csv"))

    
    peak_ppm<- int %>% 
      separate(peak_id, into = c("sample", "ml_injected","peak_no"), sep = "_",remove = F) %>% 
      mutate(ml_injected=as.numeric(gsub("[^0-9.]", "", ml_injected)),
             gas=gasname,
             peak_baseppm=peak_base/1000,
             peak_baseppm=if_else(peak_baseppm<0,0,peak_baseppm), #We only keep baseline value if it is positive (negative baselines are a machine-error)
             ppm= (peaksum/(factor*ml_injected))+peak_baseppm) %>% 
      select(dayofanalysis, gas, sample, ml_injected, peak_id, ppm, peaksum, peak_baseppm, unixtime_ofmax) %>% 
      mutate(datetime=as.POSIXct(unixtime_ofmax))

  #Save ppm of peaks
  write.csv(peak_ppm, file = paste0(folder_results, "/","ppm_samples_",i,".csv"), row.names = F)

}


