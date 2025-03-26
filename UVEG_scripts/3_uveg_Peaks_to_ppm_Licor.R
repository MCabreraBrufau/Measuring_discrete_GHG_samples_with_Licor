#Peaks to ppm 

# ---
# This script has been modified from https://github.com/MCabreraBrufau/Licor_N2O_scripts to identify and integrate peak not only for N2O but also for CO2 and CH4
# ---

#Description: this script uses integrated_injections files produced in the TF_uveg_Raw_to_peaks_Licor_individual baseline correction.R script and calculates ppm for each peak based on the UB calibration curve, volume injected and peak baseline. It outputs ppm data for each peak and for each sample. 

#UB calibration is based on zero-baseline injections (all of the GHG amount is contained within the peak area), UVEG injections on the other hand, have significant baselines, the area of peaks is proportional to the "excess concentration" in the sample with respect to its baseline and then the baseline is added. 

#The appropriateness of UB calibration will be evaluated by comparing the values obtained from samples analyzed with both the UVEG and UB method. 


#CAUTION: ml_injected is overriden with 0.5 for testing purposes (unique value provided), for final calculation, check that this is true


#Clean WD
rm(list=ls())


# ---- Directories ----

#Root
#Usually you will be working on your working directory
#folder_root <- dirname(rstudioapi::getSourceEditorContext()$path)
#But you can set the folder in other path
folder_root <- "C:/Users/Miguel/Dropbox/Licor_cores_UVEG"
# folder_root <- "/home/jorge/Documentos/Postdoctoral/Onedrive_UB/UB/NaturBPond/GHG/Pond_element_flux/December/Discrete_samples" # You have to make sure this is pointing to the write folder on your local machine

#Here is the repo root, if we keep the code (scripts and calibration) in a repository (github) and 
#we run the code always from here, just setting the folder root to the folder we want to work with
repo_root <- dirname(rstudioapi::getSourceEditorContext()$path)

#Data folders
folder_calibration <- paste0(folder_root,"/calibration_uveg")
folder_results<- paste0(folder_root,"/TF_Results_ppm")



# ---- packages & functions ----
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(ggpmisc)


#Get extracted data
integratedfiles<- list.files(path = folder_results, pattern = "^integrated_injections_")
# ppmfiles<- list.files(path = folder_results, pattern = "^.*ppm_samples_") 
ppmfiles<- NULL

#Select integratedfiles without ppm data
integratedtoppm<- gsub(".csv","",gsub("integrated_injections_","",integratedfiles[
  !gsub(".csv","",gsub("integrated_injections_","",integratedfiles))%in%gsub(".csv","",gsub("^.*ppm_samples_","",ppmfiles))]))#  integrated files "rawcode" without corresponding ppmfiles "rawcode"
  
#Get calibration curve: test with the UB calibration for the moment 
calibration <- read_csv(paste0(folder_calibration, "/Calibration_and_limit_of_detection_2024-12-12.csv"),show_col_types = F)

#Custom adjustment of calibration based on differences between UB-derived concentrations and UVEG-derived concentrations (inspection in 4_uveg_summary_cores script)
calibration$Slope[2]<- 258 #CO2 slope is increased manually to minimize discrepancy between values obtained for the two methods (based on ~90 samples measured with both instruments)
calibration$Slope[1]<- 195 #CH4 slope is adjusted manually to minimize discrepancy (~300 samples compared between two instruments)


for (i in integratedtoppm){
  #Take the correct calibration curve for the gas
  gasname <- substr(i, 1, 3)

  slope <- calibration %>% filter(Species == gasname) %>% select(Slope) %>% pull()
  intercept <- calibration %>% filter(Species == gasname) %>% select(Intercept) %>% pull()

  #Load integrated peaks of integratedfile i
  int<- read.csv(paste0(folder_results,"/","integrated_injections_",i,".csv"))

    #Using the UB calibration (baseline==0ppm), and then adding on top the peak_base
    peak_ppm<- int %>% 
      separate(peak_id, into = c("sample", "ml_injected","peak_no"), sep = "_",remove = F) %>% 
      mutate(ml_injected=as.numeric(gsub("[^0-9.]", "", ml_injected)), 
             # ml_injected=0.5, #___________CAUTION!!!! injection volume overriden for testing
             !!paste0(gasname, "_ppm") := ((peaksum-intercept))/(slope*ml_injected) + (peak_base/1000),
             peakbase_ppm=peak_base/1000,
             nopeakbase_avg_ppm =avg_baseline/1000,
             nopeakbase_sd_ppm =sd_baseline/1000,
             remark_avg_ppm =avg_remark/1000,
             remark_sd_ppm =sd_remark/1000) %>%
      select(dayofanalysis, sample, ml_injected, peak_id, !!paste0(gasname, "_ppm"), unixtime_ofmax, peakSNR,peakbase_ppm,nopeakbase_avg_ppm,nopeakbase_sd_ppm,remark_avg_ppm,remark_sd_ppm) %>% 
      mutate(datetime=as.POSIXct(unixtime_ofmax))

  #Save ppm of peaks
  write.csv(peak_ppm, file = paste0(folder_results, "/","ppm_samples_",i,".csv"), row.names = F)

}




