#Peaks to ppm 

# ---
# This script has been modified from https://github.com/MCabreraBrufau/Licor_N2O_scripts to identify and integrate peak not only for N2O but also for CO2 and CH4
# ---

#Description: this script uses integrated_injections files produced in the Raw_to_peaks_LicorN2O.R script and calculates ppm for each peak based on the calibration curve and volume injected. It outputs ppm data for each peak and for each sample

# ---- Directories ----

#Root
#Usually you will be working on your working directory
#folder_root <- dirname(rstudioapi::getSourceEditorContext()$path)
#But you can set the folder in other path
folder_root <- "/home/jorge/Documentos/Postdoctoral/Onedrive_UB/UB/NaturBPond/GHG/Pond_element_flux/December/Discrete_samples" # You have to make sure this is pointing to the write folder on your local machine

#Data folders
folder_calibration <- paste0(folder_root,"/calibration")
folder_results<- paste0(folder_root,"/Results_ppm")



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
  
#Get calibration curve
calibration <- read_csv(paste0(folder_calibration, "/Calibration_and_limit_of_detection_2024-12-12.csv"))

for (i in integratedtoppm){
  #Take the correct calibration curve for the gas
  gasname <- substr(i, 1, 3)

  slope <- calibration %>% filter(Species == gasname) %>% select(Slope) %>% pull()
  intercept <- calibration %>% filter(Species == gasname) %>% select(Intercept) %>% pull()

  #Load integrated peaks of integratedfile i
  int<- read.csv(paste0(folder_results,"/","integrated_injections_",i,".csv"))

    
    peak_ppm<- int %>% 
      separate(peak_id, into = c("sample", "ml_injected","peak_no"), sep = "_",remove = F) %>% 
      mutate(ml_injected=as.numeric(gsub("[^0-9.]", "", ml_injected)), !!paste0(gasname, "_ppm") := ((peaksum-intercept))/(slope*ml_injected)) %>% 
      select(dayofanalysis, sample, ml_injected, peak_id, !!paste0(gasname, "_ppm"), unixtime_ofmax) %>% 
      mutate(datetime=as.POSIXct(unixtime_ofmax))

  #Save ppm of peaks
  write.csv(peak_ppm, file = paste0(folder_results, "/","ppm_samples_",i,".csv"), row.names = F)

}


