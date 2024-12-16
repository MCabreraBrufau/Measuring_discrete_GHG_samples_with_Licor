#Peaks to ppm 

# ---
# This script has been modified from https://github.com/MCabreraBrufau/Licor_N2O_scripts to identify and integrate peak not only for N2O but also for CO2 and CH4
# ---

#Description: this script uses integrated_injections files produced in the Raw_to_peaks_LicorN2O.R script and calculates ppm for each peak based on the calibration curve and volume injected. It outputs ppm data for each peak and for each sample

# ---- Directories ----

#Root
folder_root <- dirname(rstudioapi::getSourceEditorContext()$path)

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
ppmfiles<- list.files(path = folder_results, pattern = "^ppm_samples_")

#Select integratedfiles without ppm data
integratedtoppm<- gsub(".csv","",gsub("integrated_injections_","",integratedfiles[
  !gsub(".csv","",gsub("integrated_injections_","",integratedfiles))%in%gsub(".csv","",gsub("ppm_samples_","",ppmfiles))]))#  integrated files "rawcode" without corresponding ppmfiles "rawcode"
  
#Get calibration curve
calibration <- read_csv(paste0(folder_calibration, "/Calibration_and_limit_of_detection_2024-12-12.csv"))

for (i in integratedtoppm){
  #Load integrated peaks of integratedfile i
  int<- read.csv(paste0(folder_results,"/","integrated_injections_",i,".csv"))
  if (grepl("TG10", i)) {
    gas <- "CO2_and_CH4"
    #Calculate ppm of each peak
    slopeCO2 <- calibration %>% filter(Species == "CO2") %>% select(Slope) %>% pull()
    interceptCO2 <- calibration %>% filter(Species == "CO2") %>% select(Intercept) %>% pull()
    
    slopeCH4 <- calibration %>% filter(Species == "CH4") %>% select(Slope) %>% pull()
    interceptCH4 <- calibration %>% filter(Species == "CH4") %>% select(Intercept) %>% pull()
    
    peak_ppm<- int %>% 
      separate(peak_id, into = c("sample", "ml_injected","peak_no"), sep = "_",remove = F) %>% 
      mutate(ml_injected=as.numeric(gsub("[^0-9.]", "", ml_injected)), 
             CO2_ppm=(peaksum-interceptCO2)/(slopeCO2*ml_injected), 
             CH4_ppm=(peaksum-interceptCH4)/(slopeCH4*ml_injected)) %>% 
      select(dayofanalysis, sample, ml_injected, peak_id, CO2_ppm, CH4_ppm, unixtime_ofmax) %>% 
      mutate(datetime=as.POSIXct(unixtime_ofmax))
  }
  if (grepl("TG20", i)) {
    gas <- "N2O"
    #Calculate ppm of each peak
    slope <- calibration %>% filter(Species == "N2O") %>% select(Slope) %>% pull()
    intercept <- calibration %>% filter(Species == "N2O") %>% select(Intercept) %>% pull()
    
    peak_ppm<- int %>% 
      separate(peak_id, into = c("sample", "ml_injected","peak_no"), sep = "_",remove = F) %>% 
      mutate(ml_injected=as.numeric(gsub("[^0-9.]", "", ml_injected)), N2O_ppm=((peaksum-intercept))/(slope*ml_injected)) %>% 
      select(dayofanalysis, sample, ml_injected, peak_id, N2O_ppm, unixtime_ofmax) %>% 
      mutate(datetime=as.POSIXct(unixtime_ofmax))
  }

  
  #Save ppm of peaks
  write.csv(peak_ppm, file = paste0(folder_results, "/", gas,"ppm_peaks_",i,".csv"), row.names = F)

}


