#Peaks to ppm 


#Description: this script uses integrated_injections files produced in the "Raw_to_peaks_..." script and calculates ppm for each peak based on the calibration factor, volume injected and baseline concentration measured (for each peak). It outputs ppm data for each integrated peak.

#Calibration factor used here was obtained following the one-point calibration procedure described in the Licor-application note. Calibration factor should be obtained for each Li-COR instrument, and standards should be treated in the exact same way as samples during injection to minimize biases. Check the structure of the One-point_calibration_factor.csv included in the "calibration" folder to see how you must create your own. 

#Clean Global environment
rm(list=ls())



# ---- Directories ----
#To test the repository functionality (with example data):
project_root<- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/EXAMPLE_PROJECT")

#TO PROCESS YOUR OWN DATA, uncomment the following line and edit with the full path to your own your project folder (no closing "/"), eg:  

# project_root<- "C:/Users/User1/Documents/Licor-injections"


#Data folders
folder_results<- paste0(project_root,"/Results_ppm")

#Here is the repository calibration folder, from which we get the calibration file:
repo_root <- dirname(rstudioapi::getSourceEditorContext()$path)
folder_calibration <- paste0(repo_root,"/calibration")


# ---- Packages & functions ----
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(ggpmisc)

#Load repository functions
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}


# ---- Calculate ppm--------
#Get extracted data
integratedfiles<- list.files(path = folder_results, pattern = "^integrated_injections_")
ppmfiles<- list.files(path = folder_results, pattern = "^.*ppm_samples_")

#Select integratedfiles without ppm data
integratedtoppm<- gsub(".csv","",gsub("integrated_injections_","",integratedfiles[
  !gsub(".csv","",gsub("integrated_injections_","",integratedfiles))%in%gsub(".csv","",gsub("^.*ppm_samples_","",ppmfiles))]))#  integrated files "rawcode" without corresponding ppmfiles "rawcode"
  


#Get 1-point calibration factor (based on repeated standard injections of standardbottle during 3 weeks of analysis, sampled from exetainers treated in the same way as the samples. This has been tested to be more accurate than the calibration curves produced from tedlar bags and stored as Calibration_and_limit_of_detection_2024-12-12.csv)
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
             peak_baseppm=if_else(peak_baseppm<0,0,peak_baseppm), #We only keep baseline value if it is positive (negative baselines are a machine-error and should not be kept for ppm calculation)
             ppm= (peaksum/(factor*ml_injected))+peak_baseppm) %>% 
      select(dayofanalysis, gas, sample, ml_injected, peak_id, ppm, peaksum, peak_baseppm, unixtime_ofmax) %>% 
      mutate(datetime=as.POSIXct(unixtime_ofmax))

  #Save ppm of peaks
  write.csv(peak_ppm, file = paste0(folder_results, "/","ppm_samples_",i,".csv"), row.names = F)

}


