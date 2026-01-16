#Map injections rawdata

#Description: This script creates a map_injection csv file for every rawdata file. These are stored in the Map_injections folder and will be manually edited to correct label text, adapt time of labels or add labels that were not written in the rawdata at the time of collection. 

#Rules for labels (remarks during acquisition):
#1. Each label identifies an injection sequence (same sample, same volume)
#2. Each label must be unique for a given day of analysis (preferable if they are unique across days as well)
#3. The underscore symbol (i.e. "_" ) is reserved to separate the sample identity from the volume injected (in ml). We cannot include the underscore for any other purpose.
#4. The injection volume must be present at the end of the label, in mL and separated from the sample identity with an underscore.
#5. You will be able to correct remarks used during acquisition after running this script

# Examples: "S4-CA-A1-5f_0.1 " sample S4-CA-A1-5f injected with 0.1 ml 

#This script uses the first and last appearance of each unique remark throughout the raw-data file, this may lead to errors if remarks were re-used (i.e. same text in remark at different moments of the day).It produces a map_injection file for every rawdata file that does not have it already. 

#Each raw_"GHG"_map_injection file will be composed of: 
#'date': date of analysis automatically filled
#'Tstart': time of first appearance of each unique remark
#'Tend': time of last appearance of each unique remark
#'label': text recorded as remark during acquisition
#'rawfile': rawfile name corresponding to the map_injection file
#'Tstart_correct': actual start to consider for integration of peaks (default NA), must be filled manually after running this script
#'Tend_correct': actual end to consider for integration of peaks (default NA), must be filled manually running this script
#'label_correct': actual label to assign to each sample (default NA), must be filled manually after running this script
#'firstliccor_TG10_or_TG20': upstream instrument during acquisition (default NA), must be filled manually after running this script. Used to set integration window widths for each instrument.  


#clean WD
rm(list=ls())

# ---- Directories ----
#To test the repository functionality (with example data):
project_root<- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/EXAMPLE_PROJECT")

#TO PROCESS YOUR OWN DATA, uncomment the following line and edit with the full path to your own your project folder (no closing "/"), eg:  

# project_root<- "C:/Users/User1/Documents/Licor-injections"


#Data folders
folder_raw <- paste0(project_root,"/Rawdata") #contains unedited files downloaded from licor

#Folder for Map Injection files: 
#Where csv files with the injection details will be saved. 
folder_mapinjections<- paste0(project_root,"/Map_injections")
if (!dir.exists(folder_mapinjections)) {
  # If it doesn't exist, create the folder
  dir.create(folder_mapinjections)
}

# ---- packages & functions ----
library(tidyverse)

#Import functions of repo 
repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}


#Check rawdata files and map_injection files

#raw_files are named as: paste("TG20-01377-", year-month-day, "-T",hourstartexport, ".data")
#maps_done will be named as raw_map_injection_rawfile.csv, the rawfile part will not end in .data

#List maps that are already created in folder_mapinjections
maps_done<- list.files(path=folder_mapinjections, pattern = "^raw_.*_map_injection")

#List raw files (for Li-7820 and Li-7810) present in folder_raw
raw_files<- list.files(path = folder_raw, pattern =  "^TG")

#Get raw files without corresponding map injection
raw_files_withoutmap<- raw_files[!gsub(".data", "",raw_files)%in%gsub(".csv", "", gsub(pattern = "raw_.*_map_injection_","",maps_done))]


#Collect Tstart Tend and labels for all unique remarks of every raw_file_withoutmap
#Save these details in csv files named "raw_[gas]_map_injection_[rawfilename without ".data"].csv" 
for (i in raw_files_withoutmap){
  #Here we check if the raw file is for CO2 and CH4 or for N2O
  if (grepl("TG10", i)) {
    a<- read_Licor_TG10(paste0(folder_raw,"/",i))
    gas <- "CO2_and_CH4"
  }
  if (grepl("TG20", i)) {
    a<- read_Licor_TG20(paste0(folder_raw,"/",i))
    gas <- "N2O"
  }
  a <- a %>% 
    group_by(label) %>% 
    summarise(date=first(date),Tstart=first(UTCtime), Tend =last(UTCtime)) %>% 
    arrange(Tstart) %>% 
    mutate(rawfile=i) %>% 
    select(date, Tstart, Tend, label, rawfile) %>% 
    mutate(Tstart_correct=NA,	Tend_correct=NA,	label_correct=NA, firstlicor_TG10_or_TG20=NA)#Add empty columns to manually correct the data
  
  write.csv(a,file = paste0(folder_mapinjections,"/raw_", gas, "_map_injection_", gsub(".data","",i), ".csv"),row.names = F)
}

#Clear WP again
rm(list=ls())

