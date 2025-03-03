#Map injections rawdata

# ---
# This script has been modified from https://github.com/MCabreraBrufau/Licor_N2O_scripts to identify and integrate peak not only for N2O but also for CO2 and CH4
# ---

#Description: This script creates a map_injection csv for every rawdata file. These are stored in the Map_injections folder and will be manually edited to correct label text, adapt time of labels or add labels that were not written in the data at the time of collection. 

#THINGS TO ADAPT!!!

#the current approach (i.e. master_map to identify rawfiles with cores,and then per-rawfile map_injections) might create duplicates, as the same data might be present in different rawfiles....

#If we have already all remarks collected in the master_map, we can use that map directly to select and correct remarks Instead of creating raw_maps and corrected_maps semi-manually with this script. We will still integrate and loop per-day to be clearer. 

#clean WD
rm(list=ls())

# ---- Directories ----

#Root
#Usually you will be working on your working directory
# folder_root <- dirname(rstudioapi::getSourceEditorContext()$path)
#But you can set the folder in other path
folder_root<- "C:/Users/Miguel/Dropbox/Licor_cores_UVEG" 

r4cs_root <- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data" 
#Data folders: R4Cs fieldwork ghg raw licor
folder_raw <- paste0(r4cs_root,"/GHG/RAW data/RAW Data Licor-7810") #contains unedited files downloaded from licor


#First we check if the folder exist and, if not, create one
folder_mapinjections<- paste0(folder_root,"/Map_injections") #Where csv files with the injection details will be saved. 
if (!dir.exists(folder_mapinjections)) {
  # If it doesn't exist, create the folder
  dir.create(folder_mapinjections)
}

# ---- packages & functions ----
library(tidyverse)
library(readxl)

#Import functions of repo 
repo_root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}


#Check rawdata files and map_injection files

#raw_files are named as: paste("TG20-01377-", year-month-day, "-T",hourstartexport, ".data")
#maps_done will be named as raw_map_injection_rawfile.csv, the rawfile part will not end in .data

#List maps that are already created in folder_mapinjections
maps_done<- list.files(path=folder_mapinjections, pattern = "^raw_.*_map_injection")

#List raw files (for Li-7820 and Li-7810) present in folder_raw
# go through the RAW data (.txt or .data, not .Rdata, not Licor850)
raw_files <- list.files(path = folder_raw, pattern = c(".txt|.data"), full.names = T, recursive = T)
# fs <- list.files(path = datapathRAW, pattern = c(".txt", ".data"), full.names = T, recursive = T)
r <- grep(pattern = ".RData|Licor850",x=raw_files)
raw_files <- raw_files[-r]
rm(r)


#Subset here the rawfiles that contain core injections
raw_ofinterest<- read.csv(paste0(folder_mapinjections, "/master_map.csv")) %>% 
  filter(iscoreremark=="yes") %>% 
  select(path) %>% 
  distinct %>% 
  pull(path)


#ADAPT TO select name well(everything between the last appearance of "/" and the first appearance of ".")
#Get raw files without corresponding map injection: 
raw_files_withoutmap<- raw_ofinterest[!str_extract(raw_ofinterest, "(?<=/)[^/]+(?=\\.)")%in%gsub(".csv", "", gsub(pattern = "raw_.*_map_injection_","",maps_done))]




#Collect Tstart Tend and labels for all unique remarks of every raw_file_withoutmap
#Save these details in csv files named "raw_map_injection_"[rawfilename without".data"].csv 
for (i in raw_files_withoutmap){
    a<- read_Licor_TG10(i)
    gas <- "CO2_and_CH4"
  
    # Extract text between the last "/" and the first "."
    filename <- str_extract(i, "(?<=/)[^/]+(?=\\.)")
      
    #ADAPT TO USE ONLY unixtime column, from that create date and UTCtime
  a <- a %>% 
    mutate(datelabel=paste0(date,"_",label)) %>% #Added grouping by date to avoid duplicate remark issue
    group_by(datelabel,label) %>% 
    summarise(date=first(date),Tstart=first(UTCtime), Tend =last(UTCtime)) %>% 
    ungroup() %>% 
    arrange(date,Tstart) %>% 
    mutate(rawfile=i) %>% 
    select(date, Tstart, Tend, label, rawfile) %>% 
    mutate(Tstart_correct=NA,	Tend_correct=NA,	label_correct=NA, firstlicor_TG10_or_TG20=NA)#Add empty columns to manually correct the data
  
    
  write.csv(a,file = paste0(folder_mapinjections,"/raw_", gas, "_map_injection_", filename, ".csv"),row.names = F)
}

#Clear WP again
rm(list=ls())

