#Map injections rawdata

# ---
# This script has been modified from https://github.com/MCabreraBrufau/Licor_N2O_scripts to identify and integrate peak not only for N2O but also for CO2 and CH4
# ---

#Description: This script creates a map_injection csv for every rawdata file. These are stored in the Map_injections folder and will be manually edited to correct label text, adapt time of labels or add labels that were not written in the data at the time of collection. 

#COMMENTS TO ADAPT!!!!-------
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
library(lubridate)
#Import functions of repo 
repo_root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}



##--Import master_map-----
#Import master_map with Carlos details

master_map_raw<- read_xlsx(paste0(r4cs_root, "/Cores/map_incubations+UVEG info.xlsx"),
                           #take only 1st, 6th, 9th and onwards columns
                           col_types = c("text", #type of meaure
                                         "skip","skip","skip","skip", #skip t0co2, t0ch4, headps, water
                                         "numeric", #vol_inj_ml
                                         "skip","skip", #skip corestart, coreend
                                         "text",#label_revised_carlos,
                                         "date",#date
                                         "text",#label
                                         "numeric","numeric",#start, stop (unixtime)
                                         "skip","skip","skip","skip", #skip timestart,timestop, folder, file
                                         "text")) #path

#Filter and add new-format
master_map_tocorrect<- master_map_raw %>% 
  filter(!is.na(type_of_measure)) %>% 
  select(type_of_measure, vol_inj_ml,label_revised_carlos,date,label,start,stop,path) %>% 
  mutate(Tstart= format(as.POSIXct(start, origin="1970-01-01", tz="UTC"),"%H:%M:%S"),
         Tstop= format(as.POSIXct(stop, origin="1970-01-01", tz="UTC"),"%H:%M:%S"),
         date=format(date, "%d/%m/%Y"), #date as text of format dd/mm/yyyy
         remark_duration=stop-start,
         rawfilename=str_extract(path, "(?<=/)[^/]+(?=\\.)"),
         remarkid=paste0(start,label),
         Tstart_correct=NA,	Tend_correct=NA,	label_correct=NA, obs_CO2_plot=NA, obs_CH4_plot=NA)


#If there is data already corrected, add it to the tocorrect map 
if(file.exists(paste0(folder_mapinjections,"/corrected_master_map.csv"))){
#Import already corrected maps
corrected_master<- read.csv( paste0(folder_mapinjections,"/corrected_master_map.csv")) %>% 
  #filter corrected lines only
  filter(!is.na(label_correct)) %>% 
  #select only remarkid and corrected columns
  #ADD essential columns for splitted remarks: date, path, type_of_measure
  select(remarkid, date, path, type_of_measure, Tstart_correct,Tend_correct,label_correct,obs_CO2_plot,obs_CH4_plot)


#add corrected data to master_tocorrect
master_map_tocorrect <- master_map_tocorrect %>% 
  select(-c(Tstart_correct,Tend_correct,label_correct,,obs_CO2_plot,obs_CH4_plot)) %>% #Remove corrected columns (filled with NAs)
  merge.data.frame(corrected_master, by = c("remarkid","date","path","type_of_measure"),all = T)#add corrected values, rest filled with NAs
}

#reorder master_map_tocorrect
master_map_tocorrect<-master_map_tocorrect %>% 
  mutate(datetoorder=as.POSIXlt.character(date, format="%d/%m/%Y")) %>% 
  arrange(type_of_measure, datetoorder,Tstart_correct) %>% 
  select(-datetoorder)

#Save in folder_mapinjections
write.csv(master_map_tocorrect, file = paste0(folder_mapinjections,"/tocorrect_master_map.csv"),row.names = F)



#Manually modify tocorrect_master_map.csv and save with name corrected_master_map.csv
#Iterative process, to modify anything, always start with tocorrect_master_map file and save changes as corrected_master_map


str(master_map_tocorrect$datetoorder)





##---OLD missing-----

#ADAPT TO select name well(everything between the last appearance of "/" and the first appearance of ".")
#Get raw files without corresponding map injection: 
raw_files_withoutmap<- raw_ofinterest[!str_extract(raw_ofinterest, "(?<=/)[^/]+(?=\\.)")%in%gsub(".csv", "", gsub(pattern = "raw_.*_map_injection_","",maps_done))]


#List raw files (for Li-7820 and Li-7810) present in folder_raw
# go through the RAW data (.txt or .data, not .Rdata, not Licor850)
raw_files <- list.files(path = folder_raw, pattern = c(".txt|.data"), full.names = T, recursive = T)
# fs <- list.files(path = datapathRAW, pattern = c(".txt", ".data"), full.names = T, recursive = T)
r <- grep(pattern = ".RData|Licor850",x=raw_files)
raw_files <- raw_files[-r]
rm(r)




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
  
    
  # write.csv(a,file = paste0(folder_mapinjections,"/raw_", gas, "_map_injection_", filename, ".csv"),row.names = F)
}

#Clear WP again
rm(list=ls())

