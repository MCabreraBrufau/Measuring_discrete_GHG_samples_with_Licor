#Raw to integrated peaks and baselines (with per-peak baseline correction)#

# ---
# This script has been modified from https://github.com/MCabreraBrufau/Licor_N2O_scripts to identify and integrate peak not only for N2O but also for CO2 and CH4
# ---

#Before running this script, you must run "Map_injections.R" script, and modify and save the "corrected_XXX_map_injection..." file manually. You can copy directly the Tstart, Tend and label from the raw_mapinjection OR edit if you have something to change.  With the new updated processing script you also need to specify in "corrected_XXX_map_injection..."which Licor was connected upstream (i.e. first in receiving the injected sample): options are "TG20" (for LicorN2O first) OR "TG10" (for LicorCH4&CO2 first).This info is used to set the width of integration windows according to the upstream-downstream position of the Licors. 

#Description: this script takes raw-files from Li-COR 7820 and Li-COR 7810 containing discrete injections, corrected injection_sequences (with label, start and stop) and calculates integrated peaks along with signal-to-noise ratio for each injection. It also generates inspection plots (baseline correction & integration) and stores the results in csv format. It also extracts the baseline data for ambient lab air and zero-Air from cylinder. 

#IMPORTANT MODIFICATIONS: 
#This script implements a per-peak baseline correction (as oposed to the original script with the per-remark baseline-correction). This approach should be more consistent for remarks where the baseline is very noisy (i.e. ambient air used as carrier gas). 

#Current per-peak baseline correction is just to subtract the value of the very first "peak" point (4seconds before peak maximum) from all of the "peak" window. 


#Clean WD
rm(list=ls())

# ---- Directories ----

#Root
#Usually you will be working on your working directory
#folder_root <- dirname(rstudioapi::getSourceEditorContext()$path)
#But you can set the folder in other path

folder_root<- "C:/Users/Miguel/Dropbox/Licor_cores_UVEG" 
r4cs_root <- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data" 
#Data folders: R4Cs fieldwork ghg raw licor
folder_raw <- paste0(r4cs_root,"/GHG/RAW data/RAW Data Licor-7810") #contains unedited files downloaded from licor

#If you ran the Map_injections.R script, this folder have already been created, if not, run it
folder_mapinjections<- paste0(folder_root,"/Map_injections") #Contains the corrected_master_map.csv with startstop times of injections and their corresponding labels, corrections should be made manually when needed (editting the csvs and re-saving with "corrected_" prefix)

#Folder for plots
folder_plots<-  paste0(folder_root,"/Integration plots") #One pdf per rawfile (auto-name from rawfile name), plots of each injection sequence (baseline correction & integration)

if (!dir.exists(folder_plots)) {
  # If it doesn't exist, create the folder
  dir.create(folder_plots)
}

#Folder for results
folder_results<- paste0(folder_root,"/Results_ppm")#One csv per rawfile will be created (auto-name from rawfile name), with individual peak parameters (label, peak_id, peaksum, peakmax, unixtime_ofmax, raw_peaksum, dayofanalysis, SNR)

if (!dir.exists(folder_results)) {
  # If it doesn't exist, create the folder
  dir.create(folder_results)
}


# ---- packages & functions ----
library(tidyverse)
library(readxl)
library(lubridate)
library(ptw)
library(pracma)
library(stringr)
library(ggpmisc)

#Load repository functions
repo_root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}

###1. Check data to integrate####

#Get corrected master map

master_map<-read.csv( paste0(folder_mapinjections,"/corrected_master_map.csv"))

#Get rawfiles
rawfiles<- master_map %>% 
  filter(!is.na(label_correct)) %>% 
  select(path,date) %>% 
  distinct() %>% 
  mutate(date=dmy(date),
         outfilename=paste0(str_extract(path, "(?<=/)[^/]+(?=\\.)"),"_",date))
  
  

#Get extracted data
integratedfiles<- list.files(path = folder_results, pattern = "^integrated_injections_")


#Select code of rawfiles with corresponding mapscorrect but without integratedfiles
rawtointegrate<- unique(rawfiles[!rawfiles$outfilename%in%gsub(".csv","",gsub("^integrated_injections_[A-Z0-9]{3}_","",integratedfiles)),"path"]) #Not match raw with integratedfiles


####TO TEST#####


###2. Integration loop####

#Loop over raw files
for (i in rawtointegrate){
  rawfilename<- str_extract(i, "(?<=/)[^/]+(?=\\.)")
  
  message(paste("Integrating peaks from",rawfilename))
  
  #Import data from rawfile
    raw_data<- read_Licor_TG10(i)
    gasname <- "CO2_and_CH4"
    gasforloop <- c("CO2", "CH4")
  
    
    #Import corrected map of injections
    mapinj<- master_map %>% 
      filter(!is.na(label_correct)) %>% 
      filter(path==i)
    
    #Subset raw_data to injections timespanspan
    first_remarkunix<- min(mapinj %>% mutate(unixstart=as.numeric(dmy_hms(paste(date,Tstart_correct), tz = "UTC"))) %>% pull(unixstart))
    last_remarkunix<- max(mapinj %>% mutate(unixstop=as.numeric(dmy_hms(paste(date,Tend_correct), tz = "UTC"))) %>% pull(unixstop))              
    #IF we have repeated entries in raw_data with the same unixtime, we keep the last appearance
  raw_data <- raw_data %>% filter(between(unixtime,first_remarkunix,last_remarkunix)) %>% 
                                    group_by(unixtime) %>% summarise(across(everything(), ~last(.))) %>% ungroup()
  
  
  daystoloop<- unique(mapinj$date)
  
  # read.csv(paste0(folder_mapinjections,"/","corrected_", gasname,"_map_injection_",i,".csv")) %>% 
    # filter(!is.na(label_correct)) %>% 
    # filter(label_correct!="") %>% 
    # select(-date)
  
  #Loop over dayofanalysis on mapinj
  for(dayofanalysis in daystoloop){
      
  # dayofanalysis <- read.csv(paste0(folder_mapinjections,"/","raw_", gasname, "_map_injection_",i,".csv")) %>% 
    # select(date) %>% pull() %>% unique()
  datetoprint<- dmy(dayofanalysis)
  
  # mapinj$date <- dayofanalysis
    mapinj<- master_map %>% 
      filter(!is.na(label_correct)) %>% 
      filter(path==i)%>% 
      filter(date==dayofanalysis)
    

  # #A loop for each gas species
  for (gas in gasforloop) {
    print(paste("Peak integration for", gas))
    
    
    #Initialize data frame for injections
    A<- data.frame(
      dayofanalysis=character(),
      label = character(),
      peak_id = character(),
      peaksum = double(),
      peakmax = double(),
      unixtime_ofmax = double(),
      raw_peaksum = double(),
      peakSNR = double())
    
    #Initialize data frame for baselines
    # B<- data.frame(
    #   dayofanalysis=character(),
    #   label = character(),
    #   base_avg = double(),
    #   base_sd = double(),
    #   base_cv = double(),
    #   base_n = integer(),
    #   stringsAsFactors = FALSE
    # )
    
    #Initialize list of plots to save integration plots
    plotspeak <- list()
    
    #loop over different labels of date dayofanalysis in rawfile i
    for (inj in mapinj$label_correct){
      
      #Unixstart, Tstart_correct from mapinj in unix time format
      unixstart<- as.numeric(dmy_hms(paste(mapinj[mapinj$label_correct==inj,]$date,mapinj[mapinj$label_correct==inj,]$Tstart_correct), tz = "UTC"))
      
      #Unixend, Tend_correct from mapinj in unix time format
      unixend<- as.numeric(dmy_hms(paste(mapinj[mapinj$label_correct==inj,]$date,mapinj[mapinj$label_correct==inj,]$Tend_correct), tz = "UTC"))
      #FirstLicor, TG10 or TG20 from mapinj 
      # firstlicor<- mapinj[mapinj$label_correct==inj,]$firstlicor_TG10_or_TG20
      
      #Subset data from injection sequence inj 
      inj_data<- raw_data[between(raw_data$unixtime, unixstart,unixend),]  
      
      # raw_data %>% filter(between(unixtime, unixstart,unixend)) %>% mutate(secondsincestart=unixtime-unixstart)
      #Make sure whole inj_data has the correct label inj
      inj_data$label<- inj
      
      ######2.1. Baselines #####
      if (grepl("baseline", inj)){
        print(paste0('Baseline recording: ',inj))
        
        #calculate descriptive statistics for baseline
        b<- inj_data %>% 
          summarise(base_date=dayofanalysis,
                    label=inj,
                    base_avg= mean(!!sym(gas),na.rm = T), 
                    base_sd= sd(!!sym(gas),na.rm=T),
                    base_cv=base_sd/base_avg,
                    base_n= n())
        
        #Add baseline statistics to baseline table
        B<- rbind(B,b)
      } 
      
      ###2.2. Injections#####
      else {
        print(paste0("Injection sample: ", inj))
        
        #Detect and integrate peaks, plot results, calculate  baseline SD within label for Signal to Noise ratio
        
        # ##____Base-correction# (OLD)
        # #Base-correct injection sequence, using asymetric least-square. 
        # inj_data<-inj_data %>% 
        #   mutate(gas_bc=baseline.corr(!!sym(gas),lambda=1e5, p=0.0001))
        
        ##____Peak-max detection#####
        
        #Find local maxima in remark and add max_id (label_1,label_2,...) : 
        #Criteria for local maximum:
        # at least 1 increase before and 1 decrease after to be considered as local maxima
        # minimum peak height to be detected is > 1/5 of maximum difference between max point and min point in all remark
        # at leas 5 points between localmaxima
        
        inj_data <- inj_data %>%
          mutate(is_localmaxgas = ifelse(row_number() %in% findpeaks(!!sym(gas), 
                                                                     minpeakheight = (((max(!!sym(gas),na.rm = T)-min(!!sym(gas),na.rm=T))/5)+min(!!sym(gas),na.rm=T)), 
                                                                     nups=1, ndowns=1,
                                                                     minpeakdistance = 5)[, 2], TRUE, FALSE)) %>%
          mutate(peak_id = ifelse(is_localmaxgas, paste0(label,"_",cumsum(is_localmaxgas)), NA)) %>%  #Add unique peak_id for each local maximum found 
          ungroup()
        
        ##____Peak-window selection#####
        #Consider peakwindow as max height + 4 leading and X trailing points. (i.e. peak width == 12points), 
        inj_data <- inj_data %>%
          mutate(peak_id = map_chr(row_number(), function(idx) {
            #For each row, search for a non-na peak_id, look "secondsbefore_max" seconds before and "secondsafter_max" seconds after the row i. Then assing the value of peak_id to the row i.
            #This results in the spread of the value of "peak_id" of the local maximum to secondsbefore_max seconds before and to secondsafter_max seconds after each identified maximum. 
            
            #Dedicated windows of integration for each gas
              if(gas == "CO2"){
                secondsbefore_max<- 4
                secondsafter_max<- 7
              }
              if(gas == "CH4"){
                secondsbefore_max<- 4
                secondsafter_max<- 7
              }
            
            # Check for peak_id in the window:
            surrounding_codes <- peak_id[seq(max(1, idx - secondsafter_max), min(n(), idx + secondsbefore_max))]  
            
            # Return the peak_id if it's available, otherwise return NA
            if (any(!is.na(surrounding_codes))) {
              return(first(na.omit(surrounding_codes)))  # Use the first valid peak_id found
            } else {
              return(NA)
            }
          }))
        
        
        ##____Peak integration gas#####
        
        #Get baseline noise to outside the peak areas
        baseline_sd<-inj_data %>% 
          filter(is.na(peak_id)) %>%
          summarise(baseline_sd=sd(!!sym(gas), na.rm=T)) %>%  ungroup()%>% pull(baseline_sd)
        
        #Summarise each peak_id (peaksum, peakmax, unixtimeofmax, raw_peaksum, peakSNR)
        integrated<- inj_data %>% 
          filter(!is.na(peak_id)) %>% #keep only data of peaks
          group_by(label, peak_id) %>% #For each peak_id do the following
          mutate(gas_bc=!!sym(gas)-first(!!sym(gas))) %>% #Base-corrected timeseries for duration of peak (using the first data point of the peak)
          summarise(peaksum=sum(gas_bc),
                    peakmax=max(gas_bc,na.rm = T), 
                    unixtime_ofmax=unixtime[gas_bc==peakmax],
                    raw_peaksum=sum(!!sym(gas)),.groups = "keep") %>%
          mutate(dayofanalysis=dayofanalysis,
                 peakSNR=peaksum/(3*baseline_sd)) %>% 
          ungroup()
        
        
        avg_peaksum<- mean(integrated$peaksum)
        sd_peaksum<- sd(integrated$peaksum)
        
        peakdataseries<- inj_data %>% 
          filter(!is.na(peak_id)) %>% #keep only data of peaks
          group_by(label, peak_id) %>% #For each peak_id do the following
          mutate(gas_bc=!!sym(gas)-first(!!sym(gas)))
        
        ###____Create integration plots#####
        p<-ggplot()+
          geom_point(data=subset(peakdataseries,!is.na(peak_id)), aes(x=as.POSIXct(unixtime),y=gas_bc,col="2_peaks base corrected"))+
          geom_line(data=subset(peakdataseries), aes(x=as.POSIXct(unixtime),y=gas_bc,col="2_peaks base corrected"))+
          geom_point(data = integrated, aes(x=as.POSIXct(unixtime_ofmax,tz = "utc"), y=peaksum, col="3_peak integration"))+
          # geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = "utc"), y=gas_bc, col="1_base-corrected"))+
          geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = "utc"), y=!!sym(gas), col="1_raw data"), linetype = 2)+
          scale_y_continuous(name=paste("signal", gas))+
          scale_x_datetime(name="Licor time (UTC)",timezone = "utc")+
          labs(col="")+
          ggtitle(paste0(datetoprint,", injection: ",inj))+
          theme_bw()+
          # Add label for average peaksum value
          # geom_text(data=integrated, aes(x = as.POSIXct(min(unixtime_ofmax)-50), 
          #               y = min(peaksum)*0.8, 
          #               label = paste("Avg: ", round(avg_peaksum, 2), " ± ", round(sd_peaksum, 2), " (CV= ",round(sd_peaksum/avg_peaksum,2),")" )), color = "black", hjust = 0, 
          #           vjust = 1, 
          #           size = 4, 
          #           fontface = "italic")
          # 
          annotate("text",x = as.POSIXct(min(integrated$unixtime_ofmax)-50), 
                   y = min(integrated$peaksum)*0.8, 
                   label = paste ("Avg: ", round(avg_peaksum, 2), " ± ", round(sd_peaksum, 2), " (CV= ",round(sd_peaksum/avg_peaksum,2),")" ), color = "black", hjust = 0, 
                   vjust = 1, 
                   size = 4, 
                   fontface = "italic")
        
        # Store each plot in the list
        plotspeak[[inj]] <- p
        
        #Add integrations of inj to injections table
        A<-rbind(A,integrated)
        
      }
      
    } 
    
    #Save baseline statistics of rawfile i 
    # write.csv(B,file = paste0(folder_results,"/", "baselines_",gas, "_", rawfilename, ".csv"),row.names = F)
    
    #Save areas of injections for rawfile i   
    write.csv(A,file = paste0(folder_results,"/", "integrated_injections_",gas, "_",datetoprint, rawfilename, ".csv"),row.names = F)
    
    #Save plots of integrations: use i for naming convention of pdf
    print(paste0("Plotting integrations to file: Integrations_",gas,"_",datetoprint,"_", rawfilename,".pdf"))
    #plot every injection sequence and their integrals: 
    pdf(file = paste0(folder_plots,"/Integrations_",gas,"_",datetoprint,"_", rawfilename,".pdf"))  # Open PDF device
    
    # Loop through the list of plots and print each plot
    for (plot_name in names(plotspeak)) {
      print(plotspeak[[plot_name]])
    }
    
    dev.off()  # Close the PDF device
  }#end loop for each gas species
  }#end loop for each dayofanalysis
} #end loop for each rawtointegrate 

