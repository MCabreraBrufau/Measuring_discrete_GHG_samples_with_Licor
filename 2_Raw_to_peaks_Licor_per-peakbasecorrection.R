#Raw to integrated peaks and baselines#

#Before to run this script, you must run "Map_injections.R" script and manually create the "corrected_XXX_map_injection..." files from the "raw_xxx_map_injection..." within the Map_injections folder. You can copy directly the 'Tstart', 'Tend' and 'label' from the raw_mapinjection OR edit if you have something to change. You also need to specify in 'firstlicor_TG10_or_TG20' instrument was connected upstream (i.e. first in receiving the injected sample): options are "TG20" (for LicorN2O first) OR "TG10" (for LicorCH4&CO2 first).This info is used to set the width of integration windows according to the upstream-downstream position of the Licors. #For data in "EXAMPLE_PROJECT" folder, TG10 was the first Licor Upstream. 

#IMPORTANT: Make sure the separator of the csv file (comma [,] separated values) is not changed when you modify the files. Excell might swap separator from comma to semicolon depending on your geographic configuration. Use text-editors (notepad, notepad++) to check the actual separator in the csv files and to correct them if needed. 

#Description: This script integrates peaks resulting from discrete open-loop injections. 

#Inputs: 
  #Rawfiles from Li-COR 7820 and Li-COR 7810
  #corrected_map_injection files

#Outputs: 
  #Integrated injection files (peak integration data)
  #Integration plots (plots to check quality, of integrations)
  #Baseline files (statistics for remarks containing 'baseline', optional, not required for further steps)

#Peak-max detection is based on difference between max and percentile-25 of each remark
#Integration window widths are fixed for every gas depending on the upstream-downstream Licor configuration specified in corrected_map_injection files (12s for upstream, 23s for downstream instrument). 
#Baseline correction is performed for every peak individually as value of the first point in the integration window (4s before max of peak).

#If remarks that contain "baseline" are present, summary statistics are calculated and written to a different csv file (only as reference, they are not used for integration purposes). 

#REPEATED RUNS: 
#the script checks which data has already been integrated and skips it. If you need to re-integrate (after inspection of integration plots and corresponding correction of map_injection files), you must delete the integrated injections csv files from the 'Results_ppm' folder. 

#Clean Global environment
rm(list=ls())


# ---- Directories ----
#To test the repository functionality (with example data):
project_root<- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/EXAMPLE_PROJECT")

#TO PROCESS YOUR OWN DATA, uncomment the following line and edit with the full path to your own your project folder (no closing "/"), eg:  

# project_root<- "C:/Users/User1/Documents/Licor-injections"



#Data folders
folder_raw <- paste0(project_root,"/Rawdata") #contains unedited files downloaded from licor

#Map injections
folder_mapinjections<- paste0(project_root,"/Map_injections") #Contains corrected_map_injections csv files with start and stop times of remarks and their corresponding labels, corrections should be made manually when needed (editing the csvs and re-saving with "corrected_" prefix)

#Folder for plots
folder_plots<-  paste0(project_root,"/Integration_plots") #Here we will generate one pdf per gas and raw-file (auto-name), plots of each injection sequence (baseline correction & integration)
if (!dir.exists(folder_plots)) {
  # If it doesn't exist, create the folder
  dir.create(folder_plots)
}

#Folder for results
folder_results<- paste0(project_root,"/Results_ppm")#Here we will generate one csv per gas and raw-file (auto-name), with individual peak parameters.
if (!dir.exists(folder_results)) {
  # If it doesn't exist, create the folder
  dir.create(folder_results)
}




# ---- Packages & functions ----
library(tidyverse)
library(readxl)
library(lubridate)
library(pracma)
library(stringr)
library(ggpmisc)

#Load repository functions
repo_root <- dirname(rstudioapi::getSourceEditorContext()$path)
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}



###1. Check data to integrate####
#Get rawfiles
rawfiles<- list.files(path = folder_raw, pattern = ".data")

#Get corrected maps of injections
mapscorrect<- list.files(path = folder_mapinjections, pattern = "corrected_.*_map_injection_")

#Get extracted data
integratedfiles<- list.files(path = folder_results, pattern = "^integrated_injections_")


#Select code of rawfiles with corresponding mapscorrect but without integratedfiles
rawtointegrate<- gsub(".data","",rawfiles[
  gsub(".data","",rawfiles)%in%gsub(".csv","",gsub("corrected_.*_map_injection_","",mapscorrect))& #Match raw with maps
    !gsub(".data","",rawfiles)%in%gsub(".csv","",gsub("^integrated_injections_[A-Z0-9]{3}_","",integratedfiles)) #Not match raw with integratedfiles
])


###2. Integration loop####

#Loop over raw files
for (i in rawtointegrate){
  
  message(paste("Integrating peaks from",i))
  
  #Import data from rawfile
  #Here we check if the raw file is for CO2 and CH4 or for N2O
  if (grepl("TG10", i)) {
    raw_data<- read_Licor_TG10(paste0(folder_raw,"/",i,".data"))
    gasname <- "CO2_and_CH4"
  }
  if (grepl("TG20", i)) {
    raw_data<- read_Licor_TG20(paste0(folder_raw,"/",i,".data"))
    gasname <- "N2O"
  }
  raw_data <- raw_data %>% group_by(UTCtime) %>% summarise(across(everything(), ~last(.))) %>% ungroup()
  
  #Import corrected map of injections
  mapinj<- read.csv(paste0(folder_mapinjections,"/","corrected_", gasname,"_map_injection_",i,".csv")) %>% 
    filter(!is.na(label_correct)) %>% 
    filter(label_correct!="") %>% 
    select(-date)
  
  #Get date of analysis 
  dayofanalysis <- read.csv(paste0(folder_mapinjections,"/","raw_", gasname, "_map_injection_",i,".csv")) %>% 
    select(date) %>% pull() %>% unique()
  
  mapinj$date <- dayofanalysis
  
  #Aquí crear una variable para el loop con un if else, si es TG10 => c("CO2", "CH4") si no c("N2O)
  if (grepl("TG10", i)) {
    gasforloop <- c("CO2", "CH4")
  }
  if (grepl("TG20", i)) {
    gasforloop <- "N2O"
  }
  
  # #A loop for each gas species
  for (gas in gasforloop) {
    print(paste("Peak integration for", gas))
    
    #Create tables where baseline and injections will be saved
    
    #Initialize data frame for injections
    A<- data.frame(
      dayofanalysis=character(),
      label = character(),
      peak_id = character(),
      peaksum = double(),
      secondspeak =double(),
      peak_base= double(),
      peakmax = double(),
      unixtime_ofmax = double(),
      raw_peaksum = double(),
      peakSNR = double(),
      avg_remark=double(),
      sd_remark=double(),
      n_remark=double(),
      avg_baseline=double(),
      sd_nopeak=double(),
      n_nopeak=double())
    
    #Initialize data frame for baselines
    B<- data.frame(
      dayofanalysis=character(),
      label = character(),
      base_avg = double(),
      base_sd = double(),
      base_cv = double(),
      base_n = integer(),
      stringsAsFactors = FALSE
    )
    
    #Initialize list of plots to save integration plots
    plotspeak <- list()
    
    #loop over different labels of rawfile i
    for (inj in mapinj$label_correct){
      
      #Unixstart, Tstart_correct from mapinj in unix time format
      unixstart<- as.numeric(as.POSIXct(paste(mapinj[mapinj$label_correct==inj,]$date,mapinj[mapinj$label_correct==inj,]$Tstart_correct), tz = "UTC"))
      
      #Unixend, Tend_correct from mapinj in unix time format
      unixend<- as.numeric(as.POSIXct(paste(mapinj[mapinj$label_correct==inj,]$date,mapinj[mapinj$label_correct==inj,]$Tend_correct), tz = "UTC"))
      
      #FirstLicor, TG10 or TG20 from mapinj 
      firstlicor<- mapinj[mapinj$label_correct==inj,]$firstlicor_TG10_or_TG20
      
      #Subset data from injection sequence inj 
      inj_data<- raw_data[between(raw_data$unixtime, unixstart,unixend),]  
      
      #Make sure whole inj_data has the correct label inj
      inj_data$label<- inj
      
      ######2.1. Baselines #####
      if (grepl("baseline", inj)){
        print(paste0(gas,' Baseline recording: ',inj))
        
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
        print(paste0(gas," Injection sample: ", inj))
        
        #Detect and integrate peaks, plot results, calculate  baseline SD within label for Signal to Noise ratio
        
        ##_Detect peaks#####
        
        #Find local maxima in remark and add max_id (label_1,label_2,...) : 
        #Criteria for local maximum:
        # at least 1 increase before and 2 decrease after to be considered as local maxima
        # minimum peak height to be detected is > 1/5 of maximum difference between max point and percentil 25% in all remark
        # at leas 12 points between localmaxima
        
        low_boundary_peak<- inj_data %>% summarise(low=quantile(!!sym(gas),0.25)) %>% pull(low) %>% as.numeric()
        high_boundary_peak<- inj_data %>% summarise(high=max(!!sym(gas),na.rm=T)) %>% pull(high)
        
        
        inj_data <- inj_data %>%
          mutate(is_localmaxgas = ifelse(row_number() %in% findpeaks(!!sym(gas), 
                                                                     minpeakheight = ((high_boundary_peak-low_boundary_peak)/5)+low_boundary_peak, 
                                                                     nups=1, ndowns=2,
                                                                     minpeakdistance = 5)[, 2], TRUE, FALSE)) %>%
          mutate(peak_id = ifelse(is_localmaxgas, paste0(label,"_",cumsum(is_localmaxgas)), NA)) %>%  #Add unique peak_id for each local maximum found 
          ungroup()
        
        ##_Set window#####
        #Consider peakwindow as max height + 4 leading and X trailing points. (i.e. peak width == 12points), 
        
        inj_data <- inj_data %>%
          mutate(peak_id = map_chr(row_number(), function(idx) {
            #For each row, search for a non-na peak_id, look up to 4 seconds before and X seconds after the row i. Then assing the value of peak_id to the row i.
            #This results in the spread of the value of "peak_id" of the local maximum to secondsbefore_max seconds before and to secondsafter_max seconds after each identified maximum. 
            secondsbefore_max<- 4
        
            #If first licor is TG20  (N2O licor), set narrow integration windows for N2O and wider for CO2 and CH4
            if(firstlicor =="TG20"){
              if(gas == "N2O"){
                secondsafter_max<- 7
              }
              if(gas == "CO2"){
                secondsafter_max<- 18
              }
              if(gas == "CH4"){
                secondsafter_max<- 18
              }
            }
            #If first licor is TG10  (CO2&CH4 licor), set narrow integration windows for CO2 and CH4 and wider N2O
            if(firstlicor =="TG10"){
              if(gas == "N2O"){
                secondsafter_max<- 18
              }
              if(gas == "CO2"){
                secondsafter_max<- 7
              }
              if(gas == "CH4"){
                secondsafter_max<- 7
              }
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
        
        
        ##_Integration#####
        
        #Get baseline avg and SD from outside the peak windows
        avg_nopeak<-inj_data %>% 
          filter(is.na(peak_id)) %>%
          summarise(avg=mean(!!sym(gas), na.rm=T)) %>% pull(avg)
        
        sd_nopeak<-inj_data %>% 
          filter(is.na(peak_id)) %>%
          summarise(nopeak_sd=sd(!!sym(gas), na.rm=T)) %>% pull(nopeak_sd)
        
        n_nopeak<-inj_data %>% 
          filter(is.na(peak_id)) %>%
          summarise(nopeak_n=sum(!is.na(!!sym(gas))))%>% pull(nopeak_n)
        
        #Get average value for whole remark
        avg_remark<- inj_data %>% 
          summarise(avg=mean(!!sym(gas), na.rm=T)) %>% pull(avg)
        sd_remark<- inj_data %>% 
          summarise(desv=sd(!!sym(gas), na.rm=T)) %>% pull(desv)
        n_remark<-inj_data %>% 
          summarise(remark_n=sum(!is.na(!!sym(gas)))) %>% pull(remark_n)
        
        #Summarise each peak_id (peaksum, peakmax, unixtimeofmax, raw_peaksum, peakSNR) add avg_remark, sd_remark
        integrated<- inj_data %>% 
          filter(!is.na(peak_id)) %>% #keep only data of peaks
          group_by(label, peak_id) %>% #For each peak_id do the following
          mutate(gas_bc=!!sym(gas) - first(!!sym(gas)),#Base-correct timeseries for duration of peak (using the concentration of the first point of integration window, before the peak )
                 peak_base=first(!!sym(gas))) %>% 
          summarise(peaksum=sum(gas_bc),
                    peak_base=mean(peak_base,na.rm=T),
                    secondspeak=sum(!is.na(gas_bc)),
                    peakmax=max(gas_bc,na.rm = T), 
                    unixtime_ofmax=unixtime[gas_bc==peakmax],
                    raw_peaksum=sum(!!sym(gas)),.groups = "keep") %>%
          mutate(dayofanalysis=dayofanalysis,
                 peakSNR=peaksum/(sd_nopeak),
                 avg_remark=avg_remark,
                 sd_remark=sd_remark,
                 n_remark=n_remark,
                 avg_nopeak=avg_nopeak,
                 sd_nopeak=sd_nopeak,
                 n_nopeak=n_nopeak) %>% 
          ungroup()
        
        
        avg_peaksum<- mean(integrated$peaksum)
        sd_peaksum<- sd(integrated$peaksum)
        
        
        peakdataseries<- inj_data %>% 
          filter(!is.na(peak_id)) %>% #keep only data of peaks
          group_by(label, peak_id) %>% #For each peak_id do the following
          mutate(gas_bc=!!sym(gas) - ( (first(!!sym(gas)) + last(!!sym(gas)))/2 ))
        
        
        ###_Plots#####
        p<-ggplot()+
          geom_point(data=subset(peakdataseries,!is.na(peak_id)), aes(x=as.POSIXct(unixtime),y=gas_bc,col="2_peaks base corrected"))+
          geom_line(data=subset(peakdataseries), aes(x=as.POSIXct(unixtime),y=gas_bc,col="2_peaks base corrected"))+
          geom_point(data = integrated, aes(x=as.POSIXct(unixtime_ofmax,tz = "utc"), y=peaksum, col="3_peak integration"))+
          # geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = "utc"), y=gas_bc, col="1_base-corrected"))+
          geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = "utc"), y=!!sym(gas), col="1_raw data"), linetype = 2)+
          scale_y_continuous(name=paste("signal", gas))+
          scale_x_datetime(name="Licor time (UTC)",timezone = "utc")+
          labs(col="")+
          ggtitle(paste0(dayofanalysis,", injection: ",inj))+
          theme_bw()+
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
    
    if(nrow(B)>1){
    #Save baseline statistics of rawfile i (only if a baseline is present)
    write.csv(B,file = paste0(folder_results,"/", "baselines_",gas, "_", i, ".csv"),row.names = F)
    }
    
    #Save areas of injections for rawfile i   
    write.csv(A,file = paste0(folder_results,"/", "integrated_injections_",gas, "_", i, ".csv"),row.names = F)
    
    #Save plots of integrations: use i for naming convention of pdf
    print(paste0("Plotting ",gas," integrations rawfile: ", i))
    #plot every injection sequence and their integrals: 
    pdf(file = paste0(folder_plots,"/Integrations_",gas, "_",i,".pdf"))  # Open PDF device
    
    # Loop through the list of plots and print each plot
    for (plot_name in names(plotspeak)) {
      print(plotspeak[[plot_name]])
    }
    
    dev.off()  # Close the PDF device
  }#end loop for each gas species
} #end of integration loop

