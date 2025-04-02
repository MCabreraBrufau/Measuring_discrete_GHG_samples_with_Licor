#Script to obtain calibration from UVEG


#Description: This script includes all steps to obtain Calibration factor from UVEG Licor. Map injections, integration of peaks, peak selection and calculation of calibration factor. 


#Execute all:


rm(list=ls())


# ---- 0. packages & functions ----
library(tidyverse)
library(readxl)
library(lubridate)
library(ptw)
library(pracma)
library(stringr)
library(ggpmisc)

#Import functions of repo 
repo_root <- dirname(dirname(rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}

#------ 0. Directories------- 
folder_root<- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/Cores/UVEG Calibration/CH4_250307"

folder_raw<- folder_root
folder_mapinjections<- folder_root

folder_plots<-  paste0(folder_root,"/Integration plots") #One pdf per dayofinjections (auto-name from rawfile name), plots of each injection sequence (baseline correction & integration)
if (!dir.exists(folder_plots)) {
  # If it doesn't exist, create the folder
  dir.create(folder_plots)
}

#Folder for results
folder_results<- paste0(folder_root,"/Integration results")#One csv per dayofinjections will be created (auto-name from rawfile name), with individual peak parameters
if (!dir.exists(folder_results)) {
  # If it doesn't exist, create the folder
  dir.create(folder_results)
}



  #1. Map_injections-----
  #List maps that are already created in folder_mapinjections
  maps_done<- list.files(path=folder_mapinjections, pattern = "^raw_.*_map_injection")
  
  #List raw files (for Li-7820 and Li-7810) present in folder_raw
  raw_files<- list.files(path = folder_raw, pattern =  "^TG")
  #Aquí debería revisar todos los archivos licor (TG10 y TG20) e identificar que gas es cada uno
  
  
  #Get raw files without corresponding map injection
  raw_files_withoutmap<- raw_files[!gsub(".data", "",raw_files)%in%gsub(".csv", "", gsub(pattern = "raw_.*_map_injection_","",maps_done))]
  
  {
    
  #Collect Tstart Tend and labels for all unique remarks of every raw_file_withoutmap
  #Save these details in csv files named "raw_map_injection_"[rawfilename without".data"].csv 
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
    
    #Estaría bien añadir una marca al nombre del archivo para saber si es de N2O o de CO2 aunque en realidad ya estaría con el nombre del rawfile (TG10 o TG20)
    write.csv(a,file = paste0(folder_mapinjections,"/raw_", gas, "_map_injection_", gsub(".data","",i), ".csv"),row.names = F)
  }
  
}


rm(i,a)
  
  
#2. Integration-------
  #UVEG Licor Not in UTC, Madrid timezone
tz<- "Europe/Madrid"
  
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
        unixstart<- as.numeric(as.POSIXct(paste(mapinj[mapinj$label_correct==inj,]$date,mapinj[mapinj$label_correct==inj,]$Tstart_correct), tz = tz))
        
        #Unixend, Tend_correct from mapinj in unix time format
        unixend<- as.numeric(as.POSIXct(paste(mapinj[mapinj$label_correct==inj,]$date,mapinj[mapinj$label_correct==inj,]$Tend_correct), tz = tz))
        
        #FirstLicor, TG10 or TG20 from mapinj 
        firstlicor<- mapinj[mapinj$label_correct==inj,]$firstlicor_TG10_or_TG20
        
        #Subset data from injection sequence inj 
        inj_data<- raw_data[between(raw_data$unixtime, unixstart,unixend),]  
        
        #Make sure whole inj_data has the correct label inj
        inj_data$label<- inj
        
        ######2.1. Baselines CH4#####
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
          
          # ##____Base-correction##### OLD NOT RUN
          # #Base-correct injection sequence, using asymetric least-square. 
          # inj_data<-inj_data %>% 
          #   mutate(gas_bc=baseline.corr(!!sym(gas),lambda=1e5, p=0.0001))
          
          ##____Peak-max detection#####
          
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
          
          ##____Peak-window selection#####
          #Consider peakwindow as max height + 4 leading and X trailing points. (i.e. peak width == 12points), 
          
          inj_data <- inj_data %>%
            mutate(peak_id = map_chr(row_number(), function(idx) {
              #For each row, search for a non-na peak_id, look up to 4 seconds before and X seconds after the row i. Then assing the value of peak_id to the row i.
              #This results in the spread of the value of "peak_id" of the local maximum to secondsbefore_max seconds before and to secondsafter_max seconds after each identified maximum. 
              secondsbefore_max<- 4
              #Aquí podría unificar CO2 y CH4 en 15, 20 o 18 segundos (el mismo para ambos) y otro para el N2O = 7
              #If first licor is TG20  (N2O licor), set narrow integration windows for N2O and wider for CO2 and CH4
              if(firstlicor =="TG20"){
                if(gas == "N2O"){
                  secondsafter_max<- 7
                }
                if(gas == "CO2"){
                  secondsafter_max<- 15
                }
                if(gas == "CH4"){
                  secondsafter_max<- 20
                }
              }
              #If first licor is TG10  (CO2&CH4 licor), set narrow integration windows for CO2 and CH4 and wider N2O
              if(firstlicor =="TG10"){
                if(gas == "N2O"){
                  secondsafter_max<- 20
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
          
          
          ##____Peak integration gas#####
          
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
            mutate(gas_bc=!!sym(gas) - ( (first(!!sym(gas)) + last(!!sym(gas)))/2 ),#Base-corrected timeseries for duration of peak (using average of the first and last data-points of the integration window)
                   peak_base=((first(!!sym(gas))+last(!!sym(gas)))/2)) %>% 
            summarise(peaksum=sum(gas_bc),
                      peak_base=mean(peak_base,na.rm=T),
                      secondspeak=sum(!is.na(gas_bc)),
                      peakmax=max(gas_bc,na.rm = T), 
                      unixtime_ofmax=unixtime[gas_bc==peakmax],
                      raw_peaksum=sum(!!sym(gas)),.groups = "keep") %>%
            mutate(dayofanalysis=dayofanalysis,
                   peakSNR=peaksum/sd_nopeak,
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
          
          
          ###____Create integration plots#####
          p<-ggplot()+
            geom_point(data=subset(peakdataseries,!is.na(peak_id)), aes(x=as.POSIXct(unixtime),y=gas_bc,col="2_peaks base corrected"))+
            geom_line(data=subset(peakdataseries), aes(x=as.POSIXct(unixtime),y=gas_bc,col="2_peaks base corrected"))+
            geom_point(data = integrated, aes(x=as.POSIXct(unixtime_ofmax,tz = tz), y=peaksum, col="3_peak integration"))+
            # geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = "utc"), y=gas_bc, col="1_base-corrected"))+
            geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = tz), y=!!sym(gas), col="1_raw data"), linetype = 2)+
            scale_y_continuous(name=paste("signal", gas))+
            scale_x_datetime(name=paste0("Licor time (",tz,")"),timezone = tz)+
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
      
      #Save baseline statistics of rawfile i 
      write.csv(B,file = paste0(folder_results,"/", "baselines_",gas, "_", i, ".csv"),row.names = F)
      
      #Save areas of injections for rawfile i   
      write.csv(A,file = paste0(folder_results,"/", "integrated_injections_",gas, "_", i, ".csv"),row.names = F)
      
      #Save plots of integrations: use i for naming convention of pdf
      print(paste0("Plotting integrations of day: ", i))
      #plot every injection sequence and their integrals: 
      pdf(file = paste0(folder_plots,"/Integrations_",gas, "_",i,".pdf"))  # Open PDF device
      
      # Loop through the list of plots and print each plot
      for (plot_name in names(plotspeak)) {
        print(plotspeak[[plot_name]])
      }
      
      dev.off()  # Close the PDF device
    }#end loop for each gas species
  } #end of integration loop
  
rm(a,A,B,inj_data, integated, mapinj, p, peakdataseries, plotspeak, raw_data, 
   avg_nopeak, avg_peaksum, avg_remark, dayofanalysis, f, files.sources, firstlicor,gas, gasforloop, gasname, high_boundary_peak, i, inj, unixend, unixstart, sd_remark, sd_peaksum, sd_nopeak,tz, plot_name, n_nopeak, n_remark, low_boundary_peak)

#3. Peak Selection------
  
#The data provided by uveg is extremely noisy, especially for CO2. unclear if they inyected standard with known CO2, but the baseline is too noisy. 
  
#Apparently cal injections are only of CH4 standards. 
  
integratedch4files<- list.files(path = folder_results, pattern = "^integrated_injections_CH4",full.names = T)
  
rm(i,a)
  for (i in integratedch4files){
a<- read.csv(i)
  
 if (i==integratedch4files[1]){ch4<- a} else{ ch4<- rbind(ch4, a)}
  }
rm(a)


#POR AQUI: 

ch4_clean<- ch4 %>%
  separate(peak_id, into = c("sample", "ml_injected", "peakno"), sep = "_", remove = F) %>% 
  separate(sample, into = c())
  filter(!(label=="peak_id%in%c(""))

