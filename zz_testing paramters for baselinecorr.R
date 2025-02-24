#Raw to integrated peaks and baselines#

#AFTER INSPECTING THE DATA for the days of noisy baseline, we saw that the lower accuracy still does not impact the results substantially. Additionally, we have the initial CO2 measured in situ by the valencian team, in case we chose to use those values instead. for N2O and CH4 everything is good. 


#In the future, if baseline is expected or seen to be noisy, we could change the way the baseline is corrected, instead of adjusting the baseline for the whole remark, extend the window of peaks a few seconds before the peak, and base-correct every individual peak based on the minimum values observed just before that peak. 




# ---
# This script has been modified from https://github.com/MCabreraBrufau/Licor_N2O_scripts to identify and integrate peak not only for N2O but also for CO2 and CH4
# ---

#Before to run this script, you must run "Map_injections.R" script and create the "corrected_XXX_map_injection..." file. You can copy directly the Tstart, Tend and label from the raw_mapinjection OR edit if you have something to change. 
#IMPORTANT: With the new updated processing script you also need to specify in "corrected_XXX_map_injection..."which Licor was connected upstream (i.e. first in recieving the injected sample): options are "TG20" (for LicorN2O first) OR "TG10" (for LicorCH4&CO2 first).This info is used to set the width of integration windows according to the upstream-downstream position of the Licors. 


#Description: this script takes raw-files from Li-COR 7820 and Li-COR 7810 containing discrete injections, corrected injection_sequences (with label, start and stop) and calculates integrated peaks along with signal-to-noise ratio for each injection. It also generates inspection plots (baseline correction & integration) and stores the results in csv format. It also extracts the baseline data for ambient lab air and zero-Air from cylinder. 


#Clean WD
rm(list=ls())

# ---- Directories ----

#Root
#Usually you will be working on your working directory
#folder_root <- dirname(rstudioapi::getSourceEditorContext()$path)
#But you can set the folder in other path

folder_root<- "C:/Users/Miguel/Dropbox/Licor_N2O"
# folder_root <- "/home/jorge/Documentos/Postdoctoral/Onedrive_UB/UB/NaturBPond/GHG/Pond_element_flux/December/Discrete_samples" # You have to make sure this is pointing to the write folder on your local machine

#Data folders
folder_raw <- paste0(folder_root,"/Rawdata_testbasecorr") #contains unedited files downloaded from licor

#If you ran the Map_injections.R script, this folder have already been created, if not, run it
folder_mapinjections<- paste0(folder_root,"/Map_injections") #Contains csvs with startstop times of injections and their corresponding labels, corrections should be made manually when needed (editting the csvs and re-saving with "corrected_" prefix)

#Folder for plots
folder_plots<-  paste0(folder_root,"/Integration plots_testbasecorr") #One pdf per dayofinjections (auto-name from rawfile name), plots of each injection sequence (baseline correction & integration)
if (!dir.exists(folder_plots)) {
  # If it doesn't exist, create the folder
  dir.create(folder_plots)
}

#Folder for results
folder_results<- paste0(folder_root,"/Results_ppm_testbasecorr")#One csv per dayofinjections will be created (auto-name from rawfile name), with individual peak parameters (label, peak_id, peaksum, peakmax, unixtime_ofmax, raw_peaksum, dayofanalysis, SNR)
if (!dir.exists(folder_results)) {
  # If it doesn't exist, create the folder
  dir.create(folder_results)
}

#Original parameters: lambda=1e5, pval=0.0001

lambdalist<- c(1e5,10)
p_vallist<- c(0.0001, 0.001)

# ---- packages & functions ----
library(tidyverse)
library(readxl)
library(lubridate)
library(ptw)
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
for (p_val in p_vallist){
for (s in lambdalist){
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
    # gasforloop <- c("CO2")
  }
  if (grepl("TG20", i)) {
    gasforloop <- "N2O"
  }
  
  # #A loop for each gas species
  for (gas in gasforloop) {
    print(paste("Peak integration for", gas," with lamda=",s," and asym=",p_val))
    
    #Create tables where baseline and injections will be saved
    
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
        print(paste0("Injection sample: ", inj," with lamda=",s," and asym=",p_val))
        
        #Detect and integrate peaks, plot results, calculate  baseline SD within label for Signal to Noise ratio
        
        ##____Base-correction#####
        #Base-correct injection sequence, using asymetric least-square. ptw::asysm()
        # "lambda" is the smoothing parameter (usually 1e5-1e8)
        # "p" is the asymmetry weight, (from 0 to 1 the closer to 0 the more weight is given to values below the line (the less values below the baseline are allowed)
        #"!!sym(gas)" just checks that all values of gas are numeric. 
        inj_data<-inj_data %>% 
          mutate(gas_bc=baseline.corr(!!sym(gas),lambda= s, p=p_val))
        
        ##____Peak-max detection#####
        
        #Find local maxima in sequence and add max_id (label_1,label_2,...) : 
        #Criteria for local maximum:
        # at least 1 increase before and 1 decrease afterwards
        # minimum peak height to be detected is > 1/5 of maximum point in all remark
        # at leas 5 points between localmaxima
        
        inj_data <- inj_data %>%
          mutate(is_localmaxgas = ifelse(row_number() %in% findpeaks(gas_bc, 
                                                                     minpeakheight = max(gas_bc,na.rm = T)/5, 
                                                                     nups=1, ndowns=1,
                                                                     minpeakdistance = 5)[, 2], TRUE, FALSE)) %>%
          mutate(peak_id = ifelse(is_localmaxgas, paste0(label,"_",cumsum(is_localmaxgas)), NA)) %>%  #Add unique code for local maxima 
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
        
        #Get baseline noise to outside the peak areas
        baseline_sd<-inj_data %>% 
          filter(is.na(peak_id)) %>%
          summarise(baseline_sd=sd(gas_bc, na.rm=T)) %>%  ungroup()%>% pull(baseline_sd)
        
        #Summarise each peak_id (peaksum, peakmax, unixtimeofmax, raw_peaksum, peakSNR)
        integrated<- inj_data %>% 
          filter(!is.na(peak_id)) %>% #keep only data of peaks
          group_by(label, peak_id) %>% 
          summarise(peaksum=sum(gas_bc),
                    peakmax=max(gas_bc), 
                    lambda=s,
                    asym_p=p_val,
                    unixtime_ofmax=unixtime[gas_bc==max(gas_bc)],
                    raw_peaksum=sum(!!sym(gas)),.groups = "keep") %>%
          mutate(dayofanalysis=dayofanalysis,
                 peakSNR=peaksum/(3*baseline_sd)) %>% 
          ungroup()
        
        
        avg_peaksum<- mean(integrated$peaksum)
        sd_peaksum<- sd(integrated$peaksum)
        min_gas<- min(inj_data[,gas],na.rm = T)
        
        ###____Create integration plots#####
        p<-ggplot()+
          geom_point(data=subset(inj_data,!is.na(peak_id)), aes(x=as.POSIXct(unixtime),y=gas_bc,col="2_peaks"))+
          geom_point(data = integrated, aes(x=as.POSIXct(unixtime_ofmax,tz = "utc"), y=peaksum, col="3_integrated"))+
          geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = "utc"), y=gas_bc, col="1_base-corrected"))+
          geom_line(data = inj_data, aes(x=as.POSIXct(unixtime,tz = "utc"), y=!!sym(gas)-min_gas, col="0_rawMinusMin"), linetype = 2)+
          scale_y_continuous(name=paste("signal", gas))+
          scale_x_datetime(name="Licor time (UTC)",timezone = "utc")+
          labs(col="")+
          ggtitle(paste0("Lambda:",s,", asym:",p_val ,", injection: ",inj))+
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
    # write.csv(B,file = paste0(folder_results,"/", "baselines_",gas,"_lambda",s, "_", i, ".csv"),row.names = F)
    
    #Save areas of injections for rawfile i   
    write.csv(A,file = paste0(folder_results,"/", "integrated_injections_",gas,"_lambda",s,"_asym",p_val,  "_", i, ".csv"),row.names = F)
    
    #Save plots of integrations: use i for naming convention of pdf
    print(paste0("Plotting integrations of day: ", i))
    #plot every injection sequence and their integrals: 
    pdf(file = paste0(folder_plots,"/Integrations_",gas,"_lambda",s,"_asym",p_val,"_",i,".pdf"))  # Open PDF device
    
    # Loop through the list of plots and print each plot
    for (plot_name in names(plotspeak)) {
      print(plotspeak[[plot_name]])
    }
    
    dev.off()  # Close the PDF device
  }#end loop for each gas species
} #end of integration loop
}
}






##--- Evaluate quality-----

##---1. Import & format----
#Import N2O
N2Oppmfiles<-list.files(folder_results, pattern = "integrated_injections_N2O", recursive = T, full.names = T)

for(i in N2Oppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==N2Oppmfiles[1]){n2o<- a}else {n2o<- rbind(n2o,a)}
  if(i==N2Oppmfiles[length(N2Oppmfiles)]){ rm(i,a,N2Oppmfiles)}
}


#Import CO2
CO2ppmfiles<-list.files(folder_results, pattern = "integrated_injections_CO2", recursive = T, full.names = T)

for(i in CO2ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CO2ppmfiles[1]){co2<- a}else {co2<- rbind(co2,a)}
  if(i==CO2ppmfiles[length(CO2ppmfiles)]){ rm(i,a,CO2ppmfiles)}
}


#Import CH4
CH4ppmfiles<-list.files(folder_results, pattern = "integrated_injections_CH4", recursive = T, full.names = T)

for(i in CH4ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4ppmfiles[1]){ch4<- a}else {ch4<- rbind(ch4,a)}
  if(i==CH4ppmfiles[length(CH4ppmfiles)]){ rm(i,a,CH4ppmfiles)}
}


#Format to join (create column gas and rename ppm)
n2o<- n2o %>% mutate(gas="n2o")
co2<- co2 %>% mutate(gas="co2")
ch4<- ch4 %>% mutate(gas="ch4")


#Join datasets
all<- rbind(n2o,co2, ch4)
rm(n2o,co2,ch4)



ggplot(A, aes(x=paste0(lambda,"_",asym_p),y=peakSNR))+
  geom_violin()


all %>% 
  group_by(lambda,asym_p, label,gas) %>% 
  summarise(avg_bc=mean(peaksum, na.rm=T),
            sd_bc=sd(peaksum, na.rm=T),
            cv_bc=sd_bc/avg_bc,
            avg_raw=mean(raw_peaksum, na.rm=T),
            sd_raw=sd(raw_peaksum,na.rm=T),
            cv_raw=sd_raw/avg_raw) %>% 
  mutate(cv_reduction=cv_raw-cv_bc,
         cv_relreduction=cv_bc/cv_raw) %>% 
  
  ggplot(aes(x=lambda,y=cv_bc, col=factor(asym_p)))+
  geom_violin(aes(group=factor(paste(lambda,asym_p))))+
  scale_x_log10()

all %>% 
  group_by(lambda,asym_p, label,gas) %>% 
  summarise(avg_bc=mean(peaksum, na.rm=T),
            sd_bc=sd(peaksum, na.rm=T),
            cv_bc=sd_bc/avg_bc,
            avg_raw=mean(raw_peaksum, na.rm=T),
            sd_raw=sd(raw_peaksum,na.rm=T),
            cv_raw=sd_raw/avg_raw) %>% 
  mutate(cv_reduction=cv_raw-cv_bc,
         cv_relreduction=cv_bc/cv_raw) %>%
  filter(cv_relreduction<30) %>% 
ggplot(aes(x=lambda,y=cv_relreduction, col=factor(asym_p)))+
  geom_boxplot(aes(group=factor(paste(lambda,asym_p))))+
  scale_x_log10()


all %>% 
  group_by(lambda,asym_p, label,gas) %>% 
  summarise(avg_bc=mean(peaksum, na.rm=T),
            sd_bc=sd(peaksum, na.rm=T),
            cv_bc=sd_bc/avg_bc,
            avg_raw=mean(raw_peaksum, na.rm=T),
            sd_raw=sd(raw_peaksum,na.rm=T),
            cv_raw=sd_raw/avg_raw) %>% 
  mutate(cv_reduction=cv_raw-cv_bc,
         cv_relreduction=cv_bc/cv_raw) %>%
  # filter(cv_relreduction<30) %>% 
  ggplot(aes(x=lambda,y=cv_bc, col=factor(asym_p)))+
  geom_boxplot(aes(group=factor(paste(lambda,asym_p))))+
  scale_x_log10()


all %>% 
  group_by(lambda,asym_p, label,gas) %>% 
  summarise(avg_bc=mean(peaksum, na.rm=T),
            sd_bc=sd(peaksum, na.rm=T),
            cv_bc=sd_bc/avg_bc,
            avg_raw=mean(raw_peaksum, na.rm=T),
            sd_raw=sd(raw_peaksum,na.rm=T),
            cv_raw=sd_raw/avg_raw) %>% 
  ungroup() %>% 
  mutate(avg_default=case_when(lambda==1e5&asym_p==1e-4~avg_bc, 
                           TRUE~NA_real_),
         cv_default=case_when(lambda==1e5&asym_p==1e-4~cv_bc, 
                               TRUE~NA_real_),) %>% 
  group_by(label,gas) %>% 
  mutate(avg_default=mean(avg_default,na.rm=T),
         cv_default=mean(cv_default, na.rm=T)) %>% 
  mutate(cv_reduction=cv_raw-cv_bc,
         cv_relreduction=cv_bc/cv_raw,
         avg_relreduction=(avg_raw-avg_bc)/avg_bc) %>%
  filter(lambda%in%c(1e5,10)) %>%
  ggplot(aes(x=factor(lambda),y=cv_default-cv_bc, col=factor(asym_p)))+
geom_boxplot()+
  facet_wrap(.~gas, scales="free")



all %>% 
  filter(grepl("6ppm|aire",label)) %>%
  separate(label, into=c("sampletype","extra"),sep = "-") %>% 
  mutate(standard=case_when(gas=="n2o"&sampletype=="6ppm"~6,
                            gas=="co2"&sampletype=="6ppm"~3000,
                            gas=="ch4"&sampletype=="6ppm"~15,
                            gas=="n2o"&sampletype=="aire"~0.340,
                            gas=="co2"&sampletype=="aire"~500,
                            gas=="ch4"&sampletype=="aire"~2)) %>% 
  ggplot(aes(x=paste(lambda,asym_p,sep="_"), y=peaksum/standard, col=sampletype))+
  geom_boxplot()+
  facet_wrap(.~gas, scales="free")
