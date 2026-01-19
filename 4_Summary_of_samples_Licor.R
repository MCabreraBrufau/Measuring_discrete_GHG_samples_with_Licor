#Summary of all N2O, CO2 and CH4 concentrations (ppm)

#This script loads all Licor-derived concentrations in Results_ppm folder and automatically produces 3 files:

#FILE All_Summary_ch4_co2_n2o_allinjections.csv: Contains average, sd and n for each gas without discarding any replicate injection.

#FILE All_Summary_ch4_co2_n2o_best3inj: Contains average, sd and n for each gas Taking into account for each sample the best 3 injections (the ones that result in the lowest CV). If discarding samples does not reduce the CV, the value reported is that without discarding injections. The number of injections is always reported

#FILE All_Injections_ppm_ch4_co2_n2o.csv: Contains the ppm calculated for every injection and every gas, keeping info of volume injected. 

#avg_ppm_GHG: mean concentration (ppm)
#sd_ppm_GHG: standard deviation (ppm)
#n_used_GHG: number of injections used for mean and standard deviation
#n_discarded_GHG: number of injections that were discarded (if any).


#Clean Global environment
rm(list=ls())


# ---- Directories ----

#To test the repository functionality (with example data):
project_root<- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/EXAMPLE_PROJECT")

#TO PROCESS YOUR OWN DATA, uncomment the following line and edit with the full path to your own your project folder (no closing "/"), eg:  

# project_root<- "C:/Users/User1/Documents/Licor-injections"



#Results folder:
folder_results<- paste0(project_root,"/Results_ppm/")


#Packages & functions ----
#Installs (if needed) and loads required packages:
required_pkgs <- c("tidyverse",
                   "readxl")

for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


#---1. Import & format----
#Import N2O
N2Oppmfiles<-list.files(folder_results, pattern = "^ppm_samples_N2O", recursive = T, full.names = T)

for(i in N2Oppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==N2Oppmfiles[1]){n2o<- a}else {n2o<- rbind(n2o,a)}
  if(i==N2Oppmfiles[length(N2Oppmfiles)]){ rm(i,a,N2Oppmfiles)}
}

#Import CO2
CO2ppmfiles<-list.files(folder_results, pattern = "^ppm_samples_CO2", recursive = T, full.names = T)

for(i in CO2ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CO2ppmfiles[1]){co2<- a}else {co2<- rbind(co2,a)}
  if(i==CO2ppmfiles[length(CO2ppmfiles)]){ rm(i,a,CO2ppmfiles)}
}

#Import CH4
CH4ppmfiles<-list.files(folder_results, pattern = "^ppm_samples_CH4", recursive = T, full.names = T)

for(i in CH4ppmfiles){
  a<- read_csv(i,show_col_types = F)
  if(i==CH4ppmfiles[1]){ch4<- a}else {ch4<- rbind(ch4,a)}
  if(i==CH4ppmfiles[length(CH4ppmfiles)]){ rm(i,a,CH4ppmfiles)}
}

#Join datasets
all<- rbind(n2o,co2, ch4)
rm(n2o,co2,ch4)




#2. Injection selection####

#Compare the CV obtained for each sample-gas-dayofanalaysis from: 

#1. Using all injections performed
#2. Using the best (most similar) 3 injections

#Compare CV and perform summary discarding when appropriate

#Function to determine the best 3 injections
select_lowest_cv <- function(measurements) {
  # Generate all combinations of 3 measurements
  combos <- combn(measurements, 3, simplify = FALSE)
  
  # Calculate the CV for each combination
  cvs <- sapply(combos, function(x) sd(x) / mean(x) * 100)
  
  # Select the combination with the lowest CV
  best_combo <- combos[[which.min(cvs)]]
  return(best_combo)
}


##Calculate summary with best 3 injections
using3best <- all %>%
  group_by(gas,sample,dayofanalysis) %>%
  mutate(total_injections=sum(!is.na(ppm))) %>% 
  filter(total_injections>3) %>% 
  reframe(
    ppm = select_lowest_cv(ppm)
  ) %>% 
  mutate(Selected = T) %>% 
  filter(Selected) %>% 
  group_by(gas,sample,dayofanalysis) %>% 
  summarise(avg3_ppm=mean(ppm),
            sd3_ppm=sd(ppm), 
            cv3=sd3_ppm/avg3_ppm,
            n3=sum(!is.na(ppm)))


##Calculate summary with all injections
usingall<- all %>% 
  group_by(gas,sample,dayofanalysis) %>% 
  summarise(avg_ppm=mean(ppm),
            sd_ppm=sd(ppm), 
            cv=sd_ppm/avg_ppm,
            nall=sum(!is.na(ppm)))


#3. Format and Save-----
#Combine and calculate summary for all or discarding (based on CV which provides a better CV)
combination<- usingall %>% 
  left_join(using3best) %>% 
  mutate(discarding=cv>cv3,
         discarding=if_else(is.na(discarding), F,T)) %>% 
  mutate(avg_ppm = if_else(discarding, avg3_ppm, avg_ppm),
         sd_ppm  = if_else(discarding, sd3_ppm, sd_ppm),
         n_used = if_else(discarding, n3, nall),
         n_discarded=if_else(discarding, nall-n3, 0)) %>% 
  select(gas,sample,dayofanalysis, avg_ppm, sd_ppm, n_used, n_discarded) %>% 
  pivot_wider(id_cols = c(sample, dayofanalysis), names_from = gas, names_sep = "_", values_from = c(avg_ppm, sd_ppm, n_used, n_discarded),names_sort = T) %>% 
  arrange(dayofanalysis,sample) %>% 
  select(dayofanalysis, sample, 
         avg_ppm_ch4, sd_ppm_ch4, n_used_ch4, n_discarded_ch4,
         avg_ppm_co2, sd_ppm_co2, n_used_co2, n_discarded_co2,
         avg_ppm_n2o, sd_ppm_n2o, n_used_n2o, n_discarded_n2o)


write.csv(combination, paste0(folder_results, "All_Summary_ch4_co2_n2o_best3inj.csv"), row.names = F)

#Provide general summary (all injections used)
summary_allinjections<- usingall %>% 
  mutate(n_used=nall, 
         n_discarded=0) %>% 
  select(gas,sample,dayofanalysis, avg_ppm, sd_ppm, n_used, n_discarded) %>% 
  pivot_wider(id_cols = c(sample, dayofanalysis), names_from = gas, names_sep = "_", values_from = c(avg_ppm, sd_ppm, n_used, n_discarded),names_sort = T) %>% 
  arrange(dayofanalysis,sample) %>% 
  select(dayofanalysis, sample, 
         avg_ppm_ch4, sd_ppm_ch4, n_used_ch4, n_discarded_ch4,
         avg_ppm_co2, sd_ppm_co2, n_used_co2, n_discarded_co2,
         avg_ppm_n2o, sd_ppm_n2o, n_used_n2o, n_discarded_n2o)


write.csv(summary_allinjections, paste0(folder_results, "All_Summary_ch4_co2_n2o_allinjections.csv"), row.names = F)


#Save all data per injection:
allinjections<- all %>% 
  select(gas, dayofanalysis, sample, ml_injected, peak_id, ppm)

write.csv(allinjections, paste0(folder_results, "All_Injections_ppm_ch4_co2_n2o.csv"), row.names = F)
