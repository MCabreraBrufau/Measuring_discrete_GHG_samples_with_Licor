#Calculate water concentration from headspace measurements

#You need to have filled the auxfile with the appropriate details (see 5_0_Create_auxiliary_template_file.R)

#Clean Global environment
rm(list=ls())

#Directories ----

#To test the repository functionality (with example data):
project_root<- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/EXAMPLE_PROJECT")

#TO PROCESS YOUR OWN DATA, uncomment the following line and edit with the full path to your own your project folder (no closing "/"), eg:  

# project_root<- "C:/Users/User1/Documents/Licor-injections"


#Folders
folder_results<- paste0(project_root,"/Results_ppm")
folder_auxfiles<- paste0(project_root,"/Auxiliary_files")


# Packages & functions ----
library(tidyverse)
library(errors)

#Import functions of repo 
repo_root <- dirname((rstudioapi::getSourceEditorContext()$path))
files.sources = list.files(path = paste0(repo_root,"/functions"), full.names = T)
for (f in files.sources){source(f)}



#Import data----
Allinjectionfile<- list.files(path = folder_results, pattern = "^All_Injections_ppm") #Find summary results with all injections
data <- read_csv(paste0(folder_results, "/", Allinjectionfile),show_col_types = FALSE)
data <- data %>% rename(SampleID = sample)


#Import auxiliary file----
Auxfile<- list.files(path = folder_auxfiles, pattern = "filled") #Find summary results with all injections
aux <- read_csv(paste0(folder_auxfiles, "/", Auxfile),show_col_types = FALSE)

##Check missed samples----
missed_inj <- setdiff(aux$SampleID, data$SampleID) #samples missed in injections
missed_aux <- setdiff(data$SampleID, aux$SampleID) #Samples missed in auxfile

if(length(missed_inj) > 0){
  warning(paste("Warning: Missed samples in injections:", paste(missed_inj, collapse = ", ")))
}

if(length(missed_aux) > 0){
  warning(paste("Warning: Missed samples in auxiliary file:", paste(missed_aux, collapse = ", ")))
}

#Merge with injections
data_aux <- data %>% left_join(aux)

#Split headspace samples and atm references----
atm_ref <- data_aux %>% filter(Sample_type == "AtmRef") %>% select(-Sample_type) 
if(all(is.na(unique(data_aux$LinkID)))){
  warning(paste("Warning: No LinkID provided to use your own atmospheric reference values"))
  atm_ref <- atm_ref %>% summarise(SampleID = unique(SampleID),
                                   avg_ppm_reference = NA,
                                   sd_ppm_reference = NA)
}else{
  atm_ref <- atm_ref %>% group_by(gas, LinkID) %>% 
    summarise(avg_ppm_reference = mean(ppm, na.rm = T),
              sd_ppm_reference = sd(ppm, na.rm = T)) %>% 
    ungroup()
}


headspace <- data_aux %>% filter(Sample_type == "HS") %>% select(-Sample_type)

##Check if all LinkID are provided----

  missed_LinkID_HS <- setdiff(atm_ref$LinkID, headspace$LinkID) #LinkID missed in headspace samples
  missed_LinkID_AtmRef <- setdiff(headspace$LinkID, atm_ref$LinkID) #LinkID missed in headspace atmosphere reference
  
  if(length(missed_inj) > 0){
    warning(paste("Warning: Some headspace samples do not have a LinkID assigned"))
  }
  
  if(length(missed_aux) > 0){
    warning(paste("Warning: Some atmospheric reference entries do not correspond to any sample"))
  }

#Merge----
df <- headspace %>% left_join(atm_ref)

#Set sd for atmosphere reference to propagate the error----
df <- df %>% mutate(avg_ppm_reference = set_errors(avg_ppm_reference, sd_ppm_reference))

#Caculate concentration uM in the water----
df <- df %>% mutate(water_uM = case_when(gas == "co2" ~ nGHG_water_uM("CO2", ppm, Vol_H2O = Vol_water, Vol_air = Vol_air, T_Celsius = Temp, Patm_eq = Pressure, R = 0.08206, GHG_atm_ppmv = avg_ppm_reference),
                                         gas == "ch4" ~ nGHG_water_uM("CH4", ppm, Vol_H2O = Vol_water, Vol_air = Vol_air, T_Celsius = Temp, Patm_eq = Pressure, R = 0.08206, GHG_atm_ppmv = avg_ppm_reference),
                                         gas == "n2o" ~ nGHG_water_uM("N2O", ppm, Vol_H2O = Vol_water, Vol_air = Vol_air, T_Celsius = Temp, Patm_eq = Pressure, R = 0.08206, GHG_atm_ppmv = avg_ppm_reference)))
df <- df %>% mutate(water_uM_sd = errors(water_uM))

#Caculate concentration uatm in the water----
df <- df %>% mutate(water_uatm = case_when(gas == "co2" ~ water_uM/Kh_CO2(Temp),
                                           gas == "ch4" ~ water_uM/Kh_CH4(Temp),
                                           gas == "n2o" ~ water_uM/Kh_N2O(Temp)))                    

df <- df %>% mutate(water_uatm_sd = errors(water_uatm))

#Add flags
df <-df %>% mutate(Flags = str_trim(paste(
  ifelse(is.na(avg_ppm_reference), 
         "Missing atmospheric reference sample", 
         ""),
  ifelse(as.numeric(water_uM) < 0, 
         "Unrealistic values — check for incorrect data or atmospheric references higher than expected or headspace concentration extremely low", 
         ""),
  sep = "; "
)))

#Export results all injections----
folder_results_headspace <- paste0(project_root,"/Results_headspace")
#Check if the folder exist and, if not, create one
if (!dir.exists(folder_results_headspace)) {
  # If it doesn't exist, create the folder
  dir.create(folder_results_headspace)
}
write_csv(df, paste0(folder_results_headspace, "/Results_headspace_water_concentration_allinjections.csv"))

#Summary with maximum three best injections
#Select max 3 injections with the lower CV for each volume-dilution treatment----
# Function to find the best combination for a group
select_lowest_cv <- function(measurements) {
  # Generate all combinations of 3 measurements, if not, use all of them
  if(length(measurements)< 3){
    n <- length(measurements)
  }else{
    n <- 3
  }
  combos <- combn(measurements, n, simplify = FALSE)
  
  # Calculate the CV for each combination
  cvs <- sapply(combos, function(x) sd(x) / mean(x) * 100)
  
  # Select the combination with the lowest CV
  if(length(measurements) == 1){
    best_combo <-combos[[1]]
  }else{
    best_combo <- combos[[which.min(cvs)]]
  }
  return(best_combo)
}
#Create a df with the combination selected
result <- df %>%
  group_by(SampleID, gas) %>%
  reframe(
    ppm = select_lowest_cv(ppm)
  ) %>% 
  mutate(Selected = "Yes") %>%  #This is to track the best combination after join
  ungroup()
#Join with data
df_sum <- df %>% left_join(result)
df_sum <- df_sum %>% 
  filter(Selected == "Yes")

df_sum <- df_sum %>% group_by(SampleID, gas) %>% 
  summarise(avg_ppm = mean(ppm),
            sd_ppm = sd(ppm),
            avg_ppm_reference = first(avg_ppm_reference),
            sd_ppm_reference = first(sd_ppm_reference),
            water_uM = mean(water_uM),
            water_uatm = mean(water_uatm),
            n = length(ppm))

df_sum <- df_sum %>% mutate(water_uM_sd = errors(water_uM),
                          water_uatm_sd = errors(water_uatm)) %>% 
  relocate(water_uM_sd, .after = water_uM) %>% 
  relocate(water_uatm_sd, .after = water_uatm)

#Export result summary----
write_csv(df_sum, paste0(folder_results_headspace, "/Results_headspace_water_concentration_summary.csv"))
