#Calculate water concentration from headspace measurements

#Library----
  library(tidyverse)

#Set folders paths----
  #You can work on your working directory (not recommended)
  #folder_root <- dirname(rstudioapi::getSourceEditorContext()$path)
  #You can set the folder in other path
  folder_root <- "/home/jorge/Documentos/Postdoctoral/Onedrive_UB/UB/NaturBPond/Hidrogeologos/Muestras_Licor" # You have to make sure this is pointing to the write folder on your local machine
  
  #Folders
  folder_results<- paste0(folder_root,"/Results_ppm")
  folder_auxfiles<- paste0(folder_root,"/Auxiliary_files")
  
  #Check if the folder exist and, if not, create one
  if (!dir.exists(folder_auxfiles)) {
    # If it doesn't exist, create the folder
    dir.create(folder_auxfiles)
  }

##I think this is not necessary because there is just one file for all samples
# #Create empty auxiliary template file for all the All_injetions_ppm files----
#   Allinjectionfiles<- list.files(path = folder_results, pattern = "^All_Injections_ppm") #Find summary results with all injections
#   auxfiles<- list.files(path = folder_auxfiles, pattern = "auxiliary_template") #Find auxiliary templates files
#   auxfiles <- auxfiles[!grepl("filled", auxfiles)] #Ignore the templates filled
# 
#   #All_injection files without auxiliary template file
#   createAUXfile<- gsub(".csv","",gsub("integrated_injections_","",integratedfiles[
#     !gsub(".csv","",gsub("integrated_injections_","",integratedfiles))%in%gsub(".csv","",gsub("^.*ppm_samples_","",ppmfiles))]))#  integrated files "rawcode" without corresponding ppmfiles "rawcode"


#Import All_Injections_file data----
  Allinjectionfile<- list.files(path = folder_results, pattern = "^All_Injections_ppm") #Find summary results with all injections
  data <- read_csv(paste0(folder_results, "/", Allinjectionfile))
  
# #Remove 6ppm and air patterns----(/quizas merece la pena dejarlo como ejemplos de ignore)
#   data <-  data %>%
#     filter(!str_detect(sample, "6ppm|aire"))
  
  
#Create empty auxiliary template file for all the All_injetions_ppm files----
  #Create template for auxiliary data to calculate water concentration
  auxtemplate <- data.frame(SampleID = unique(data$sample),
                            Sample_type = NA,
                            LinkID = NA,
                            Temp = NA,
                            Vol_air = NA,
                            Vol_water = NA,
                            Pressure = NA)
  
  auxtemplate <- auxtemplate %>% mutate(Sample_type = case_when(str_detect(SampleID, "6ppm|aire") ~ "Ignore"))
  
  #Save the template
  write_csv(auxtemplate, paste0(folder_auxfiles, "/HeadSpace_auxiliary_template.csv"))
  warning("Now you must edit the HeadSpace_auxiliary_template.csv file by adding the information necessary to calculate water concentration, and save it with the word \"filled\" added to the filename, as follows: HeadSpace_auxiliary_template_filled.csv")

  