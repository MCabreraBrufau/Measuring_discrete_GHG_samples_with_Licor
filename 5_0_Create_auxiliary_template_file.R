#Calculate water concentration from headspace measurements

rm(list=ls())

#Library----
  library(tidyverse)

#Set folders paths----

#To test the repository functionality (with example data):
project_root<- paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/EXAMPLE_PROJECT")

#TO PROCESS YOUR OWN DATA, uncomment the following line and edit with the full path to your own your project folder (no closing "/"), eg:  

# project_root<- "C:/Users/User1/Documents/Licor-injections"


  #Folders
  folder_results<- paste0(project_root,"/Results_ppm")
  folder_auxfiles<- paste0(project_root,"/Auxiliary_files")
  
  #Check if the folder exist and, if not, create one
  if (!dir.exists(folder_auxfiles)) {
    # If it doesn't exist, create the folder
    dir.create(folder_auxfiles)
  }


#Import All_Injections_file data----
  Allinjectionfile<- list.files(path = folder_results, pattern = "^All_Injections_ppm") #Find summary results with all injections
  data <- read_csv(paste0(folder_results, "/", Allinjectionfile))
  

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

  