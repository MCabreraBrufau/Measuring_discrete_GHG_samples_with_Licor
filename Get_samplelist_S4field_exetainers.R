#Create single samplelist for Restore4Cs S4 field samples to be analysed with Licor 7820 for N2O data


#Packages
library(tidyverse)
library(readxl)


#Directories
datafolder<- "C:/Users/Miguel/Dropbox/Licor_N2O/"
folderfieldsheets<- "C:/Users/Miguel/Dropbox/RESTORE4Cs - Fieldwork/Data/GHG/Fieldsheets/"


#Import fieldsheets exetainers field samples

fieldsheets<-list.files(folderfieldsheets, pattern = "exetainers.xlsx", recursive = T, full.names = T)

for(i in fieldsheets){
  
  a<- read_xlsx(i, trim_ws = T)
  a<- a[!is.na(a[,1]),]
  
  if(i==fieldsheets[1]){A<- a}else {A<- rbind(A,a)}
}
rm(a,i)

names(A)[11]<- "exe_type"


S4exe<- A %>% filter(campaign=="S4")

S4headspace<- S4exe %>% filter(exe_type=="headspace")

write.csv(S4exe, file = paste0(datafolder, "Samplelist/S4field_exetainers.csv"),row.names = F)

write.csv(S4headspace, file = paste0(datafolder, "Samplelist/S4onlyheadspace_exetainers.csv"),row.names = F)
