library(readxl)
library(tidyverse)
library(ggforce)
library(cowplot)
library(directlabels)


meta_raw <- read_excel("~/SAP/SAP GUI/Metafolin3.xlsx")


meta <- meta_raw %>% 
  select("Lev", "Batch"  ,"IL short text" , "Inspection Lot","Assay Imp. HOMeTHFA","Water content","Assay 5-MeTHFA",
         "Assay Imp. Mefguan","Assay Imp. Mefdiamid","Assay Mefox", "Assay Imp. DiMeTHFA", "Assay Imp. DHFA...296" ,"Assay Imp. MeTHPA...328",
         "Assay Imp. CH2THFA...312","Assay Imp. rRt 1.06", "Assay Imp. ABGA...260" ) %>% 
  mutate(month = str_extract(`IL short text`, "\\d+(?= ?Mt[ e.])"))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°?C[ /])"),"°C")) %>%  #str_c wie paste, aber besser für NA
 # filter(Lev == 2) %>% 
  drop_na("Assay Imp. HOMeTHFA") %>% 
  rename("HOMeTHFA"="Assay Imp. HOMeTHFA")
  #filter((str_detect(`Inspection Lot`, "^89") & !is.na(temp)) |str_detect(`Inspection Lot`, "^40") ) %>% 
  #filter(temp=="5°C")


  #relocate(any_of(c("temp","months")), .after = Batch)

meta$month <-replace_na(meta$month, 0)
meta$month <- as.numeric(meta$month)  
meta$HOMeTHFA <- as.numeric(meta$HOMeTHFA)  
  
  
meta_mean<-meta %>% 
  filter(Lev ==2) %>% 
  group_by(Batch, month) %>% 
  summarise(mean =mean(HOMeTHFA), min=min(HOMeTHFA), max=max(HOMeTHFA))


#--------------
meta2 <- meta %>%
  filter(Lev=="1") %>% 
  select(!c("IL short text" , "Inspection Lot", "Lev"))

meta2[3:15]<-sapply(meta2[3:15],as.numeric)
  
library(GGally)  

ggpairs(meta2)  
  