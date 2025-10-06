
LMCG_raw <- read_excel("LMCG0939XX_60C.xlsx") #only Y1

#Überflüssige Spalten
wegdamit <- c("Lev", "Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant", "Material","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time") 


LMCG <- LMCG_raw %>%
  filter(Lev==1) %>% 
  filter(!is.na(`UD code`) )%>% 
  select(-any_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC '),-contains("No QC")) %>% 
  select(-contains("ICP-")) %>%  #ohne ICP-Resultate   
  mutate(days = case_when(str_detect(`Inspection Lot`,"^89") ~ str_c(str_extract(`IL short text`, "\\d+(?=[ ]{0,2}Tage)")),
                           str_detect(`Inspection Lot`,"^16") ~ str_c(str_extract(`IL short text`, "^.{2}"))))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°?C[ /,])"),"°C")) |>   #str_c wie paste, aber besser für NA
  
  relocate(any_of(c("temp","days")), .after = Batch) %>% 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  mutate_at(vars(days), as.numeric) %>%  
  relocate(any_of(c("temp","days")), .after = Batch) 


LMCG_selection <- LMCG |> 
  select(Batch, temp, days, `Inspection Lot`,  "Appearance color", "Appearance texture","Water content" ,"Assay H2O free 5-MeTHFA-Ca...52",
         "Assay Imp. ABGA...60",  "Assay Imp. HOMeTHFA", 
         "Assay Mefox", "Assay Imp. Mefdiamid",  "Assay Imp. THFA...88", "Assay Imp. Mefguan", 
         "Assay Imp. DHFA...100", "Assay Imp. FA...108", "Assay Imp. CH2THFA...116", 
         "Assay Imp. rRt 1.06", "Assay Imp. MeTHPA...128", 
         "Assay Imp. rRt 1.16", "Assay Imp. DiMeTHFA",  "Individual ORC",  "Sum ORC + RC") |> 
  mutate_at(7:23, as.numeric) 
  # mutate(`5-MeTHFA-Ca water free calc` = `Assay 5-MeTHFA-Ca`*100/(100-(`Water content`)))  # only Water free

# LMCG_selection_StringResults <- LMCG |> 
  # select(Batch, temp, days, `Inspection Lot`, `IL short text`, `Assay 5-MeTHFA`, 
         # `Assay 5-MeTHFA-Ca`, "Appearance color", "Appearance texture")

spalten <- colnames(LMCG_selection)
spalten <- gsub("\\.{3}\\d{1,3}","",spalten) #3 Punkte erkennen sowie 1-3 Ziffern
spalten <- gsub("\\Assay ?","",spalten) #Assay entfernen
spalten <- gsub("\\Imp. ","",spalten) #Imp. entfernen
names(LMCG_selection) <- spalten #neue, korrigierte Namen

LMCG_long<- LMCG_selection %>% 
  # select(1:26) %>% # ohne MiBi
  select(-c("Inspection Lot","IL short text")) %>% 
  pivot_longer(!c(Batch, days,temp ), names_to = "analy_param", values_to = "value") |>     # alle zu long ausser months und temp
  mutate_at(vars(value), as.numeric)

