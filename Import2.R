LMCG_raw <- read_excel("LMCG0926XX-37XX_Y2.xlsx")

#Überflüssige Spalten
wegdamit <- c("Lev", "Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant", "Material","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time") 


LMCG <- LMCG_raw %>%
  filter(Lev==1) %>% 
  filter(!is.na(`UD code`) )%>% 
  select(-any_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC '),-contains("No QC")) %>% 
  select(-contains("ICP-")) %>%  #ohne ICP-Resultate   
  mutate(month = case_when(str_detect(`Inspection Lot`,"^89") ~ str_c(str_extract(`IL short text`, "\\d+(?=[ ]{0,2}M(ona?)?t[ e.h,])")),
                           str_detect(`Inspection Lot`,"^16") ~ str_c(str_extract(`IL short text`, "^.{2}"))))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°?C[ /,])"),"°C")) |>   #str_c wie paste, aber besser für NA
  mutate(inert = case_when(
    str_detect(`IL short text`, "5LMCG-3|5LMCG-4|mit|% inert") ~ "inert",
    TRUE ~ "not inert")) %>% 
  # filter(!is.na("UD code"))%>%  # Leere UD -> entfernen

  relocate(any_of(c("temp","month")), .after = Batch) %>% 
  # filter(str_detect(`Inspection Lot`, "^40|^89")) %>%    #Nur Freigabe und Stabi IL (mit Temp!)
  # filter(!(str_detect(`Inspection Lot`, "^89") & is.na(temp)) ) %>%
# select(c("Lev","Batch","month","temp","inert","Inspection Lot", "IL short text","pH" )) %>% 
  mutate_at(vars(temp,inert), factor) %>%  #Faktorumwandlung
  mutate_at(vars(month), as.numeric) %>%  
  relocate(any_of(c("temp","month","inert")), .after = Batch) 

# LMCG$inert[LMCG$`Inspection Lot`=="160000026252"] <- "not inert"

LMCG$temp <- factor(LMCG$temp, levels = c("5°C", "25°C","40°C"))

LMCG$month <- as.integer(LMCG$month)

LMCG$month <-replace_na(LMCG$month, 0) #Fehlende Monate gleich Initialwert

lev1 <-levels(droplevels(LMCG$temp)) # droplevels entfernt unbenutzte Level, die sonst irgendwie drin bleiben....
lev2 <-levels(droplevels(LMCG$inert))

LMCG0<-LMCG %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-c(temp, inert)) %>%
  expand_grid(temp=lev1) |>
  expand_grid(inert=lev2)

LMCG <- LMCG[!LMCG$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
LMCG_fertig <- rbind(LMCG,LMCG0)  # Kombinieren
LMCG_fertig$month <-as.integer(LMCG_fertig$month)

LMCG <-LMCG_fertig


LMCG_selection <- LMCG |> 
  select(Batch, temp, month, inert, `Inspection Lot`, `IL short text`, `Assay 5-MeTHFA`, 
         `Assay 5-MeTHFA-Ca`, "Assay Imp. ABGA...112",  "Assay Imp. HOMeTHFA", 
         "Assay Imp. D-Mefox", "Assay Imp. L-Mefox", "Assay Mefox", "Assay Imp. Mefdiamid", 
         "Assay Imp. THFA...140", "Assay Imp. Mefguan", 
          "Assay Imp. DHFA...156", "Assay Imp. FA...164", "Assay Imp. CH2THFA...168", 
         "Assay Imp. rRt 1.06", "Assay Imp. MeTHPA...180", 
         "Assay Imp. rRt 1.16", "Assay Imp. DiMeTHFA", "Assay Imp. sum unknown", 
         "Individual ORC", "Individual RC", "Sum ORC", "Sum ORC + RC") 

spalten <- colnames(LMCG_selection)
spalten <- gsub("\\.{3}\\d{1,3}","",spalten) #3 Punkte erkennen sowie 1-3 Ziffern
spalten <- gsub("\\Assay ?","",spalten) #Assay entfernen
spalten <- gsub("\\Imp. ","",spalten) #Imp. entfernen
names(LMCG_selection) <- spalten #neue, korrigierte Namen

LMCG_long<- LMCG_selection %>% 
  # select(1:26) %>% # ohne MiBi
   select(-c("Inspection Lot","IL short text")) %>% 
  pivot_longer(!c(Batch, month,temp, inert, ), names_to = "analy_param", values_to = "value") |>     # alle zu long ausser months und temp
  mutate_at(vars(value), as.numeric)

