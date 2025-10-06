lmcg_raw <- read_excel("~/SAP/SAP GUI/LMCG0890ZX.xlsx")

#Überflüssige Spalten
wegdamit <- c("Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant", "Material","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time",
              "UD code" )

lmcg <- lmcg_raw %>%
  mutate(month = str_extract(`IL short text`, "\\d+(?= ?Mt[ e.])"))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°C)"),"°C"))%>%  #str_c wie paste, aber besser für NA
  select(-all_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC ')) %>% 
  select(-contains("ICP-")) %>%  #ohne ICP-Resultate 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  mutate_at(vars(month), as.integer) %>%  #Faktorumwandlung
  relocate(any_of(c("temp","month")), .after = Batch)%>%
  filter(Lev == 1) 

lmcg$month <-replace_na(lmcg$month, 0) #Fehlende Monate gleich Initialwert

lev <- levels(lmcg$temp)  # Temperatur Levels

lmcg0<-lmcg %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-temp) %>%
  expand_grid(temp=lev)

lmcg <- lmcg[!lmcg$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
lmcg <- rbind(lmcg,lmcg0)  # Kombinieren
lmcg$month <-as.integer(lmcg$month)

# rename(mPAA, resid_evap = "Residue on Evaporation") # Umbenennung, falls nötig



