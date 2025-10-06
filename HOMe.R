HOMe_raw <- read_excel("LMCG0926XX-36XX, HOMeTHFA.xlsx")


HOMe <- HOMe_raw %>%
  mutate(month = case_when(str_detect(`Inspection Lot`,"^89") ~ str_c(str_extract(`IL short text`, "\\d+(?=[ ]{0,2}M(ona?)?t[ e.h,])")),
                           str_detect(`Inspection Lot`,"^16") ~ str_c(str_extract(`IL short text`, "^.{2}"))))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°C)"),"°C"))%>%  #str_c wie paste, aber besser für NA
  mutate(inert = case_when(
    str_detect(`IL short text`, "mit ") ~ "inert",
    str_detect(`IL short text`, "ohne ") ~ "not inert")) %>% 
  select(c("Lev","Batch","month","temp","inert","Inspection Lot", "IL short text","Assay Imp. HOMeTHFA", "Assay Mefox" )) %>% 
  filter(!str_detect(`Inspection Lot`, "89000017347") ) %>% 
  drop_na(`Assay Imp. HOMeTHFA`) %>% 
  mutate_at(vars(temp, inert), factor) %>%  #Faktorumwandlung
  mutate_at(vars(month,`Assay Imp. HOMeTHFA`, `Assay Mefox`), as.numeric) %>%  
  relocate(any_of(c("temp","month","inert")), .after = Batch) %>% 
  filter(Lev == 1) %>% 
  rename("HOMeTHFA"="Assay Imp. HOMeTHFA") %>% 
  rename("Mefox"="Assay Mefox")


# HOMe$`H2O free 5-MeTHFA` <- ifelse(is.na(HOMe$`H2O free 5-MeTHFA`), HOMe$`H2O- and solvent free 5-MeTHFA`, HOMe$`H2O free 5-MeTHFA`) #merge column
# HOMe <- HOMe[ , ! names(HOMe) %in% c("H2O- and solvent free 5-MeTHFA")] #remove column

HOMe$temp <- factor(HOMe$temp, levels = c("5°C", "25°C","40°C"))

HOMe$month <-replace_na(HOMe$month, 0) 

lev <- levels(HOMe$temp)  # Temperatur Levels
lev_inert=levels(HOMe$inert)

HOMe0<-HOMe %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-c(temp,inert)) %>%
  expand_grid(temp=lev) %>% 
  expand_grid(inert=lev_inert)

HOMe <- HOMe[!HOMe$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
HOMe <- rbind(HOMe,HOMe0)  # Kombinieren
HOMe$month <-as.integer(HOMe$month)


