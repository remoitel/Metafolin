# ---- neu in Sept 2023

meta_raw <- read_excel("C:/Users/M291711/OneDrive - MerckGroup/SAP/SAP GUI/Metafolin_Oct23.xlsx")

wegdamit <- c("Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant", "Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time", "UD code",
              "Chloride Content", "Limit testing / Heavy Metals" ,"Assay Typ 3 (UV)",
              "Escherichia coli","Staphylococcus aureus","Pseudomonas aeruginosa", "Absorbance 420 nm (4cm)","Masterbatch approved" )

meta <- meta_raw %>%
  mutate(month = str_extract(`IL short text`, "\\d+(?=[ ]{0,2}M(ona?)?t[ e.h,])"))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°C)"),"°C"))%>%  #str_c wie paste, aber besser für NA
  select(-all_of(wegdamit)) %>%
  select(-c("Water content...212","Assay H2O and solv. free 5-MeTHFA-Ca...288","Assay H2O free 5-MeTHFA-Ca...664",
            "Assay Imp. DHFA...612" ,"Assay Imp. FA...616","Assay Imp. CH2THFA...620" ,"Assay Imp. MeTHPA...624" ,  
            "Assay Imp. ABGA...604" ,"Assay Imp. THFA...608")) %>% 
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC ')) %>% 
  rename("Purity 5-MeTHFA"= "Area% 5-MeTHFA") %>%  
  select(-contains(c("ICP-", "document", "DCP","action", "Pos ", "unknown", "article", "review", "identity", 
                     "No QC", "Pages", "Rinse", "Content accuracy", "Absorbtion", "Response", "Area% "))) %>%  #ohne ICP-Resultate 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  mutate_at(vars(month), as.numeric) %>%  
  relocate(any_of(c("temp","month")), .after = Batch) %>% 
  filter(str_detect(`Inspection Lot`, "^89|^40")) %>% #Nur Freigabe und Stabi
  filter(!str_detect(Batch, "^VALIDUM")) %>% 
  # filter(!str_detect(`Material description`, "Seed")) %>% 
  # filter(!str_detect(`IL short text`, "Sulfit|IPK")) %>% 
  filter(Lev == 1) %>% 
  rename(`Water content`= `Water content...208`,
         "Assay H2O and solv. free 5-MeTHFA-Ca" = "Assay H2O and solv. free 5-MeTHFA-Ca...284",
         "Assay H2O free 5-MeTHFA-Ca" = "Assay H2O free 5-MeTHFA-Ca...660") %>%
  rename_with( ~ gsub("Assay Imp. ", "", .x, fixed = TRUE)) %>% 
  rename_with( ~ gsub("Assay ", "", .x, fixed = TRUE)) %>% 
  select(-c(46:54)) %>% 
  # select(-c(46:47))
  # select(order(colnames(meta)))
  rename_with( ~ gsub("\\.{3}\\d{1,3}","", .x)) %>% 
  mutate_at(vars(c(11:47)), as.numeric) %>%  # es wird nicht alles numerisch....
  mutate(Solvents = Ethanol+Isopropanol) %>% # Methanol und Aceton ist überall na oder 0 (aber nicht numerisch.....)
  mutate(`5-MeTHFA-Ca water free calc` = `5-MeTHFA-Ca`*100/(100-(`Water content`))) %>%  # only Water free
  relocate(any_of(c("H2O and solv. free 5-MeTHFA-Ca")), .after = "5-MeTHFA-Ca water free calc" ) %>% 
  filter(!(`Inspection Lot`%in% c("890000068513", "890000068515", "890000068632", "890000068630","890000069130","890000068634", 
                                  "890000068636", "890000009012", "890000010065", "890000016161", "890000021124","890000065410",
                                  "890000121982", "890000090620", "40000283115", "890000128938", "40000043442","40000087133",
                                  "890000041434", "890000106803", "890000113450", "40000347168", "40000124598", "890000105512",
                                  "890000106810","890000106812","890000106815", "40000344337", "890000123622", "890000123623" )))

# meta$`H2O free 5-MeTHFA` <- ifelse(is.na(meta$`H2O free 5-MeTHFA`), meta$`H2O- and solvent free 5-MeTHFA`, meta$`H2O free 5-MeTHFA`) #merge column
# meta <- meta[ , ! names(meta) %in% c("H2O- and solvent free 5-MeTHFA")] #remove column
  
meta$temp <- factor(meta$temp, levels = c("-20°C", "5°C", "25°C","40°C"))

meta$month <-replace_na(meta$month, 0) 

lev <- levels(meta$temp)  # Temperatur Levels

meta0<-meta %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-temp) %>%
  expand_grid(temp=lev)

meta <- meta[!meta$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
meta <- rbind(meta,meta0)  # Kombinieren
meta$month <-as.integer(meta$month)

meta_long <- meta %>% 
  select(-c(`Inspection Lot`, `IL short text`,Lev, `Material`, `Material description`, `Appearance color`, `Appearance texture`)) %>% 
  # select(-c(`Water content`)) %>% 
  pivot_longer(!c(Batch,temp, month), names_to = "analy_param", values_to = "value") %>% 
  drop_na(value) %>% 
  mutate_at(vars(value), as.numeric)

