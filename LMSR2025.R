lmsr_raw <- read_excel("../../../SAP/SAP GUI/LMSR_Y2L2.xlsx")


wegdamit <- c("Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time",
               "UD code" )

lmsr <- lmsr_raw %>%
  mutate(month = case_when(str_detect(`Inspection Lot`,"^89") ~ str_c(str_extract(`IL short text`, "\\d+(?=[ ]{0,2}M(ona?)?t[ e.h,])")),
                           str_detect(`Inspection Lot`,"^16") ~ str_c(str_extract(`IL short text`, "^.{2}"))))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°C)"),"°C"))%>%  #str_c wie paste, aber besser für NA
  select(-all_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC ')) %>% 
  select(-contains(c("ICP-", "document", "DCP","action", "Pos ", "unknown", "article"))) %>%  #ohne ICP-Resultate 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  relocate(any_of(c("temp","month")), .after = Batch)  %>% 
  filter(Lev==1) %>% 
  filter(str_detect(`Inspection Lot`, "^89|^40|^10|^16"))  %>%    #Nur Freigabe und Stabi
  filter(is.na(`IL short text`) | str_detect(`IL short text`, "OOS|Monitor|Muster", negate = T))  %>%  # NA sollen drin bleiben, REGEX nur für Text
  filter(is.na(`Material description`) | str_detect(`Material description`, "reproc", negate = T)) %>% 
  filter( `Inspection Lot` != "890000137176" & `Inspection Lot` != "890000137177" ) |> 
  mutate_at(vars(month), as.integer)
# 890000079607   890000079610  890000067629
lmsr$month <-replace_na(lmsr$month, 0)

lev <-levels(droplevels(lmsr$temp)) # droplevels entfernt unbenutzte Level, die sonst irgendwie drin bleiben....

lmsr0<-lmsr %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-temp) %>%
  expand_grid(temp=lev)

lmsr <- lmsr[!lmsr$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
lmsr_fertig <- rbind(lmsr,lmsr0)  # Kombinieren
lmsr_fertig$month <-as.integer(lmsr_fertig$month)
lmsr <- lmsr_fertig

lmsr <- lmsr %>% 
  select("Batch", "temp", "month", "Inspection Lot", "IL short text",
         "Assay 5-MeTHFA","Assay Imp. ABGA", "Assay Imp. HOMeTHFA", 
         "Assay Imp. D-Mefox", "Assay Imp. L-Mefox", "Assay Mefox", "Assay Imp. Mefdiamid", 
         "Assay Imp. THFA", "Assay Imp. Mefguan", "Assay Imp. DHFA",  
         "Assay Imp. FA","Assay Imp. CH2THFA","Assay Imp. rRt 1.06", "Assay Imp. MeTHPA", 
         "Assay Imp. rRt 1.16", "Assay Imp. DiMeTHFA","Sum ORC + RC" ) %>% 
  rename("5-MeTHFA" ="Assay 5-MeTHFA", 
         "ABGA" ="Assay Imp. ABGA", 
         "HOMeTHFA" ="Assay Imp. HOMeTHFA", 
         "D-Mefox" ="Assay Imp. D-Mefox", 
         "L-Mefox" ="Assay Imp. L-Mefox", 
         "Mefox" ="Assay Mefox", 
         "Mefdiamid" ="Assay Imp. Mefdiamid", 
         "THFA" ="Assay Imp. THFA", 
         "Mefguan" ="Assay Imp. Mefguan", 
         "DHFA" ="Assay Imp. DHFA", 
         "FA" ="Assay Imp. FA", 
         "CH2THFA" ="Assay Imp. CH2THFA", 
         "rRt 1.06" ="Assay Imp. rRt 1.06", 
         "MeTHPA" ="Assay Imp. MeTHPA", 
         "rRt 1.16" ="Assay Imp. rRt 1.16", 
         "DiMeTHFA" ="Assay Imp. DiMeTHFA",
         "SumImp" = "Sum ORC + RC" )

lmsr[,6:22] <- sapply(lmsr[,6:22],as.numeric)

lmsr$temp <- factor(lmsr$temp, levels = c("-20°C" ,"5°C" ,"25°C","40°C"  ))


#----- remove redundant 0 Mte results for temperatures without subseqeunt timepoints ------
redundant_0Mte <-lmsr |> 
  group_by(Batch, temp) |> 
  #summarise( n=n_distinct(month), IL = list(`Inspection Lot`)) |> 
  summarise( n=n_distinct(month), `Inspection Lot`) |> 
  filter(n==1) |> 
  select(-c(n))
# as.vector(unnest(test[1,4], cols = c(IL)))

lmsr <- lmsr |> anti_join(redundant_0Mte)


#---- convert to long ----------
lmsr_long <- lmsr %>% 
  select(-c("Inspection Lot", "IL short text") ) %>% 
  pivot_longer(!c(Batch, month, temp), names_to = "analy_param", values_to = "value") %>% 
  mutate_at(vars(value), as.numeric)

