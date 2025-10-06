lmsr_raw <- read_excel("~/SAP/SAP GUI/LMSR_diverse_Y1Y2.xlsx")


wegdamit <- c("Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time","Pages inclusive cover",
               "Deviations from analytical method" ,"UD code" )

lmsr <- lmsr_raw %>%
  mutate(month = str_extract(`IL short text`, "\\d+(?= ?Mt[ e.])"))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°C)"),"°C"))%>%  #str_c wie paste, aber besser für NA
  select(-all_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC ')) %>% 
  select(-contains(c("ICP-", "document", "DCP","action", "Pos ", "unknown", "article"))) %>%  #ohne ICP-Resultate 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  relocate(any_of(c("temp","month")), .after = Batch)  %>% 
  filter(Lev==1) %>% 
  filter(str_detect(`Inspection Lot`, "^89|^40|^10"))  %>%    #Nur Freigabe und Stabi
  filter(is.na(`IL short text`) | str_detect(`IL short text`, "OOS|Monitor|Muster", negate = T))  %>%  # NA sollen drin bleiben, REGEX nur für Text
  filter(is.na(`Material description`) | str_detect(`Material description`, "reproc", negate = T)) %>% 
  filter( `Inspection Lot` != "890000079607" & `Inspection Lot` != "890000079610" & `Inspection Lot` != "890000067629")

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



#----alte Daten von Excel------
lmsr_alt <- read_excel("LMSR_alt.xlsx")
lmsr_alt[,7:25] <- sapply(lmsr_alt[,7:25],as.numeric)

lmsr_comb<- bind_rows(lmsr, lmsr_alt)

lmsr_long <- lmsr_comb %>% 
  select(-c("Inspection Lot", "IL short text", "DOM", "date", "AN", "Purity", "IndivUK", "SumUK", "L-Mefolic_acid") ) %>% 
  pivot_longer(!c(Batch, month, temp), names_to = "analy_param", values_to = "value") %>% 
  mutate_at(vars(value), as.numeric)



lmsr_long %>% 
  filter(temp =="-20°C") %>% 
  filter(!analy_param %in% c("D-Mefox","Mefguan", "rRt 1.06" )) %>% 
  filter(parse_number(Batch) < 680 | parse_number(Batch) == 21600 ) %>% 
  # filter(!Batch %in% c("LMSR0685XX", "LMSR0686XX","LMSR0543XX" )) %>% 
  
  ggplot(aes(month, value, group= Batch, col= Batch)) +
  xlab("Months")+
  ylab(" % w/w")+
  geom_point(data=. %>% filter(parse_number(Batch) < 680),  aes(color=Batch), size=2)+
  geom_line(data=. %>% filter(parse_number(Batch) < 680),  aes(color=Batch), size=1.5, alpha=0.7)+ 
  scale_colour_grey()  +
  geom_point(data=. %>% filter(Batch == "21600T0001"),  color="red", size=2)+
  geom_line(data=. %>% filter(Batch  == "21600T0001"),  color="red", size=1.5, alpha=0.7)+ 
  geom_point(data=. %>% filter(Batch == "21600T0002"),  color="red3", size=2)+
  geom_line(data=. %>% filter(Batch  == "21600T0002"),  color="red3", size=1.5, alpha=0.7)+ 
  facet_wrap(~ analy_param, scales = "free", ncol=3 )+
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,1,3,6,9,12,18,24))+
  theme(legend.position = "bottom", legend.direction = "horizontal")


lmsr_long %>% 
  filter(temp =="-20°C") %>% 
  filter(!analy_param %in% c("D-Mefox","Mefguan", "rRt 1.06", "rRt 1.06", "L-Mefox", "Mefox", "rRt 1.16", "MeTHPA", "DHFA")) %>% 
  filter(parse_number(Batch) < 680 | parse_number(Batch) == 21600 ) %>% 
  
  ggplot(aes(month, value, group= Batch, col= Batch)) +
  xlab("Months")+
  ylab(" % w/w")+
  geom_point(data=. %>% filter(parse_number(Batch) < 680),  aes(color=Batch), color="#2dbecd", size=2)+
  geom_line(data=. %>% filter(parse_number(Batch) < 680),  aes(color=Batch), color="#2dbecd",size=1.5, alpha=0.7)+ 
  scale_colour_grey()  +
  geom_point(data=. %>% filter(Batch == "21600T0001"),  color="#eb3c96", size=2)+
  geom_line(data=. %>% filter(Batch  == "21600T0001"),  color="#eb3c96", size=1.5, alpha=0.7)+ 
  geom_point(data=. %>% filter(Batch == "21600T0002"),  color="#eb3c96", size=2)+
  geom_line(data=. %>% filter(Batch  == "21600T0002"),  color="#eb3c96", size=1.5, alpha=0.7)+ 
  facet_wrap(~ analy_param, scales = "free", ncol=3 )+
  scale_x_continuous(expand=c(0,0), limits=c(-1,13),breaks = c(0,1,2,3,6,9,12))+
  theme(legend.position = c(0.7,0.1), legend.direction = "horizontal")
