meta_raw <- read_excel("~/SAP/SAP GUI/LMCG0623-0867.xlsx")


wegdamit <- c("Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant", "Material","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time","Pages inclusive cover",
              "Retain Sample Chem Created" ,"Analytical processing time","UD code" )

meta <- meta_raw %>%
  # mutate(month = str_extract(`IL short text`, "\\d+(?= ?Mt[ e.])"))%>%
  # mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°C)"),"°C"))%>%  #str_c wie paste, aber besser für NA
  select(-all_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC ')) %>% 
  select(-contains(c( "document", "DCP","action", "Pos ", "unknown", "article"))) %>%  #ohne ICP-Resultate 
  # mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  # relocate(any_of(c("temp","month")), .after = Batch) %>% 
  filter(str_detect(`Inspection Lot`, "^40")) %>%  #Nur Freigabe und Stabi
  mutate(BatchDigit = parse_number(Batch)) %>% 
  mutate(external = BatchDigit >841)


meta <- meta %>% 
  select("Batch", "external", "Inspection Lot", "Water content","Chloride Content",
         "Assay 5-MeTHFA", "Assay 5-MeTHFA-Ca", "Assay H2O- and solvent free 5-MeTHFA", "Assay H2O and solv. free 5-MeTHFA-Ca...596",
         "Assay Imp. ABGA...632", "Assay Imp. HOMeTHFA", 
         "Assay Imp. D-Mefox", "Assay Imp. L-Mefox", "Assay Mefox", "Assay Imp. Mefdiamid", 
         "Assay Imp. THFA...744", "Assay Imp. Mefguan", "Assay Imp. DHFA...784",  
         "Assay Imp. FA...808","Assay Imp. CH2THFA...832","Assay Imp. rRt 1.06", "Assay Imp. MeTHPA...880", 
         "Assay Imp. rRt 1.16", "Assay Imp. DiMeTHFA","Sum ORC + RC" ,"Assay Isopropanol","Assay Ethanol",
         "Assay Sulfur (ICP-OES)") %>% 
  rename("5-MeTHFA" ="Assay 5-MeTHFA", 
         "5-MeTHFA-Ca" ="Assay 5-MeTHFA-Ca", 
         "Assay" ="Assay H2O and solv. free 5-MeTHFA-Ca...596", 
         "ABGA" ="Assay Imp. ABGA...632", 
         "HOMeTHFA" ="Assay Imp. HOMeTHFA", 
         "D-Mefox" ="Assay Imp. D-Mefox", 
         "L-Mefox" ="Assay Imp. L-Mefox", 
         "Mefox" ="Assay Mefox", 
         "Mefdiamid" ="Assay Imp. Mefdiamid", 
         "THFA" ="Assay Imp. THFA...744", 
         "Mefguan" ="Assay Imp. Mefguan", 
         "DHFA" ="Assay Imp. DHFA...784", 
         "FA" ="Assay Imp. FA...808", 
         "CH2THFA" ="Assay Imp. CH2THFA...832", 
         "rRt 1.06" ="Assay Imp. rRt 1.06", 
         "MeTHPA" ="Assay Imp. MeTHPA...880", 
         "rRt 1.16" ="Assay Imp. rRt 1.16", 
         "DiMeTHFA" ="Assay Imp. DiMeTHFA",
         "SumImp" = "Sum ORC + RC",
         "Isopropanol" = "Assay Isopropanol" ,
         "Ethanol" = "Assay Ethanol",
         "Sulfur" ="Assay Sulfur (ICP-OES)")



meta_long <- meta %>% 
  pivot_longer(!c(Batch, "Inspection Lot", "external"), names_to = "analy_param", values_to = "value") %>% 
  mutate_at(vars(value), as.numeric)


extrakt <- meta_long %>% 
  filter(!is.na(value)) %>% 
  # filter(month < 7) %>% 
  filter(value <2.5) 

ggplot(extrakt, aes(analy_param, value))+
  ylab("conc. [% w/w]")+
  xlab("Impurity")+
  ylim(0,0.4)+
  geom_boxplot()+
  geom_jitter(data=extrakt %>% filter(`Batch` != "LMCG0843XX"), aes(analy_param, value), size=2, width = 0.1 )+
  geom_point(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(analy_param, value), color="red", size=4)

meta_long %>% 
  filter(!is.na(value)) %>% 
  filter(!str_detect(analy_param, "5-MeTHFA") ) %>% 
ggplot(aes(analy_param, value, fill=external))+
  ylab("conc. [% w/w] (ppm for Sulfur)")+
  xlab("Impurity")+
  # ylim(0,0.4)+
  geom_boxplot()+
  # geom_jitter(data=extrakt %>% filter(`Batch` != "LMCG0843XX"), aes(analy_param, value), size=2, width = 0.1 )+
  # geom_point(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(analy_param, value), color="red", size=4)+
  facet_wrap(~ analy_param, scales = "free", ncol=4 )+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_fill_manual(values=c("#2dbecd","#eb3c96"))+
  theme(legend.position = c(0.7,0.1), legend.direction = "horizontal")


hline_dat = data.frame(analy_param=rep(c(  "ABGA", "HOMeTHFA","Mefox", "FA", "CH2THFA", "Mefguan", 
                                           "DiMeTHFA", "SumImp","Water content","Water content", "Isopropanol","Ethanol","Sulfur",
                                           "rRt 1.06","MeTHPA","rRt 1.16","Chloride Content" ,"Mefdiamid","THFA","DHFA","D-Mefox","L-Mefox", "Assay", "Assay"), each = length(levels(metafolin_long$Temp))), # neue Spez.
                       temp=rep(c("-20°C", "5°C", "25°C", "40°C"),24),
                       new_limit=rep(c(0.5, 1,1,0.5,0.5,0.5,0.15,2.5, 6,17, 0.5, 0.5, 50,0.15,0.5,0.1,0.5,0.15,0.5,0.5,0.5,0.5,95,102),each=length(levels(metafolin_long$Temp))))
hline_dat$temp <- factor(hline_dat$temp, levels = c("-20°C", "5°C", "25°C","40°C"))

meta_long %>% 
  filter(!str_detect(analy_param, "5-MeTHFA") ) %>% 
  na.omit() %>% 
  
  ggplot(aes(Batch, value, color=external))+
  ylab("conc. [% w/w] (ppm for Sulfur)")+
  geom_point(size=1.5, alpha=1)+
  geom_hline(data=hline_dat, aes(yintercept=new_limit), linetype= "twodash", col = "orange2", size=1)+
  # geom_vline(aes(xintercept = "LMCG0842XX"))+
  facet_wrap(~analy_param, scales = "free", ncol = 4)+

  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  scale_color_manual(values=c("#2dbecd","#eb3c96"))+
  theme(legend.position = c(0.7,0.1), legend.direction = "horizontal")
