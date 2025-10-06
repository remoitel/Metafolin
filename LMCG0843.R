
library(plotly) 

meta_raw <- read_excel("~/SAP/SAP GUI/LMGC0843XX_ua_Y1Y2.xlsx")


wegdamit <- c("Operation","Operation short text", "Physical sample", "Inspection unit number", "Plant", "Material","Shelf Life Exp. Date",
              "Last goods receipt","Order","Vendor","Sequence", "Vial", "Seq. Notification", "Weighing No.","Date","Time","Pages inclusive cover",
              "Retain Sample Chem Created", "Deviations from analytical method" ,"Analytical processing time","UD code" )

meta <- meta_raw %>%
  mutate(month = str_extract(`IL short text`, "\\d+(?= ?Mt[ e.])"))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°C)"),"°C"))%>%  #str_c wie paste, aber besser für NA
  select(-all_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC ')) %>% 
  select(-contains(c("ICP-", "document", "DCP","action", "Pos ", "unknown", "article"))) %>%  #ohne ICP-Resultate 
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  relocate(any_of(c("temp","month")), .after = Batch) %>% 
  filter(Lev==1) %>% 
  filter(str_detect(`Inspection Lot`, "^89|^40"))  #Nur Freigabe und Stabi


meta$month <-replace_na(meta$month, 0)
meta$month <- as.numeric(meta$month) 

meta <- meta %>% 
  select("Batch", "temp", "month", "Inspection Lot", "IL short text","Water content",
         "Assay 5-MeTHFA", "Assay 5-MeTHFA-Ca", "Assay H2O- and solvent free 5-MeTHFA", 
         "Assay H2O and solv. free 5-MeTHFA-Ca...268","Assay Imp. ABGA...276", "Assay Imp. HOMeTHFA", 
         "Assay Imp. D-Mefox", "Assay Imp. L-Mefox", "Assay Mefox", "Assay Imp. Mefdiamid", 
         "Assay Imp. THFA...304", "Assay Imp. Mefguan", "Assay Imp. DHFA...316",  
         "Assay Imp. FA...324","Assay Imp. CH2THFA...332","Assay Imp. rRt 1.06", "Assay Imp. MeTHPA...344", 
         "Assay Imp. rRt 1.16", "Assay Imp. DiMeTHFA","Sum ORC + RC" ) %>% 
  rename("5-MeTHFA" ="Assay 5-MeTHFA", 
         "5-MeTHFA-Ca" ="Assay 5-MeTHFA-Ca", 
         "H2O- and solvent free 5-MeTHFA" ="Assay H2O- and solvent free 5-MeTHFA", 
         "5-MeTHFA" ="Assay 5-MeTHFA", 
         "5-MeTHFA-Ca" ="Assay 5-MeTHFA-Ca", 
         "H2O- and solvent free 5-MeTHFA" ="Assay H2O- and solvent free 5-MeTHFA", 
         "H2O and solv. free 5-MeTHFA-Ca" ="Assay H2O and solv. free 5-MeTHFA-Ca...268", 
         "ABGA" ="Assay Imp. ABGA...276", 
         "HOMeTHFA" ="Assay Imp. HOMeTHFA", 
         "D-Mefox" ="Assay Imp. D-Mefox", 
         "L-Mefox" ="Assay Imp. L-Mefox", 
         "Mefox" ="Assay Mefox", 
         "Mefdiamid" ="Assay Imp. Mefdiamid", 
         "THFA" ="Assay Imp. THFA...304", 
         "Mefguan" ="Assay Imp. Mefguan", 
         "DHFA" ="Assay Imp. DHFA...316", 
         "FA" ="Assay Imp. FA...324", 
         "CH2THFA" ="Assay Imp. CH2THFA...332", 
         "rRt 1.06" ="Assay Imp. rRt 1.06", 
         "MeTHPA" ="Assay Imp. MeTHPA...344", 
         "rRt 1.16" ="Assay Imp. rRt 1.16", 
         "DiMeTHFA" ="Assay Imp. DiMeTHFA",
         "SumImp" = "Sum ORC + RC" )


meta_long <- meta %>% 
  pivot_longer(!c(Batch, month,temp,"Inspection Lot", "IL short text"), names_to = "analy_param", values_to = "value") %>% 
  mutate_at(vars(value), as.numeric)


parameter <- "SumImp" # 

meta_long %>% 
  filter(!is.na(value)) %>% 
  filter(analy_param==parameter) %>% 
  
  ggplot(aes(Batch, value))+
  ylab(parameter)+
  geom_boxplot()+
  geom_point(data=meta_long %>% filter(`Inspection Lot` != "890000134946") %>% filter(analy_param==parameter), aes(Batch, value, color=month) )+
  geom_point(data=meta_long %>% filter(`Inspection Lot` == "890000134946") %>% filter(analy_param==parameter), aes(Batch, value), color = "red", size=2)
  

parameter <- "SumImp" # 

extrakt <- meta_long %>% 
  filter(!is.na(value)) %>% 
  # filter(month < 7) %>% 
  filter(value <2.5) %>% 
  mutate_at(vars(month), as.factor)
  
  ggplot(extrakt, aes(analy_param, value))+
  ylab("conc. [% w/w]")+
  xlab("Impurity")+
  ylim(0,2.2)+
  geom_boxplot()+
  geom_point(data=extrakt %>% filter(`Batch` != "LMCG0843XX"), aes(analy_param, value, shape=temp, color=month), size=2 )+
  geom_point(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(analy_param, value), color="red", size=4)

 
  
  extrakt <- meta_long %>% 
    filter(!is.na(value)) %>% 
     # filter(month < 7) %>% 
    filter(analy_param == "SumImp")  # SumImp
    mutate_at(vars(month), as.factor) 

ggplotly( 
  ggplot(extrakt, aes(analy_param, value))+
    ylab("conc. [% w/w]")+
    xlab("")+
    geom_boxplot()+
    geom_jitter(data=extrakt %>% filter(`Batch` != "LMCG0843XX"), aes(analy_param, value, shape=temp, color=month), width=0.05, size=2 )+
    geom_point(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(analy_param, value, color= month), size=4)
)

  
  #----Tabelle-----
  
  library(flextable)
  
  tab1 <- meta %>% 
    arrange(Batch, temp) %>% 
    flextable() %>% 
    merge_v(j = c("Batch","temp"))
  
  tab1
  
  save_as_docx(tab1, path = "LMCG0843ua.docx")
  
  