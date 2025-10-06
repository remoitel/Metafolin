library(flextable)
library(janitor)

LMCG_tab <- LMCG  %>% 
  # filter(Lev=="1") %>% 
  select(c("Batch", "temp", "month", "inert", "Inspection Lot", 
           "Assay 5-MeTHFA", "Assay 5-MeTHFA-Ca", 
           "Assay Imp. ABGA...112",  "Assay Imp. HOMeTHFA", 
           "Assay Imp. D-Mefox", "Assay Imp. L-Mefox", "Assay Mefox", "Assay Imp. Mefdiamid", 
           "Assay Imp. THFA...140", "Assay Imp. Mefguan", 
           "Assay Imp. DHFA...152",  "Assay Imp. FA...160", 
           "Assay Imp. CH2THFA...168",  
           "Assay Imp. rRt 1.06", "Assay Imp. MeTHPA...180", 
           "Assay Imp. DiMeTHFA", "Assay Imp. sum unknown", 
           "Individual ORC", "Individual RC", "Sum ORC", "Sum ORC + RC"
           ))  |>  #`
  # select(Batch, temp, month, `Inspection Lot`, `Water content`,Content, Spectrum,  `Biggest Unknown`,"1-Met, hylLMCGazole",
  # "2-MethylLMCGazole", "4-MethylLMCGazole", "1-MethoxymethylLMCGazole"  ) %>% 
  # filter(!str_detect(Batch,"038")) %>% 
  
  # filter(!(month == 0 & str_detect(`Inspection Lot`, "^89"))) %>% 
  # filter(!((Batch=="LMCG0016XX" |Batch=="LMCG0024XX") & temp=="40°C")) %>% 
  rename("Individual unknown impurities" = "Individual ORC", "Sum of all impurities" = "Sum ORC + RC") %>% 
  # rename("5-MeTHFA-Ca water free" = "5-MeTHFA-Ca water free calc" ) %>% 
  # relocate("5-MeTHFA-Ca water free",  .after = "5-MeTHFA-Ca") %>% 
  mutate(across(6:26, ~ as.numeric(.))) %>%
  mutate(across(6:7, ~ round_half_up(., 2))) %>% 
  mutate(across(8:26, ~ round_half_up(., 3))) %>% 
  group_by(Batch) %>% 
  arrange(Batch, inert, temp , month)

spalten <- colnames(LMCG_tab)
spalten <- gsub("\\.{3}\\d{1,3}","",spalten) #3 Punkte erkennen sowie 1-3 Ziffern
spalten <- gsub("\\Assay ?","",spalten) #Assay entfernen
spalten <- gsub("\\Imp. ","",spalten) #Imp. entfernen
names(LMCG_tab) <- spalten #neue, korrigierte Namen

LMCG_tab$temp[LMCG_tab$month ==0] <- c("n/a") 
LMCG_tab <- unique(LMCG_tab)

tab <- as_grouped_data(LMCG_tab, groups = c("inert","Batch"))
tab <- flextable(tab)
tab <- set_header_labels(tab, temp = "Storage Condition", month = "Duration")
tab

save_as_docx(tab, path = "LMCG_tab.docx")

#►----- nur MiBi

LMCG_tabMiBi <- LMCG  %>% 
  # filter(Lev=="1") %>% 
  select(c("Batch", "temp", "month", "Inspection Lot","Microbial Count (TAMC)", "Microbial Count (TYMC)")) %>%  #`
  # mutate(across(7:10, ~ round_half_up(., 2))) %>% 
  # mutate(across(10, ~ round_half_up(., 1))) %>%
  group_by(Batch) %>% 
  arrange(Batch, temp , month)

LMCG_tabMiBi$temp[LMCG_tabMiBi$month ==0] <- c("n/a") 
LMCG_tabMiBi <- unique(LMCG_tabMiBi)

tab2 <- as_grouped_data(LMCG_tabMiBi, groups = c("Batch"))
tab2 <- flextable(tab2)
tab2 <- set_header_labels(tab2, temp = "Storage Condition", month = "Duration")
tab2

save_as_docx(tab2, path = "LMCG_tabMiBi.docx")
