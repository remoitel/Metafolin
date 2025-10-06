library(flextable)
library(janitor)

meta_tab <- meta  %>% 
  # filter(Lev=="1") %>% 
  select(!c("(6S)-Mefolinate","(6R)-Mefolinate","Methanol", "Ethanol","Aceton","Isopropanol", "Bacterial endotoxins (BET)",
            "Lev","Appearance texture", "Calcium content on dried basis", "Purity 5-MeTHFA","Appearance texture", 
            "D-Mefox",  "L-Mefox" , "Material description", "Bacterial endotoxins (BET)","Sum ORC" ,"Individual RC" ,
            "Calcium content on dried basis" , "H2O and solv. free 5-MeTHFA-Ca", "H2O free 5-MeTHFA", "H2O free 5-MeTHFA-Ca",
            "Solvents", "H2O- and solvent free 5-MeTHFA", "IL short text")) %>%  #`
  # select(Batch, temp, month, `Inspection Lot`, `Water content`,Content, Spectrum,  `Biggest Unknown`,"1-Met, hylmetaazole",
         # "2-Methylmetaazole", "4-Methylmetaazole", "1-Methoxymethylmetaazole"  ) %>% 
  # filter(!str_detect(Batch,"038")) %>% 
  
  # filter(!(month == 0 & str_detect(`Inspection Lot`, "^89"))) %>% 
  # filter(!((Batch=="meta0016XX" |Batch=="meta0024XX") & temp=="40°C")) %>% 
  rename("Individual unknown impurities" = "Individual ORC", "Sum of all impurities" = "Sum ORC + RC") %>% 
  rename("5-MeTHFA-Ca water free" = "5-MeTHFA-Ca water free calc" ) %>% 
  relocate("5-MeTHFA-Ca water free",  .after = "5-MeTHFA-Ca") %>% 
  mutate(across(7:10, ~ round_half_up(., 2))) %>% 
  # mutate(across(10, ~ round_half_up(., 1))) %>%
  group_by(Batch) %>% 
  arrange(Batch, temp , month)

meta_tab$temp[meta_tab$month ==0] <- c("n/a") 
meta_tab <- unique(meta_tab)
  
tab <- as_grouped_data(meta_tab, groups = c("Batch"))
tab <- flextable(tab)
tab <- set_header_labels(tab, temp = "Storage Condition", month = "Duration")
tab

save_as_docx(tab, path = "meta_tab.docx")

#►----- nur MiBi

meta_tabMiBi <- meta  %>% 
  # filter(Lev=="1") %>% 
  select(c("Batch", "temp", "month", "Inspection Lot","Microbial Count (TAMC)", "Microbial Count (TYMC)")) %>%  #`
  # mutate(across(7:10, ~ round_half_up(., 2))) %>% 
  # mutate(across(10, ~ round_half_up(., 1))) %>%
  group_by(Batch) %>% 
  arrange(Batch, temp , month)

meta_tabMiBi$temp[meta_tabMiBi$month ==0] <- c("n/a") 
meta_tabMiBi <- unique(meta_tabMiBi)

tab2 <- as_grouped_data(meta_tabMiBi, groups = c("Batch"))
tab2 <- flextable(tab2)
tab2 <- set_header_labels(tab2, temp = "Storage Condition", month = "Duration")
tab2

save_as_docx(tab2, path = "meta_tabMiBi.docx")
