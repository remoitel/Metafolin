library(flextable)
library(janitor)

LMCG_tab <- LMCG_selection  %>% 
  rename("Individual unknown impurities" = "Individual ORC", "Sum of all impurities" = "Sum ORC + RC") %>% 
  # rename("5-MeTHFA-Ca water free" = "5-MeTHFA-Ca water free calc" ) %>% 
  # relocate("5-MeTHFA-Ca water free",  .after = "5-MeTHFA-Ca") %>% 

  mutate(across(7:8, ~ round_half_up(., 2))) %>% 
  mutate(across(9:23, ~ round_half_up(., 3))) %>% 
  group_by(Batch) %>% 
  arrange(Batch, temp , days)
  
tab <- as_grouped_data(LMCG_tab, groups = c("Batch"))
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
