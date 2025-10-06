

#----  Daten aus Excel ------

LMC <- read_excel("~/R/Projekte/Metafolin/Stabidaten_bis_2015.xlsx", 
                           sheet = "Data", col_types = c("text", "text",  "numeric",  "text", "text", "numeric", "numeric","numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric" ))
# LMC <-  LMC %>% 
#     mutate_at(vars("temp"), as.factor)
  

# batches <- readClipboard()

# batch_list_final<-  c("LMCM0622ZX","LMCG0623ZX","LMCM0575ZX","LMCG0565ZX","LMCM0504XX","LMCG0526ZX","LMCM0480ZX","LMCG0454ZX",
                      # "LMCM0432XX","LMCG0427PX","LMCG0407ZX","LMCG0388XX","LMCG0350NX","LMCM0338IX","LMCM0337ZX","LMCG0340IX",
                      # "LMCM0280XX","LMCG0288XX","LMCA-M-0243","LMCA-G-0229","LMCA-G-0211-A")

batch_list_final<-  c("LMCG0623ZX","LMCG0565ZX","LMCG0526ZX","LMCG0454ZX","LMCG0427PX","LMCG0388XX","LMCG0340IX",
                      "LMCG0288XX","LMCA-G-0229")

LMC_use <- filter(LMC, batch %in% batch_list_final) # nur manuelle Liste


LMC_long<- LMC_use %>% 
  select(1:26) %>% # ohne MiBi
  select(-c("IL", "date")) %>% 
  # mutate_at(vars(6:27), as.numeric) %>% 
  pivot_longer(!c(batch, months,temp), names_to = "analy_param", values_to = "value")   %>%  # alle zu long ausser months und temp
  mutate(temp = factor(temp, levels = c("-20°C","5°C","25°C","40°C")))



