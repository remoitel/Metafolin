

batchlist <- c("LMCG0350NX", "LMCG0388XX", "LMCG0407ZX", 
               "LMCG0427PX", "LMCG0454ZX", "LMCG0526ZX", "LMCG0565ZX", "LMCG0623ZX", 
               "LMCG0711XX", "LMCG0695ZX", "LMCG0691XX", "LMCG0734XX", "LMCG0743FX", 
               "LMCG0746FX", "LMCG0778XX", "LMCG0781XX", "LMCG0812XX", "LMCG0843XX", 
               "LMCG0855XX", "LMCG0890ZX")


# Impurities with 0.5 -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "MeTHPA" |analy_param == "CH2THFA" |analy_param == "FA" |
           analy_param == "DHFA" |analy_param == "THFA" |analy_param == "ABGA"  ) %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value, col=Batch)) +
  xlab("Months")+
  ylab("conc. [% w/w]")+
  # facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  facet_grid(temp ~ analy_param)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=Batch), size=1)+
  geom_line(aes(col=Batch), size=0.7, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 22, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 18, colour = c("#0f69af","#149b5f",  "#ffc832", "#e61e50")),
        axis.text.x = element_text(size = 18,  angle=0),
        axis.text.y = element_text(size = 18,  angle=0),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.text=element_text(size=18),
        legend.title = element_text(size=18),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") 
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("meta_imp_05.png", last_plot(),dpi=300, units = "cm", width = 25, height = 12)


# Impurities with 0.15 -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "DiMeTHFA" |analy_param == "rRt 1.06" |analy_param == "FA" |analy_param == "DHFA" |analy_param == "THFA" |analy_param == "ABGA"  ) %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value, col=Batch)) +
  xlab("Months")+
  ylab("conc. [% w/w]")+
  # facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  facet_grid(temp ~ analy_param)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=Batch), size=1.5)+
  geom_line(aes(col=Batch), size=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = c("#0f69af","#149b5f",  "#ffc832", "#e61e50")),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") 
scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_imp015.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)
