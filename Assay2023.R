

batchlist <- c("LMCG0350NX", "LMCG0388XX", "LMCG0407ZX", 
               "LMCG0427PX", "LMCG0454ZX", "LMCG0526ZX", "LMCG0565ZX", "LMCG0623ZX", 
               "LMCG0711XX", "LMCG0695ZX", "LMCG0691XX", "LMCG0734XX", "LMCG0743FX", 
               "LMCG0746FX", "LMCG0778XX", "LMCG0781XX", "LMCG0812XX", "LMCG0843XX", 
               "LMCG0855XX", "LMCG0890ZX")
# Purity -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "Purity 5-MeTHFA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Purity 5-MeTHFA")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(95,100))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  # geom_hline(yintercept = 86, linetype= "dashed", col = "red3", size =1)+
  # geom_hline(yintercept = 90, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_pur.svg", device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_pur.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)



# Water content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "Water content") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Water content [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(5.5,17.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 6, linetype= "twodash", col = "red3", size =0.5)+
  geom_hline(yintercept = 17, linetype= "twodash", col = "red3", size =0.5)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("meta_water.svg", device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("meta_water.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# Assay -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "5-MeTHFA-Ca water free calc") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Assay 5-MeTHFA-Ca [% w/w] (waterfree)")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(94,103))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 95, linetype= "twodash", col = "red3", size =0.5)+
  geom_hline(yintercept = 102, linetype= "twodash", col = "red3", size=0.5)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_assay.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)



# ABGA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "ABGA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("ABGA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.6))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_abga.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)





# Mefox content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "Mefox") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Mefox [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,1.1))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 1, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_mefox.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# Mefdiamid content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "Mefdiamid") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Mefdiamid [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.16))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.15, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_mefdiamid.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# THFA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "THFA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("THFA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_THFA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# Mefguan content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "Mefguan") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Mefguan [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_mefguan.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)



# DHFA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "DHFA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("DHFA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_DHFA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# FA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "FA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("FA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_FA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# CH2THFA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "CH2THFA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("CH2THFA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_CH2THFA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# rRt 1.06 content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "rRt 1.06") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("rRt 1.06 [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.17))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.15, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_rRt_1_06.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# MeTHPA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "MeTHPA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("MeTHPA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_MeTHPA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# rRt 1.16 content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "rRt 1.16") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("rRt 1.16 [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.12))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.1, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_rRt_1_16.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# DiMeTHFA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "DiMeTHFA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("DiMeTHFA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.17))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.15, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_DiMeTHFA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# Ind. Unk. Imp. content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "Individual ORC") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Individual ORC [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.12))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.1, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_Individual_ORC.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)



# Sum all -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "Sum ORC + RC") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Sum of all impurities [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,2.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 2, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("H:/My Documents/R/Projekte/Bilder_export/meta_sum_all.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)



# rRt 1.06 -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "rRt 1.06") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("rRt 1.06 [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.16))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.15, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("meta_rrt106.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# rRt 1.16 -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "rRt 1.16") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("rRt 1.16 [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.11))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.1, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("meta_rrt116.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)


# DiMeTHFA-----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "DiMeTHFA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("DiMeTHFA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.16))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.15, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("meta_DiMeTHFA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)

# MeTHPA content -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "MeTHPA") %>%  
  filter(Batch %in% batchlist) %>% 
  group_by(Batch,temp) %>%
  filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("MeTHPA [% w/w]")+
  facet_wrap( ~ Batch , ncol =5)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,0.2))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36,48,60)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.15, linetype= "twodash", col = "red3", size =0.5)+
  # geom_hline(yintercept = 16, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=1.5)+
  geom_line(aes(col=temp), linewidth=1, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
        # strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 14,  angle=0),
        axis.text.y = element_text(size = 14,  angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title = element_text(size=16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af","#149b5f",  "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:/Users/M291711/OneDrive - MerckGroup/Stability Testing/Folate/Metafolin/meta_DiTHPA.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)
