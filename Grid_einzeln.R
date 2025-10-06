# Impurities: Water -----

LMC_long %>% 
  filter(analy_param == "water") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("Water %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(5.5,17.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 6, linetype= "dashed", col = "red3", size=1)+
  geom_hline(yintercept = 17, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_water.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_water.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)


# Impurities: ABGA -----

LMC_long %>% 
  filter(analy_param == "ABGA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("ABGA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_ABGA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_ABGA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: HOMeTHFA -----

LMC_long %>% 
  filter(analy_param == "HOMeTHFA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("HOMeTHFA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,1.1))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 1, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_HOMeTHFA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_HOMeTHFA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: Mefox -----

LMC_long %>% 
  filter(analy_param == "Mefox") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("Mefox %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,1.1))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 1, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_Mefox.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_Mefox.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: THFA -----

LMC_long %>% 
  filter(analy_param == "ABGA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("THFA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_THFA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_THFA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: DHFA -----

LMC_long %>% 
  filter(analy_param == "DHFA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("DHFA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_DHFA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_DHFA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: FA -----

LMC_long %>% 
  filter(analy_param == "FA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("FA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_FA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_FA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: CH2THFA -----

LMC_long %>% 
  filter(analy_param == "CH2THFA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("CH2THFA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_CH2THFA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_CH2THFA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: MeTHPA -----

LMC_long %>% 
  filter(analy_param == "MeTHPA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("MeTHPA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,.55))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.5, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_MeTHPA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_MeTHPA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: DiMeTHFA -----

LMC_long %>% 
  filter(analy_param == "DiMeTHFA") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("DiMeTHFA %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,.16))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 0.15, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_DiMeTHFA.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_DiMeTHFA.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)

# Impurities: Sum unk -----

LMC_long %>% 
  filter(analy_param == "sum_unk") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("Sum unknown %(area)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(0,2.6))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 2.5, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 100, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_Sum_unk.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_Sum_unk.png",device = "png", last_plot(),units = "cm", width = 20, height = 12)
