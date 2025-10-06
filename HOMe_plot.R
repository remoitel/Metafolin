j=4

HOMe %>% 

  ggplot(aes(month, HOMeTHFA)) +
  xlab("Months")+
  ylab("HOMeTHFA [%]")+
  facet_grid(Batch ~ inert)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  # ylim(c(5.5,17.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-0.1,6.1),breaks = c(0,1,2,3,6)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  # geom_hline(yintercept = 6, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 17, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 4.5*j, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 6*j, colour = "#503291"),
        axis.text.x = element_text(size = 5*j, angle=0),
        axis.text.y = element_text(size = 5*j,  angle=0),
        axis.title.x = element_text(size = 5*j),
        axis.title.y = element_text(size = 5*j),
        legend.text = element_text(size = 6*j),
        legend.title = element_text(size = 6*j),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("HOMe.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 20)
ggsave("HOMe.png",device = "png", dpi=150, last_plot(),units = "cm", width = 20, height = 20)

HOMe %>% 
  
  ggplot(aes(month, Mefox)) +
  xlab("Months")+
  ylab("Mefox [%]")+
  facet_grid(Batch ~ inert)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  # ylim(c(5.5,17.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-0.1,6.1),breaks = c(0,1,2,3,6)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  # geom_hline(yintercept = 6, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 17, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 4.5*j, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 6*j, colour = "#503291"),
        axis.text.x = element_text(size = 5*j, angle=0),
        axis.text.y = element_text(size = 5*j,  angle=0),
        axis.title.x = element_text(size = 5*j),
        axis.title.y = element_text(size = 5*j),
        legend.text = element_text(size = 6*j),
        legend.title = element_text(size = 6*j),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("Mefox.svg",device = "svg", last_plot(),units = "cm", width = 20, height = 20)
ggsave("Mefox.png",device = "png", dpi=150, last_plot(),units = "cm", width = 20, height = 20)

