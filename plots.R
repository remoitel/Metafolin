
lmsr_long %>% 
  # filter(temp =="25°C" |temp =="40°C") %>% 
  # filter(!analy_param %in% c("D-Mefox","Mefguan", "rRt 1.06" )) %>% 
  filter(analy_param %in% c("5-MeTHFA" )) %>% 
  # filter(parse_number(Batch) != 2990 ) %>% 
  filter(!Batch %in% c("DR-2990-A" )) %>% 
  
  ggplot(aes(month, value, group= Batch, col= Batch)) +
  xlab("Months")+
  ylab("% w/w")+
  ylim(c(84.5,100.5))+
  geom_point(  aes(color=temp), size=2)+
  geom_line( aes(color=temp), linewidth=1.5, alpha=0.7)+ 
  geom_hline(yintercept=85,linetype= 1, col = "red3", linewidth=0.2)+
  # scale_colour_grey()  +
  facet_grid(temp~ Batch, scales = "free" )+
  scale_x_continuous(expand=c(0,0), limits=c(-0.2,12.2),breaks = c(0,3,6,9,12))+
  theme(strip.text.x = element_text(family="Merck",  size = 18, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#ffc832"),
        axis.text.x = element_text(size = 14, colour = "#503291", angle=0),
        axis.text.y = element_text(size = 14, colour = "#503291", angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal", # "bottom"
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#fff7e2")) 

ggsave("plots/LMSRassay.png",dpi = 200, last_plot(),units = "cm", width = 20, height = 16)
ggsave("plots/LMSRassay.svg", last_plot(),units = "cm", width = 20, height = 16)


# ------ specified impurities ------
hline


lmsr_long %>% 
  filter(temp =="25°C" |temp =="40°C") %>% 
  filter(analy_param %in% c("HOMeTHFA","DiMeTHFA", "SumImp" )) %>% 
  # filter(parse_number(Batch) != 2990 ) %>% 
  filter(!Batch %in% c("DR-2990-A" )) %>% 
  
  ggplot(aes(month, value, group= Batch, col= Batch)) +
  xlab("Months")+
  ylab("% w/w")+
  # ylim(c(84.5,100.5))+
  geom_point(  aes(color=Batch), size=2)+
  geom_line( aes(color=Batch), linewidth=1.5, alpha=0.7)+ 
  # geom_hline(yintercept=85,linetype= 1, col = "red3", linewidth=0.2)+
  # scale_colour_grey()  +
  facet_grid(temp~ analy_param, scales = "free" )+
  scale_x_continuous(expand=c(0,0), limits=c(-0.2,12.2),breaks = c(0,1,3,6,9,12))+
  theme(strip.text.x = element_text(family="Merck",  size = 24, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#ffc832"),
        axis.text.x = element_text(size = 18, colour = "#503291", angle=0),
        axis.text.y = element_text(size = 18, colour = "#503291", angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal", # "bottom"
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#fff7e2")) 

ggsave("plots/imp1.png",dpi = 200, last_plot(),units = "cm", width = 20, height = 16)
ggsave("plots/imp1.svg", last_plot(),units = "cm", width = 20, height = 16)


# ------ unspecified impurities ------
lmsr_long %>% 
  filter(temp =="25°C" |temp =="40°C") %>% 
  filter(analy_param %in% c("ABGA", "Mefox" )) %>% 
  # filter(parse_number(Batch) != 2990 ) %>% 
  filter(!Batch %in% c("DR-2990-A" )) %>% 
  
  ggplot(aes(month, value, group= Batch, col= Batch)) +
  xlab("Months")+
  ylab("% w/w")+
  # ylim(c(84.5,100.5))+
  geom_point(  aes(color=Batch), size=2)+
  geom_line( aes(color=Batch), linewidth=1.5, alpha=0.7)+ 
  # geom_hline(yintercept=85,linetype= 1, col = "red3", linewidth=0.2)+
  # scale_colour_grey()  +
  facet_grid(temp~ analy_param, scales = "free" )+
  scale_x_continuous(expand=c(0,0), limits=c(-0.2,12.2),breaks = c(0,1,3,6,9,12))+
  theme(strip.text.x = element_text(family="Merck",  size = 24, colour = "#ffc832"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#ffc832"),
        axis.text.x = element_text(size = 18, colour = "#503291", angle=0),
        axis.text.y = element_text(size = 18, colour = "#503291", angle=0),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal", # "bottom"
        strip.background =element_rect(fill="#503291"),
        panel.background  = element_rect(fill = "#fff7e2")) 

ggsave("plots/imp1.png",dpi = 200, last_plot(),units = "cm", width = 20, height = 16)
ggsave("plots/imp1.svg", last_plot(),units = "cm", width = 20, height = 16)