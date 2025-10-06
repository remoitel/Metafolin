unique(meta$`Appearance color`)

meta$`Appearance color` <- factor(meta$`Appearance color`, levels = c("white","almost white", "light yellowish",  "light beige", "beige", "Accepted", NA))





unique(FK$`Appearance color`)

# Color


meta %>% 
  select(Batch, month, temp, "Appearance color") %>% 
  filter(Batch %in% batchlist) %>% 
  # pivot_longer(!c(Batch, month,temp), names_to = "analy_param", values_to = "value")   %>%
  rename( "Color" = "Appearance color")%>% 
  mutate(appearance = factor(Color, levels = c("white","almost white", "light yellowish",  "light beige", "beige", "Accepted", NA))) %>% 
  mutate(temp = factor(temp, levels = c("-20°C","5°C","25°C","40°C"))) %>% 
  #filter(Batch=="FKG0581XX" | Batch== "FKG0548XX")%>% 
  #filter(str_detect(manuf_batch, "^..1[2-7]")) %>%  #Jahreszahlen 2011-2014 beginnend
  filter(temp=="5°C")%>% 
  
  ggplot(aes(month, Color)) + 
  facet_wrap(. ~ Batch)+ 
  scale_x_continuous(expand=c(0,0), limits=c(-3,42),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_point(size=2.5, alpha=0.5, shape=21, stroke=1, color="black",aes(fill=Color))+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 12, colour = "#503291"), legend.position = "none",
        strip.text.y = element_text(family="Merck", size = 12, colour = "#503291"))+
  scale_y_discrete(limits = c("white","almost white", "light yellowish",  "light beige", "beige"))+
  scale_fill_manual(values = c( "white"="snow", 
                                "almost white"="lightyellow1", 
                                "light yellowish"="lightyellow2",
                                "light beige"= "wheat1",
                                "beige"="wheat3"))

ggsave("Color_5C.png", last_plot(),dpi=200, units = "cm", width = 18, height = 10)
ggsave("Color_5C.svg", device = "svg", last_plot(),units = "cm", width = 24, height = 10)


#---------- 3 Batches full -------
FK %>% 
  filter(Batch == "FKG0621XX"| Batch == "FKG0931ZX" | Batch == "FKG0939ZX" ) %>% 
  select(Batch, month, temp,manuf_batch, "Appearance color") %>% 
  # pivot_longer(!c(Batch, month,temp), names_to = "analy_param", values_to = "value")   %>%
  rename( "Color" = "Appearance color")%>% 
  mutate(appearance = factor(Color, levels = c("white","almost white","light yellowish","light beige",
                                               "yellowish","light yellow" ))) %>% 
  mutate(temp = factor(temp, levels = c("-20°C","5°C","25°C","40°C"))) %>% 
  
  
  ggplot(aes(month, Color)) + 
  xlab("Months")+
  facet_grid(temp ~ Batch)+ 
  scale_x_continuous(expand=c(0,0), limits=c(-3,42),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_point(size=5, alpha=0.5, shape=21, stroke=2, color="black",aes(fill=Color))+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 12, colour = "#503291"), legend.position = "none",
        strip.text.y = element_text(family="Merck", size = 12, colour = "#503291"))+
  scale_y_discrete(limits = c("white","almost white","light beige","light yellowish","light yellow", "yellowish"))+
  scale_fill_manual(values = c( "white"="snow", 
                                "almost white"="#FFFEF6", 
                                "light beige"= "#F3E3D3",
                                "light yellowish"="#FFFAC9",
                                "light yellow"="#FFF6B2",
                                "yellowish"="#FFF5A5"))


ggsave("FK_Color3.svg", device = "svg", last_plot(),units = "cm", width = 24, height = 15)
ggsave("FK_Color3.png", device = "png", last_plot(),units = "cm", width = 24, height = 15)
