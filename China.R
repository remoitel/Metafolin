
meta_china <- c("LMCG0407ZX", "LMCG0711XX", "LMCG0695ZX", "LMCG0691XX", "LMCG0743FX", 
                "LMCG0746FX", "LMCG0855XX", "LMCG0890ZX")

# Assay -----
meta_long %>%  # DF mit Batches/Temp, die nur einmal vorkommen (T0)
  filter(analy_param == "5-MeTHFA-Ca water free calc"|analy_param ==  "Mefox" |analy_param == "Mefguan" |analy_param == "Sum ORC + RC") %>%  
  filter(Batch %in% meta_china) %>% 
  group_by(Batch) %>%
  mutate(analy_param = str_replace(analy_param, "5-MeTHFA-Ca water free calc", "5-MeTHFA-Ca water free")) %>% 
  # mutate(analy_param = str_replace(analy_param, "Sum ORC + RC", "Sum all imp.")) %>% 
  filter(temp=="25°C") %>% 
  # filter(n()!=1) %>%  #remove if occurence is only once (only T0)
  
  ggplot(aes(month, value, col=Batch)) +
  xlab("Months")+
  ylab("[% w/w]")+
  # ylim(c(94.5,102.5))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,10),breaks = c(0,3,6,9)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  # geom_hline(yintercept = 95, linetype= "twodash", col = "red3", linewidth =0.5)+
  # geom_hline(yintercept = 102, linetype= "twodash", col = "red3", linewidth=0.5)+
  geom_point(aes(col=Batch), size=2)+
  geom_line(aes(col=Batch), linewidth=1, alpha=0.5)+
  facet_wrap( ~ analy_param , scales = "free_y" )+
  # geom_line(data=. %>% filter(Batch == "LMCG0890ZX"), color="#ff6700", size=1.5, alpha=0.5)+
  # geom_point(data=. %>% filter(Batch == "LMCG0890ZX"),  color="#ff6700", size=2)+
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
  scale_color_manual(values = c("#3d3d3d",  "#383838", "#323232","#2d2d2d","#282828","#232323","#1e1e1e", "#ffc832"), name ="Storage Condition")

ggsave("meta_assay_thianxin.png", last_plot(),dpi=200, units = "cm", width = 18, height = 12)
