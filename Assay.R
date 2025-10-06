
# Assay nur Fullstudy -----
LMC_long %>% 
  filter(analy_param == "assay_H2O_free") %>%  
  filter(temp == "5°C" | temp == "25°C"  ) %>% 
  drop_na() %>% 
  
  ggplot(aes(months, value)) +
  xlab("Months")+
  ylab("Assay Ca-Folinate %(w/w)")+
  facet_wrap( ~ batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(95,103))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 96, linetype= "dashed", col = "red3", size=1)+
  geom_hline(yintercept = 102, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("C:\\Users\\M291711/Desktop/LMC_assay_full.svg", device = "svg", last_plot(),units = "cm", width = 20, height = 12)
ggsave("C:\\Users\\M291711/Desktop/LMC_assay_full.png", last_plot(),units = "cm", width = 20, height = 12)


# Assay alle-----
LMC_big_long %>% 
  filter(Batch %in%  batch_list_final) %>% 
  filter(temp == "5°C" ) %>% 
  filter(analy_param == "Assay anhyd") %>%  
  
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Assay [w/w %]")+
  facet_wrap( ~ Batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  ylim(c(96,103))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 97, linetype= "dashed", col = "red3", size=1)+
  # geom_hline(yintercept = 96.5, linetype= "dashed", col = "orange", size=1)+
  geom_hline(yintercept = 102, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=2, alpha=0.5)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 9, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c( "#149b5f"), name ="Storage Condition")

ggsave("LMC_assay2.svg", device = "svg", last_plot(),units = "cm", width = 20, height = 25)
ggsave("LMC_assay2.png", last_plot(),units = "cm", width = 20, height = 25)



akz_krit <- 97 #Akzeptanzkriterium in %
conf_level <- 0.95 #Vertrauensintervall z.B 0.95 f?r 95%
ylow <- 94
yhigh <- 101
bis_monate <- 40


#---Loop mit allen Batchs gemäss Liste -------
i<-1
batch_plots = list()
#batch_list_manuell <- batchlist[-c(15, 16)] # ohne die Mahlvalidierungen
batch_list_manuell <- batchlist # ohne die Mahlvalidierungen

for (i in 1:length(batch_list_manuell)) {
  
  FK_assay <- FK_long %>%
    # select(Batch, month, temp, "Assay H2O free Folinate-Ca") %>% 
    # rename("Assay Ca-Folinate" = "Assay H2O free Folinate-Ca") %>% 
    # pivot_longer(!c(Batch, month,temp), names_to = "analy_param", values_to = "value")   %>% # alle zu long ausser months und temp
    # mutate(temp = factor(temp, levels = c("-20°C","5°C","25°C","40°C"))) %>%
    # mutate(value = as.numeric(value), month = as.integer(month)) %>%
     filter(Batch==batch_list_manuell[i]) %>%
    filter(analy_param==	"Assay H2O free Folinate-Ca") %>%
    drop_na() %>% 
    filter(temp=="25°C") 
  
  fit1 <- lm(value ~ month, data = FK_assay)
  newx <- seq(0, bis_monate, by=0.1)
  #newx <- seq(min(bsp$months), max(bsp$months), by=0.05)#f?r Min/Max aus bsp
  conf_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="confidence",level = conf_level)
  pred_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="prediction", level = conf_level)
  conf_table1 <- as.data.table(cbind(newx, conf_interval1))
  # Variante ohne data.table: which.min(abs(conf_interval1-akz_krit))
  setkey(conf_table1, lwr) # sorts the data
  schnittpunkt1_1 <- conf_table1[J(akz_krit), roll = "nearest"] # sucht den n?chsten Wert
  schnittpunkt1_1 <- as.numeric(schnittpunkt1_1[,1]) # Nur den Monat als Nummer
  setkey(conf_table1, upr) # sorts the data
  schnittpunkt1_2 <- conf_table1[J(akz_krit), roll = "nearest"] # sucht den n?chsten Wert
  schnittpunkt1_2 <- as.numeric(schnittpunkt1_2[,1]) # Nur den Monat als Nummer
  # Erster Schnittpunkt oben oder unten?
  if(schnittpunkt1_2 < schnittpunkt1_1) {schnittpunkt1_1 <- schnittpunkt1_2}
  if(schnittpunkt1_1 > bis_monate) {schnittpunkt1_1 <- NULL}
  
  
  batch_plots[[batch_list_manuell[i]]] = ggplot(aes(month, value), data = FK_assay) +
    xlab("Months")+
    ylab("Assay Ca-Folinate [%]")+
    facet_wrap(~ Batch, ncol=2)+
    coord_cartesian(xlim = c(0,bis_monate), ylim = c(ylow,yhigh), expand = FALSE)+
    geom_hline(yintercept = akz_krit, linetype= "dashed", col = "red3", size=1)+
    geom_point(aes(color=temp), size=2)+
    geom_line(aes(color=temp), size=1.5, alpha=0.7)+
    theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
          legend.title = element_blank(), legend.position = c(0.15,0.3), legend.background = element_rect(fill="transparent"))
  
  if(nrow(FK_assay)>3){batch_plots[[batch_list_manuell[i]]] <- batch_plots[[batch_list_manuell[i]]] + # Conf-Level nur wenn mehr als 3 Punkte
    geom_line(data = conf_table1, aes(newx, lwr), linetype= "longdash", col = "skyblue3", size=1)+
    geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
    geom_point(aes(color=temp))+
    geom_line(aes(color=temp), size=1.5, alpha=0.7)}
  
  # if (is.null(schnittpunkt1_1)== FALSE) {batch_plots[[batch_list_manuell[i]]] <- batch_plots[[batch_list_manuell[i]]] + # falls kein Schnittpunkt vorhanden, kein Pfeil
  #   geom_segment(aes(x = schnittpunkt1_1, y = akz_krit, xend = schnittpunkt1_1, yend = ylow), arrow = arrow(length = unit(0.3, "cm"), angle = 20),col= "grey50", size=1)+
  #   geom_text(x=schnittpunkt1_1-2, y=ylow+0.2, label=schnittpunkt1_1, color="grey50")} 
  
  print(batch_plots[[batch_list_manuell[i]]])
  ggsave(batch_plots[[batch_list_manuell[i]]], file=paste0("plot", i,".svg"),path = , device = "svg", units = "cm", width = 22, height = 10)
  
}

plot_grid(plotlist = batch_plots)

ggsave("FK_assay_all.png", last_plot(),units = "cm", width = 30, height = 25)


# Assay as is-----
FK_long %>% 
  filter(analy_param == "Assay as is", manuf_batch %notin% c("2012 - FKG0621XX","2018 - FKG0931ZX","2018 - FKG0939ZX","2017 - FKT0893XC","2017 - FKT0897ZB")) %>% 
  filter(temp == "25°C") %>% 
  ggplot(aes(month, value)) +
  xlab("Months")+
  ylab("Assay as is [w/w %]")+
  facet_wrap( ~ manuf_batch , ncol =3)+
  #facet_grid_paginate( Batch ~ analy_param, ncol =1, nrow = 6, page = 1)+
  #facet_grid( analy_param ~ Batch)+
  #ylim(c(95,100))+
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  #geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
  geom_hline(yintercept = 74.5, linetype= "dashed", col = "red3", size=1)+
  geom_hline(yintercept = 94.4, linetype= "dashed", col = "red3", size=1)+
  geom_point(aes(col=temp), size=2)+
  geom_line(aes(col=temp), size=1, alpha=0.7)+
  #geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
  theme(strip.text.x = element_text(family="Merck", face="bold", size = 8, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal") +
  scale_color_manual(values = c( "#ffc832", "#e61e50"), name ="Storage Condition")

ggsave("FK_asis.svg", device = "svg", last_plot(),units = "cm", width = 20, height = 25)
ggsave("FK_asis.png", last_plot(),units = "cm", width = 20, height = 25)


akz_krit <- 74.5 #Akzeptanzkriterium in %
akz_krit2 <- 94.4 #Akzeptanzkriterium in %
conf_level <- 0.95 #Vertrauensintervall z.B 0.95 f?r 95%
ylow <- 72
yhigh <- 96
bis_monate <- 40


#---Loop mit allen Batchs gemäss Liste -------
i<-1
batch_plots = list()
batch_list_manuell <- batchlist[-c(15, 16)] # ohne die Mahlvalidierungen
for (i in 1:length(batch_list_manuell)) {
  
  FK_assay <- FK_long %>%
    # select(Batch, month, temp, "Assay H2O free Folinate-Ca") %>% 
    # rename("Assay Ca-Folinate" = "Assay H2O free Folinate-Ca") %>% 
    # pivot_longer(!c(Batch, month,temp), names_to = "analy_param", values_to = "value")   %>% # alle zu long ausser months und temp
    # mutate(temp = factor(temp, levels = c("-20°C","5°C","25°C","40°C"))) %>%
    # mutate(value = as.numeric(value), month = as.integer(month)) %>%
    filter(Batch==batch_list_manuell[i]) %>%
    filter(analy_param==	"Assay as is") %>%
    filter(temp=="25°C") 
  
  fit1 <- lm(value ~ month, data = FK_assay)
  newx <- seq(0, bis_monate, by=0.1)
  #newx <- seq(min(bsp$months), max(bsp$months), by=0.05)#f?r Min/Max aus bsp
  conf_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="confidence",level = conf_level)
  pred_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="prediction", level = conf_level)
  conf_table1 <- as.data.table(cbind(newx, conf_interval1))
  # Variante ohne data.table: which.min(abs(conf_interval1-akz_krit))
  setkey(conf_table1, lwr) # sorts the data
  schnittpunkt1_1 <- conf_table1[J(akz_krit), roll = "nearest"] # sucht den n?chsten Wert
  schnittpunkt1_1 <- as.numeric(schnittpunkt1_1[,1]) # Nur den Monat als Nummer
  setkey(conf_table1, upr) # sorts the data
  schnittpunkt1_2 <- conf_table1[J(akz_krit), roll = "nearest"] # sucht den n?chsten Wert
  schnittpunkt1_2 <- as.numeric(schnittpunkt1_2[,1]) # Nur den Monat als Nummer
  # Erster Schnittpunkt oben oder unten?
  if(schnittpunkt1_2 < schnittpunkt1_1) {schnittpunkt1_1 <- schnittpunkt1_2}
  if(schnittpunkt1_1 > bis_monate) {schnittpunkt1_1 <- NULL}
  
  
  batch_plots[[batch_list_manuell[i]]] = ggplot(aes(month, value), data = FK_assay) +
    xlab("Months")+
    ylab("Assay as is [w/w %]")+
    facet_wrap(~ Batch, ncol=2)+
    coord_cartesian(xlim = c(0,bis_monate), ylim = c(ylow,yhigh), expand = FALSE)+
    geom_hline(yintercept = akz_krit, linetype= "dashed", col = "red3", size=1)+
    geom_hline(yintercept = akz_krit2, linetype= "dashed", col = "red3", size=1)+
    geom_point(aes(color=temp), size=2)+
    geom_line(aes(color=temp), size=1.5, alpha=0.7)+
    theme(strip.text.x = element_text(family="Merck", face="bold", size = 14, colour = "#503291"),
          legend.title = element_blank(), legend.position = c(0.8,0.8), legend.background = element_rect(fill="transparent"))
  
  if(nrow(FK_assay)>3){batch_plots[[batch_list_manuell[i]]] <- batch_plots[[batch_list_manuell[i]]] + # Conf-Level nur wenn mehr als 3 Punkte
    geom_line(data = conf_table1, aes(newx, lwr), linetype= "longdash", col = "skyblue3", size=1)+
    geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
    geom_point(aes(color=temp))+
    geom_line(aes(color=temp), size=1.5, alpha=0.7)}
  
  # if (is.null(schnittpunkt1_1)== FALSE) {batch_plots[[batch_list_manuell[i]]] <- batch_plots[[batch_list_manuell[i]]] + # falls kein Schnittpunkt vorhanden, kein Pfeil
  #   geom_segment(aes(x = schnittpunkt1_1, y = akz_krit, xend = schnittpunkt1_1, yend = ylow), arrow = arrow(length = unit(0.3, "cm"), angle = 20),col= "grey50", size=1)+
  #   geom_text(x=schnittpunkt1_1-2, y=ylow+0.2, label=schnittpunkt1_1, color="grey50")} 
  
  print(batch_plots[[batch_list_manuell[i]]])
  ggsave(batch_plots[[batch_list_manuell[i]]], file=paste0("plot", i,".svg"),path = , device = "svg", units = "cm", width = 22, height = 10)
}

plot_grid(plotlist = batch_plots)

ggsave("FK_assay_asis.png", last_plot(),units = "cm", width = 30, height = 25)
