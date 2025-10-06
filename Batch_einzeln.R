library(data.table)
library(tidyverse)
library(cowplot)

batch_list<-  meta %>% 
  distinct(Batch) %>% 
  filter(!str_detect(Batch,"^LMCM")) %>% 
  pull(Batch)  # muss Liste sein, kein Tibble
  

akz_krit <- 0.5 #Akzeptanzkriterium in %
conf_level <- 0.90 #Vertrauensintervall z.B 0.95 f?r 95%
ylow <- 0
yhigh <- 1.1
bis_monate <- 40

#---Loop mit allen Batchs gemäss Liste (Mittelwert) -------
i<-1
batch_plots = list()
for (i in 1:length(batch_list)) {
  
  meta_run <- meta_mean %>%
    select(Batch, month,  mean) %>% 
   # mutate(value = as.numeric(value), month = as.integer(month)) %>%
    filter(Batch==batch_list[i])
  
  fit1 <- lm(mean ~ month, data = meta_run)
  newx <- seq(0, bis_monate, by=0.1)
  #newx <- seq(min(bsp$months), max(bsp$months), by=0.05)#f?r Min/Max aus bsp
  conf_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="confidence",level = conf_level)
  pred_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="prediction", level = conf_level)
  conf_table1 <- as.data.table(cbind(newx, pred_interval1))
  # Variante ohne data.table: which.min(abs(conf_interval1-akz_krit))
  setkey(conf_table1, lwr) # sorts the data
  schnittpunkt1_1 <- conf_table1[J(akz_krit), roll = "nearest"] # sucht den nächsten Wert
  schnittpunkt1_1 <- as.numeric(schnittpunkt1_1[,1]) # Nur den Monat als Nummer
  setkey(conf_table1, upr) # sorts the data
  schnittpunkt1_2 <- conf_table1[J(akz_krit), roll = "nearest"] # sucht den nächsten Wert
  schnittpunkt1_2 <- as.numeric(schnittpunkt1_2[,1]) # Nur den Monat als Nummer
  # Erster Schnittpunkt oben oder unten?
  if(schnittpunkt1_2 < schnittpunkt1_1) {schnittpunkt1_1 <- schnittpunkt1_2}
  if(schnittpunkt1_1 > bis_monate) {schnittpunkt1_1 <- NULL}
  
  
  batch_plots[[batch_list[i]]] = ggplot(aes(month, mean), data = meta_run) +
    xlab("Months")+
    ylab("HOMeTHFA [% v/v]")+
    facet_wrap(~ Batch, ncol=2)+
    coord_cartesian(xlim = c(0,bis_monate), ylim = c(ylow,yhigh), expand = FALSE)+
    geom_hline(yintercept = akz_krit,  col = "red", size=0.5)+
    geom_hline(yintercept = 0.7,  col = "red2", size=0.5)+
    geom_hline(yintercept = 0.5,  col = "red3", size=0.5)+

    geom_point( size=2)+
    geom_line(size=1.5, alpha=0.7)+
    theme(strip.text.x = element_text(family="Merck", face="bold", size = 11, colour = "#503291"),
          legend.title = element_blank(), legend.position = c(0.15,0.3), legend.background = element_rect(fill="transparent"))
  
  if(nrow(meta_run)>3){batch_plots[[batch_list[i]]] <- batch_plots[[batch_list[i]]] + # Conf-Level nur wenn mehr als 3 Punkte
    geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
    geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
    geom_point()+
    geom_line(size=1.5, alpha=0.7)} 
  
  # geom_rect(aes(xmin = 24, xmax = 40, ymin = 0, ymax = 1.1),  fill="grey99", alpha=0.2)
  
  # if (is.null(schnittpunkt1_1)== FALSE) {batch_plots[[batch_list_manuell[i]]] <- batch_plots[[batch_list_manuell[i]]] + # falls kein Schnittpunkt vorhanden, kein Pfeil
  #   geom_segment(aes(x = schnittpunkt1_1, y = akz_krit, xend = schnittpunkt1_1, yend = ylow), arrow = arrow(length = unit(0.3, "cm"), angle = 20),col= "grey50", size=1)+
  #   geom_text(x=schnittpunkt1_1-2, y=ylow+0.2, label=schnittpunkt1_1, color="grey50")} 
  
  print(batch_plots[[batch_list[i]]])
  ggsave(batch_plots[[batch_list[i]]], file=paste0("plot", i,".svg"), device = "svg", units = "cm", width = 22, height = 10)
  
}

plot_grid(plotlist = batch_plots)

ggsave("Metafolin_Batches ohne Mironized, HOMeTHFA, Prediction.png", plot=last_plot(), width = 35, height = 25, units="cm")


#---Loop mit allen Batchs gemäss Liste (Einzelwerte) -------

i<-1
batch_plots = list()
for (i in 1:length(batch_list)) {
  
  meta_long <- meta_mean %>% 
    pivot_longer(!c(Batch, month), names_to = "param", values_to = "value") %>% 
    filter(param=="min" | param=="max") %>% 
    filter(Batch==batch_list[i])
  
  fit1 <- lm(value ~ month, data = meta_long)
  newx <- seq(0, bis_monate, by=0.1)
  #newx <- seq(min(bsp$months), max(bsp$months), by=0.05)#f?r Min/Max aus bsp
  conf_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="confidence",level = conf_level)
  pred_interval1 <- predict(fit1, newdata=data.frame(month=newx), interval="prediction", level = conf_level)
  conf_table1 <- as.data.table(cbind(newx, pred_interval1))
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
  
  
  batch_plots[[batch_list[i]]] = ggplot(aes(month, value), data = meta_long) +
    xlab("Months")+
    ylab("HOMeTHFA [% v/v]")+
    facet_wrap(~ Batch, ncol=2)+
    coord_cartesian(xlim = c(0,bis_monate), ylim = c(ylow,yhigh), expand = FALSE)+
    geom_hline(yintercept = akz_krit,  col = "red", size=0.5)+
    geom_hline(yintercept = 0.7,  col = "red2", size=0.5)+
    geom_hline(yintercept = 1,  col = "red3", size=0.5)+
    
    geom_point( size=2)+
  #  geom_line(size=1.5, alpha=0.7)+
    theme(strip.text.x = element_text(family="Merck", face="bold", size = 11, colour = "#503291"),
          legend.title = element_blank(), legend.position = c(0.15,0.3), legend.background = element_rect(fill="transparent"))
  
  if(nrow(meta_long)>5){batch_plots[[batch_list[i]]] <- batch_plots[[batch_list[i]]] + # Conf-Level nur wenn mehr als 3 Punkte
    geom_line(data = conf_table1, aes(newx, upr), linetype= "longdash", col = "skyblue3", size=1)+
    geom_smooth(method=lm ,  color="transparent", fill="skyblue2", se=TRUE, formula = y ~ x, level=conf_level, fullrange =T)+
    geom_point()}
  #  geom_line(size=1.5, alpha=0.7)} 
  
  # geom_rect(aes(xmin = 24, xmax = 40, ymin = 0, ymax = 1.1),  fill="grey99", alpha=0.2)
  
   # if (is.null(schnittpunkt1_1)== FALSE) {batch_plots[[batch_list[i]]] <- batch_plots[[batch_list[i]]] + # falls kein Schnittpunkt vorhanden, kein Pfeil
   #   geom_segment(aes(x = schnittpunkt1_1, y = akz_krit, xend = schnittpunkt1_1, yend = ylow), arrow = arrow(length = unit(0.3, "cm"), angle = 20),col= "grey50", size=1)+
   #   geom_text(x=schnittpunkt1_1-2, y=ylow+0.05, label=schnittpunkt1_1, color="grey50")}
  
  
  print(batch_plots[[batch_list[i]]])
  ggsave(batch_plots[[batch_list[i]]], file=paste0("plot", i,".svg"), device = "svg", units = "cm", width = 22, height = 10)
  
}

plot_grid(plotlist = batch_plots)

ggsave("Metafolin_Batches, HOMeTHFA2, Prediction.png", plot=last_plot(), width = 35, height = 25, units="cm")
