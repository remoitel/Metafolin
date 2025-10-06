
#-------Variablen------ 
akz_krit_low <- 0 #Akzeptanzkriterium in %
akz_krit_high1 <- 1 #Akzeptanzkriterium in %
akz_krit_high2 <- 0.7 #Akzeptanzkriterium in %
akz_krit_high3 <- 0.5 #Akzeptanzkriterium in %
conf_level <- 0.90 #Vertrauensintervall z.B 0.95 f?r 95%
ylow <- 0
yhigh <- 1.1
bis_monate <- 38

#HOMeTHFA content, Level 2
 ggplot(aes(month, mean), data = meta_mean) +
  geom_point(aes(color=Batch),size=3)+ # y= aus anderer Spalte
  geom_point(aes(color=Batch),size=3)+
  geom_line(aes(color=Batch),size=1.5, alpha=0.7)+
   facet_wrap(~ Batch, ncol=2)+
  xlab("Months")+
  ylab("HOMeTHFA [area %]")+
  scale_x_continuous(expand=c(0,0), limits=c(0,100), breaks = seq(0, 48, 6)) +  # diese Zeilen brauchts damit der ConfInt weiter gef?hrt wird
  coord_cartesian(xlim=c(0,bis_monate), ylim=c(ylow,yhigh)) +
  #geom_hline(yintercept = akz_krit_low, linetype= "longdash", col = "red3", size=1)+
  geom_hline(yintercept = akz_krit_high3, linetype= "longdash", col = "red3", size=1)+
  geom_hline(yintercept = akz_krit_high2, linetype= "longdash", col = "red2", size=1)+
  geom_hline(yintercept = akz_krit_high1, linetype= "longdash", col = "red1", size=1)+
  #facet_wrap(~ batch, ncol=2)+
  #scale_color_manual(values = usecol(pal=pal_merck_vib, alpha = 1))+
  theme(strip.text.x = element_text(family = "Merck", size = 14, colour = '#149b5f'))
  #theme(strip.text.x = element_text(family = "Merck", size = 14, colour = '#149b5f'),legend.position = c(0.8, 0.2))

library(directlabels)
#♦direct.label(plot1,"angled.boxes")

ggsave("Metaffolin_HOMe.svg", plot=last_plot(), width = 20, height = 15, units="cm")
       

#-----

#HOMeTHFA content, Level 1
ggplot(aes(month, HOMeTHFA), data = meta) +
  geom_point(aes(y=min, color=Batch),size=3)+ # y= aus anderer Spalte
  geom_point(aes(y=max, color=Batch),size=3)+
  geom_line(aes(color=Batch),size=1.5, alpha=0.7)+
  xlab("Months")+
  ylab("HOMeTHFA [area %]")+
  scale_x_continuous(expand=c(0,0), limits=c(0,100), breaks = seq(0, 48, 6)) +  # diese Zeilen brauchts damit der ConfInt weiter gef?hrt wird
  coord_cartesian(xlim=c(0,bis_monate), ylim=c(ylow,yhigh)) +
  #geom_hline(yintercept = akz_krit_low, linetype= "longdash", col = "red3", size=1)+
  geom_hline(yintercept = akz_krit_high3, linetype= "longdash", col = "red3", size=1)+
  geom_hline(yintercept = akz_krit_high2, linetype= "longdash", col = "red2", size=1)+
  geom_hline(yintercept = akz_krit_high1, linetype= "longdash", col = "red1", size=1)+
  #facet_wrap(~ batch, ncol=2)+
  #scale_color_manual(values = usecol(pal=pal_merck_vib, alpha = 1))+
  theme(strip.text.x = element_text(family = "Merck", size = 14, colour = '#149b5f'))
