library(plotly)
library(viridis) 
library(ggsci)

metafolin_raw <- read_excel("//chsc1vfiler01/M291711/My Documents/R/Projekte/Metafolin/LMCA_JP.xlsx", sheet="StandSept22")

metafolin_raw <-mutate_at(metafolin_raw,8:29, as.numeric)

metafolin_long <- metafolin_raw %>% 
  filter(!Batch=="JK-1040-732") %>% 
  select(-c( "Prod", "Date", "AN", "Des.", "D-MCA")) %>% 
  pivot_longer(!c(Batch, Month,Temp), names_to = "analy_param", values_to = "value") %>% 
  mutate_at(vars(Month), as.numeric)
  


extrakt<- metafolin_long %>% 
  filter(Temp=="40") %>% 
  filter(analy_param=="Assay") 
 
  ggplot(extrakt, aes(Month, value))+
  geom_line(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(col=Batch), size=1.5, color="red", alpha=0.7)+
  geom_line(data=extrakt %>% filter(`Batch` != "LMCG0843XX"),aes(col=Batch), size=1.5, alpha=0.7)+
    geom_point(aes(col=Batch))+
    ylab("Assay [% w/w]")+
    scale_x_continuous(expand=c(0,0), limits=c(0,12),breaks = c(0,1,3,6,9,12))+
  scale_colour_grey()  +
  theme(legend.position = "none")



extrakt<- metafolin_long %>% 
  filter(Temp=="40") %>% 
  filter(analy_param=="HOMeTHFA") 
  
  ggplot(extrakt, aes(Month, value))+
    geom_line(data=extrakt %>% filter(Batch != "LMCG0843XX" | Batch != "LMCG0861XX"),aes(col=Batch), size=1.5, alpha=0.7)+
    geom_line(data=extrakt %>% filter(Batch == "LMCG0843XX"), aes(col=Batch), size=1.5, color="red", alpha=0.5)+
    geom_line(data=extrakt %>% filter(Batch == "LMCG0861XX"), aes(col=Batch), size=1.5, color="orange", alpha=0.5)+

  geom_point(aes(col=Batch))+
  geom_hline(yintercept = 1, linetype= "dashed", col = "red3", size=1)+
  ylab("HOMeTHFA")+
  scale_x_continuous(expand=c(0,0), limits=c(0,12),breaks = c(0,1,3,6,9,12))+
  scale_colour_grey()  +
  theme(legend.position = "none")
  ggsave("LFC_FT_imp.png",path = "//chsc1vfiler01/M291711/My Documents/R/Projekte/Bilder_export",dpi = 300, last_plot(),units = "cm", width = 16, height = 15)
  

extrakt<- metafolin_long %>% 
  filter(Temp==40) %>% 
  filter(analy_param=="SumT") 
  
  ggplot(extrakt, aes(Month, value))+
    geom_line(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(col=Batch), size=1.5, color="red", alpha=0.7)+
    geom_line(data=extrakt %>% filter(`Batch` != "LMCG0843XX"),aes(col=Batch), size=1.5, alpha=0.7)+
  geom_point(aes(col=Batch))+
  geom_hline(yintercept = 2.5, linetype= "dashed", col = "red3", size=1)+
  ylab("Sum Total [% w/w]")+
  scale_x_continuous(expand=c(0,0), limits=c(0,12),breaks = c(0,1,3,6,9,12))+
  scale_colour_grey()  +
  theme(legend.position = "none")


extrakt<-metafolin_long %>% 
  filter(Temp==40) %>% 
  filter(analy_param=="Mefox")
  
  ggplot(extrakt, aes(Month, value))+
  geom_line(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(col=Batch), size=1.5, color="red", alpha=0.7)+
  geom_line(data=extrakt %>% filter(`Batch` != "LMCG0843XX"),aes(col=Batch), size=1.5, alpha=0.7)+
  geom_point(aes(col=Batch))+
  geom_hline(yintercept = 1, linetype= "dashed", col = "red3", size=1)+
  ylab("Mefox")+
  scale_x_continuous(expand=c(0,0), limits=c(0,12),breaks = c(0,1,3,6,9,12))+
  scale_colour_grey()  +
  theme(legend.position = "none")


extrakt<-metafolin_long %>% 
  filter(Temp==40) %>% 
  filter(analy_param=="Mefguan") 
  # filter(Batch != "LMCG0843XX") %>% 
  
  ggplot(extrakt, aes(Month, value))+
    geom_line(data=extrakt %>% filter(`Batch` == "LMCG0843XX"), aes(col=Batch), size=1.5, color="red", alpha=0.7)+
    geom_line(data=extrakt %>% filter(`Batch` != "LMCG0843XX"),aes(col=Batch), size=1.5, alpha=0.7)+
  geom_point(aes(col=Batch))+
  geom_hline(yintercept = 0.5, linetype= "dashed", col = "red3", size=1)+
  # ylim(0,0.2)+
  ylab("Mefguan")+
  scale_x_continuous(expand=c(0,0), limits=c(0,12),breaks = c(0,1,3,6,9,12)) +
  # geom_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)+
  scale_colour_grey()  +
  theme(legend.position = "none")

  
#----- Grid -------
  metafolin_long <- mutate_at(metafolin_long, vars(Temp), as.factor)
  hline_dat = data.frame(analy_param=rep(c(  "ABGA", "HOMeTHFA","Mefox", "FA", "CH2-THFA",  
                                            "DiMe-THFA", "SumImp","H2O","H2O", "Assay", "Assay", "Me-THPA", "THFA", "DHFA"), each = length(levels(metafolin_long$Temp))), # neue Spez.
                         temp=rep(c("-20°C", "5°C", "25°C", "40°C"),14),
                         new_limit=rep(c(0.5, 1,1,0.5,0.5,0.15,2.5, 6,17,95,102, 0.5, 0.5,0.5  ),each=length(levels(metafolin_long$Temp))))
  hline_dat$temp <- factor(hline_dat$temp, levels = c("-20°C", "5°C", "25°C","40°C"))
  
  
  metafolin_long %>% 
    filter(Temp=="40") %>% 
    filter(analy_param %in% c("H2Ofree","H2O", "ABGA","CH2-THFA","DiMe-THFA", "HOMeTHFA", "SumT",  "Mefox",  "FA","Me-THPA", "THFA", "DHFA")) %>% 
    # filter(parse_number(Batch) < 680 | parse_number(Batch) == 21600 ) %>% 
    mutate(analy_param = replace(analy_param, analy_param=="SumT", "SumImp") ) %>%  
    mutate(analy_param = replace(analy_param, analy_param=="H2Ofree", "Assay") ) %>%  
    
    ggplot(aes(Month, value, group= Batch, col= Batch)) +
    xlab("Months")+
    ylab(" % w/w")+
    geom_line(data=. %>% filter(Batch != "LMCG0843XX" | Batch != "LMCG0861XX"),aes(col=Batch), size=1, color="#2dbecd", alpha=0.7)+
    geom_line(data=. %>% filter(Batch == "LMCG0843XX"), aes(col=Batch), size=1, color="#eb3c96", alpha=1)+
    geom_line(data=. %>% filter(Batch == "LMCG0861XX"), aes(col=Batch), size=1, color="orange", alpha=1)+
    geom_hline(data=hline_dat, aes(yintercept=new_limit), linetype= "twodash", col = "orange2", size=1)+
    scale_colour_grey()  +
    facet_wrap(~ analy_param, scales = "free", ncol=3 )+
    scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12))+
    theme(strip.text.x = element_text(family="Merck",  size = 20, colour = "#503291"),
          strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
          axis.text.x = element_text(size = 18, colour = "#503291", angle=0),
          axis.text.y = element_text(size = 18, colour = "#503291", angle=0),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.position = "none", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal", # "bottom"
          strip.background =element_rect(fill="#ffc832")
          # , panel.background  = element_rect(fill = "#fff7e2")
          ) 

  ggsave("LMCA_LMSR_40.png",path = "//chsc1vfiler01/M291711/My Documents/R/Projekte/Bilder_export",dpi = 300, last_plot(),units = "cm", width = 16, height = 15)
  
  
  metafolin_long %>% 
    filter(Temp=="25") %>% 
    filter(analy_param %in% c("H2Ofree","H2O", "ABGA","CH2-THFA","DiMe-THFA", "HOMeTHFA", "SumT",  "Mefox",  "FA","Me-THPA", "THFA", "DHFA")) %>% 
    # filter(parse_number(Batch) < 680 | parse_number(Batch) == 21600 ) %>% 
    mutate(analy_param = replace(analy_param, analy_param=="SumT", "SumImp") ) %>%  
    mutate(analy_param = replace(analy_param, analy_param=="H2Ofree", "Assay") ) %>%  
    
  
    ggplot(aes(Month, value, group= Batch, col= Batch)) +
    xlab("Months")+
    ylab(" % w/w")+
    geom_line(data=. %>% filter( Batch != "LMCG0861XX"),aes(col=Batch), size=1, color="#2dbecd", alpha=0.7)+
    geom_line(data=. %>% filter(Batch == "LMCG0861XX"), aes(col=Batch), size=1, color="orange", alpha=1)+
    geom_hline(data=hline_dat, aes(yintercept=new_limit), linetype= "twodash", col = "orange2", size=1)+
    scale_colour_grey()  +
    facet_wrap(~ analy_param, scales = "free", ncol=3 )+
    scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12))+
    theme(strip.text.x = element_text(family="Merck",  size = 20, colour = "#503291"),
          strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
          axis.text.x = element_text(size = 18, colour = "#503291", angle=0),
          axis.text.y = element_text(size = 18, colour = "#503291", angle=0),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.position = "none", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal", # "bottom"
          strip.background =element_rect(fill="#ffc832")
          # , panel.background  = element_rect(fill = "#fff7e2")
    ) 
  
  ggsave("LMCA_LMSR25C.png",path = "//chsc1vfiler01/M291711/My Documents/R/Projekte/Bilder_export",dpi = 300, last_plot(),units = "cm", width = 16, height = 15)
  
  
  metafolin_long %>% 
    filter(Temp=="5") %>% 
    filter(analy_param %in% c("H2Ofree","H2O", "ABGA","CH2-THFA","DiMe-THFA", "HOMeTHFA", "SumT",  "Mefox",  "FA","Me-THPA", "THFA", "DHFA")) %>% 
    # filter(parse_number(Batch) < 680 | parse_number(Batch) == 21600 ) %>% 
    mutate(analy_param = replace(analy_param, analy_param=="SumT", "SumImp") ) %>%  
    mutate(analy_param = replace(analy_param, analy_param=="H2Ofree", "Assay") ) %>%  
    
    ggplot(aes(Month, value, group= Batch, col= Batch)) +
    xlab("Months")+
    ylab(" % w/w")+
    geom_line(data=. %>% filter( Batch != "LMCG0861XX"),aes(col=Batch), size=1, color="#2dbecd", alpha=0.7)+
    geom_line(data=. %>% filter(Batch == "LMCG0861XX"), aes(col=Batch), size=1, color="orange", alpha=1)+
    geom_hline(data=hline_dat, aes(yintercept=new_limit), linetype= "twodash", col = "orange2", size=1)+
    scale_colour_grey()  +
    facet_wrap(~ analy_param, scales = "free", ncol=3 )+
    scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12))+
    theme(strip.text.x = element_text(family="Merck",  size = 20, colour = "#503291"),
          strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
          axis.text.x = element_text(size = 18, colour = "#503291", angle=0),
          axis.text.y = element_text(size = 18, colour = "#503291", angle=0),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.position = "none", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal", # "bottom"
          strip.background =element_rect(fill="#ffc832")
          # , panel.background  = element_rect(fill = "#fff7e2")
    ) 
  
  ggsave("LMCA_LMSR5C.png",path = "//chsc1vfiler01/M291711/My Documents/R/Projekte/Bilder_export",dpi = 300, last_plot(),units = "cm", width = 16, height = 15)
  
  
  
  metafolin_long %>% 
    filter(Batch=="LMCG0843XX" | Batch=="LMCG0861XX" ) %>% 
    filter(analy_param %in% c("Assay","H2O", "ABGA","CH2-THFA","DiMe-THFA", "HOMeTHFA","Mefguan", "SumT",  "Mefox",  "FA")) %>% 
    # filter(parse_number(Batch) < 680 | parse_number(Batch) == 21600 ) %>% 
    mutate(analy_param = replace(analy_param, analy_param=="SumT", "SumImp") ) %>%  
    
    ggplot(aes(Month, value, group= Batch, col= Temp)) +
    xlab("Months")+
    ylab(" % w/w")+
    geom_line( )+
    geom_line( )+
    facet_wrap(~ analy_param, scales = "free", ncol=3 )+
    scale_x_continuous(expand=c(0,0), limits=c(-0.5,12.5),breaks = c(0,1,2,3,6,9,12))+
    theme(strip.text.x = element_text(family="Merck",  size = 20, colour = "#503291"),
          strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
          axis.text.x = element_text(size = 18, colour = "#503291", angle=0),
          axis.text.y = element_text(size = 18, colour = "#503291", angle=0),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.position = "bottom", legend.background = element_rect(fill="transparent"), legend.direction = "horizontal", # "bottom"
          strip.background =element_rect(fill="#ffc832")
          # , panel.background  = element_rect(fill = "#fff7e2")
    )
  
  
  (copdat <-read.delim("clipboard", sep="\t", header =F))
  apply(copdat,2,mean)
  