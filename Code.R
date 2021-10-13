library(tidytuesdayR)
library(tidyverse)
library(reshape2)
tuesdata <- tidytuesdayR::tt_load('2021-10-12')
tuesdata
tuesdata$`global-fishery-catch-by-sector`->sector


sector%>%drop_na()%>%
  distinct(Year,.keep_all = TRUE)%>%
  select(-c(Entity,Code))->sector1
colnames(sector1)
sector1
melt(sector1,id.vars = "Year",measure.vars = c("Artisanal (small-scale commercial)",
                                               "Discards",
                                               "Industrial (large-scale commercial)",
                                               "Recreational",
                                               "Subsistence"), value.name = "value")->ms
ms$value<-(ms$value)/1000
ms$value<-round(ms$value)
ms%>%
  arrange(variable)->ms
colnames(ms)<-c("Year","Sector","Value")
ms
summary(ms)
ggplot(ms, aes(x=Year,y=Sector))+
  geom_point(aes(size=Value),colour="#051e3e", alpha=0.8)+
  scale_y_discrete(limits=rev)+
  scale_x_continuous(limits = c(1950,2010),breaks=c(1950,1960,1970,1980,1990,2000,2010))+
  scale_size(name="Number of fish (in thousand tonnes)", breaks=c(10000,20000,30000,40000,50000,60000,70000, 80000, 90000), labels=c("10000","20000","30000","40000","50000","60000","70000","80000","90000"))+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="#e7eff6"),
        plot.background = element_rect(fill="#e7eff6"),
        legend.background = element_rect(fill="#e7eff6"),
        legend.key = element_rect(fill="#e7eff6"),
        legend.title = element_text(face="bold"),
        legend.position = "top",
        legend.text = element_text(face="bold",colour = "black"),
        axis.text = element_text(face="bold", size=10, colour = "black"))+
  guides(size = guide_legend(nrow = 1))+
  labs(title = "A GLIMPSE OF THE SECTOR-WISE GLOBAL FISHERY CATCH", subtitle = "A breakdown of the global wild fishery catch by sector. This relates only to wild fishery catch, and does not include
aquaculture (fish farming) production",
       caption = "Date: Our World in Data|Design and analysis: @annapurani93")+
  theme(plot.title = element_text(colour = "black",face="bold",size=21, hjust=0.5),
        plot.subtitle = element_text(colour = "black",size=11,hjust=0.5),
        plot.caption = element_text(colour = "black",size=8))->p

ggsave("dotplot.png",p,width=16,height=8,dpi=500)
p        
