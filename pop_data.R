library(readxl)
library(tidyverse)
library(grid)
library(gridExtra)
Ngr_mex <- read_excel("C:/Users/hp/Desktop/Book1.xlsx")
Ngr_mex<-Ngr_mex %>% mutate(pop_diff= Nigeria- Mexico) %>%
  pivot_longer(cols = c("Nigeria", "Mexico")) %>% rename(Country= name, Population= value)
pop_stat<- Ngr_mex %>% group_by(Country) %>%
  summarise(mean_pop= mean(Population), SD= sd(Population))%>%
  mutate( q1= mean_pop-(1*SD),q3= mean_pop+ (1*SD))
Nigeria_stat<- pop_stat %>% filter(Country== "Nigeria")
Mexico_stat<- pop_stat %>%  filter(Country=="Mexico")
Nigeria<- Ngr_mex %>% filter(Country== "Nigeria")
Mexico<- Ngr_mex %>% filter(Country== "Mexico")
picture<- readPNG ("C:/Users/hp/Downloads/coat_of_arm.png")
ngr_mex_pop_diff<- Ngr_mex %>% filter(Country=="Mexico") %>% mutate(diff_pos= Population+ (pop_diff/2))
a<-ggplot(data = Ngr_mex)+
  geom_rect(xmin= (Nigeria_stat$mean_pop + 0.5*Nigeria_stat$SD), xmax= max(Nigeria$Population), ymin= 1999.5, ymax= 2020.5,
            fill="green", alpha= .009)+
  geom_rect(xmin= min(Mexico$Population), xmax= max(Mexico$Population), ymin= 1999.5, ymax= 2020.5,
            fill= "green", alpha=.009)+geom_segment(data = Mexico, mapping = aes(x= Nigeria$Population, y= Nigeria$Year,
                                                                                xend= Population, yend= Year),
                                                   size= 4.8, color= "gray19", alpha= 0.5)+
  geom_segment(data = Mexico, mapping = aes(x= Nigeria$Population, y= Nigeria$Year,
                                            xend= Population, yend= Year),
               size= 4.8, color= "gray85", alpha= 0.5)+
  geom_point(data= Nigeria, mapping = aes(x=Nigeria$Population, y= Nigeria$Year),
             color= "green", size=5)+
  geom_point(data = Mexico, mapping = aes(x= Mexico$Population,
                                          y= Mexico$Year),
             color= "blue", size=5)+
  geom_text(data = ngr_mex_pop_diff, mapping = aes(label= paste("∆",ngr_mex_pop_diff$pop_diff)),
            x= ngr_mex_pop_diff$diff_pos, y= ngr_mex_pop_diff$Year,
            color= "black", fill= "white", size= 4)+
  theme_minimal()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_line(color= "red"))+
  scale_x_continuous(breaks =c(100000000,125000000,150000000, 175000000,200000000, 225000000,250000000),
                  labels = c("100m","125m","150m","175m","200m","225m","250m"))+
  scale_y_continuous(breaks = c(2000:2020))+
  labs(x="Population", y="",
       subtitle = "created by Akinola Samson",
       caption = "Data source: https://data.worldbank.org/indicator/SP.POP.TOTL \n #tidytuesday \n @Samson_Akinola1")
b<- grobTree(
  gp= gpar(fontsize= 13),
  textGrob(label ="Human population growth in ", name = "title1", x= unit(7, "lines"),
           y= unit(1.4, "lines")),
  textGrob(label =" Nigeria ",name = "title2",
           x=grobWidth("title1")+unit(2.5, "lines"), y= unit(1.4, "lines"), gp=gpar(col = "green", fontface= "bold")),
  textGrob(label ="from year 2000 to 2020 compared with", name = "title3",
           x= grobWidth("title1")+grobWidth("title2")+unit(9, "lines"),
           y=unit(1.4, "lines")),
  textGrob(label =" Mexico ", name = "title4",
           x=grobWidth("title1")+grobWidth("title2")+grobWidth("title3")+
             unit(2, "lines"),
           y=unit(1.4, "lines"), gp=gpar(col= "blue", fontface= "bold"))
)

plot.new()
d<- arrangeGrob(a, top = b, padding= unit(3, "line"))
grid.draw(d)
