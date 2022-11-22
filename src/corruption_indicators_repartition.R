require(plotly)
require(data.table)
require(dplyr)
require(tidyr)
data <- read.csv(file = "./data/wgidataset.csv",sep = ",",dec = ".")
country_data <- data[which(data$countryname=="United States"),c("year","vae","pve","gee","rqe","cce","rle")]
country_data$Sum <- apply(country_data[,2:7],1,sum)
country_data <- gather(country_data, "Indicator", "Value", vae:rle)
country_data$Year <- country_data$year


#1st try
fig2 <- plot_ly(country_data, x = ~year, y = ~vae, type = 'bar',name = 'vae')
fig2 <- fig2 %>% add_trace(y = ~country_data$pve, name = 'pve')
fig2 <- fig2 %>% add_trace(y = ~country_data$gee, name = 'gee')
fig2 <- fig2 %>% add_trace(y = ~country_data$rqe, name = 'rqe')
fig2 <- fig2 %>% add_trace(y = ~country_data$cce, name = 'cce')
fig2 <- fig2 %>% add_trace(y = ~country_data$rle, name = 'rle')

fig2 <- fig2 %>% layout(yaxis = list(title = 'Count'), barmode = 'stack',xaxis = list(country_data$year) )
fig2
labels_full <-c("Voice and Accountability",
           "Regulatory Quality",
           "Rule of Law",
           " Political Stability and Absence of Violence/Terrorism",
           "Government Effectiveness",
           " Control of Corruption")
library(viridis)
library(hrbrthemes)
Indicators <- rep(unique(country_data$Indicator),times = length(unique(country_data$Year)))
# stacked area chart
area <- ggplot(country_data, aes(x=Year, y=Value, fill=Indicators)) +
  geom_area(alpha=0.6 , size=.5, colour="white") + ggtitle("Repartition of WGI Indicators in the US from 1996 to 2021") +
  scale_fill_viridis(discrete = T,labels = labels_full)+ scale_x_continuous(breaks = round(seq(min(country_data$Year), max(country_data$Year), by = 1))) +
   theme_ipsum(axis_title_just = c(2), axis_title_size = 8 , plot_title_size = 10 
               ,plot_title_face = "plain",axis_text_size = 6) +theme(legend.text = element_text(size= 6),legend.title = element_text(size = 8)
                                                                      ,axis.text.x  = element_text(angle = 90)) + geom_vline(xintercept=c(2001,2003,2004,2007,2016), color= "#440154",size = 1)+ 
   annotate(geom = "text",x = c(2001.3,2003.3,2004.3,2007.3,2016.3), y = c(3,3,3,3,3), label = c("Attentats du 11 Septembre","Guerre en Irak","Reelection de G.Bush","Crise des Subrpimes","Election D.Trump"), color= "black",angle = 90,size=3)

area
