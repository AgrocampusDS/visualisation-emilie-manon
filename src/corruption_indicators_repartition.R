#########################
### required Packages
#########################
require(plotly)
require(data.table)
require(dplyr)
require(tidyr)
library(viridis)
library(hrbrthemes)

###########################################
### Importing Data and Preprocessing 
###########################################

data <- read.csv(file = "./data/wgidataset.csv",sep = ",",dec = ".")
country_data <- data[which(data$countryname=="United States"),c("year","vae","pve","gee","rqe","cce","rle")]
country_data$Sum <- apply(country_data[,2:7],1,sum)
country_data <- gather(country_data, "Indicator", "Value", vae:rle)
country_data$Year <- country_data$year
###########################################
### Plot construction
###########################################

labels_full <-c("Voice and Accountability",
           "Regulatory Quality",
           "Rule of Law",
           " Political Stability and Absence of Violence/Terrorism",
           "Government Effectiveness",
           " Control of Corruption")

Indicators <- rep(unique(country_data$Indicator),times = length(unique(country_data$Year)))
##############################################
###Fonction réalisant le graph pour la répartition des indices
##############################################


indicator.repartition <- function(input,output){
  output$repartition <- 
    renderPlot({
      ggplot(country_data, aes(x=Year, y=Value, fill=Indicators)) + scale_y_continuous(limits = c(0,9), expand = c(0, 0)) +
  geom_area(alpha=0.6 , size=0.5, colour="white") + labs(title = 
          "Repartition of WGI Indicators in the US from 1996 to 2021"
          , subtitle = " The WGI is a mean of 6 indicators, we represented these indicator's contribution to the WGI score each year in the US. \nOn top of that, we added major geopolitical incidents that occured from 1996 to 2021") +
  scale_fill_viridis(discrete = T,labels = labels_full)+ scale_x_continuous(breaks = round(seq(min(country_data$Year), max(country_data$Year), by = 1))) +
   theme_ipsum(axis_title_just = c(2), axis_title_size = 14 , plot_title_size = 18 
               ,plot_title_face = "bold",axis_text_size = 14,subtitle_face = "italic",subtitle_size  = 9) +theme(legend.text = element_text(size= 7),legend.title = element_text(size = 15,face = "bold")
                                                                      ,axis.text.x  = element_text(angle = 90,size = 10)) + geom_vline(xintercept=c(2001,2003,2004,2007,2016), color= "#440154",size = 1)+ 
   annotate(geom = "text",x = c(2001.3,2003.3,2004.3,2007.3,2016.3), y = c(4,4,4,4,4), label = c("11/9 Terrorist attack","Irak War debuting","G.Bush's Reelection","Subrpimes Crisis","D.Trump's Election"), color= "black",angle = 90,size=3)
 })
}

