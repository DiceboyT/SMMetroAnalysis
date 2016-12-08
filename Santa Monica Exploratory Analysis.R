library(lubridate)
library(ggplot2)
library(dplyr)
library(ggmap)
library(readxl)

data = read.csv("Police_Incidents (1).csv")

data = na.omit(data)

sm = qmap("Santa Monica", zoom = 13, maptype = "road")

data$Date.Occurred = mdy_hms(data$Date.Occurred)

data = data %>% 
  mutate(day = day(data$Date.Occurred),
         month = month(data$Date.Occurred,T,F),
         year = year(data$Date.Occurred),
         weekday = wday(data$Date.Occurred,T,F)) %>% 
  filter(month %in% c("June","July","August","September","October"))

data %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("red",10),"blue"))+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("Number of Police Incidents June-October")

sm +
  stat_density2d(data = data, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for all crimes June-October 2006-2016")+
  facet_wrap(~year)

# which crimes increased when the train came in?

avgpre2016 = data %>% 
  filter(year !=2016) %>% 
  group_by(UCR.Description) %>% 
  summarise(avg_2006_2015 = n()/10)

count2016 = data %>% 
  filter(year == 2016) %>% 
  group_by(UCR.Description) %>% 
  summarise(count_2016 = n())

count2015 = data %>% 
  filter(year == 2015) %>% 
  group_by(UCR.Description) %>% 
  summarise(count_2015 = n())

table1 = inner_join(count2016,avgpre2016) %>% 
  filter(count_2016 > avg_2006_2015)

table2 = inner_join(count2015,count2016) %>% 
  filter(count_2016 > count_2015)

# intead of for loop, use this code
# pvalues = mapply(function(x,y) poisson.test(c(as.integer(x),as.integer(y)),
# alternative = "greater")$p.value,
# table1$count_2016,table1$avg_2006_2015)

fun1 = function(x,y){
  z <- poisson.test(c(as.integer(x),as.integer(y)), alternative = "greater")
  z$p.value
}

pvalues <- vector("numeric", 49L)
for(i in 1:length(table1$count_2016)){
  pvalues[i] <- print(fun1(table1$count_2016[i],table1$avg_2006_2015[i]))
}

pvalues2 <- vector("numeric", 36L)
for(i in 1:length(table2$count_2016)){
  pvalues2[i] <- print(fun1(table2$count_2016[i],table2$count_2015[i]))
}

table1 = cbind(pvalues,table1)

table2 = cbind(pvalues2,table2)

table1 = table1 %>% 
  filter(pvalues < .05)

table2 = table2 %>% 
  filter(pvalues2 < .05)

sig_increase = inner_join(table1,table2)

data %>% 
  filter(UCR.Description == "Larceny - From Vehicle") %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("blue",10),"red"))+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("Vehicular Larceny June-October by Year")

data %>% 
  filter(UCR.Description %in% c("Narco Sales-Synthetic","Narco Sales Opiate/Cocaine")) %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("blue",10),"red"))+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("Synthetic/Opiate/Cocaine Narcotics Sales June-October by Year")

data %>% 
  filter(UCR.Description == "Carry/Poss Weapon") %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("blue",10),"red"))+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("Carry/Posseion of a Weapon June-October by Year")

hmap2016 = data %>% 
  filter(UCR.Description == "Larceny - From Vehicle" & year == 2016)

hmappre2016 = data %>% 
  filter(UCR.Description == "Larceny - From Vehicle" & year != 2016)

hmap2015 = data %>% 
  filter(UCR.Description == "Larceny - From Vehicle" & year == 2015)

sm +
  stat_density2d(data = hmap2016, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for Vehicular Larceny June-October 2016")

sm +
  stat_density2d(data = hmappre2016, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for Vehicular Larceny June-october 2006-2015")

sm +
  stat_density2d(data = hmap2015, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for Vehicular Larceny June-October 2015")

hmapdrugs = data %>% 
  filter(UCR.Description %in% c("Narco Sales-Synthetic", "Narco Sales Opiate/Cocaine"))

sm +
  geom_point(data = hmapdrugs, aes(Longitude,Latitude, col = factor(year)), 
             size = 2, alpha = .7, shape = 16)+
  scale_color_manual(values = c(rep("blue",10),"red"),guide = F)+
  ggtitle("2016 (red) vs 2006-2015 (blue) Narco Sales June-October")

# which crimes decreased when the train came in?

table3 = inner_join(count2016,avgpre2016) %>% 
  filter(count_2016 < avg_2006_2015)

table4 = inner_join(count2015,count2016) %>% 
  filter(count_2016 < count_2015)
  
pvalues3 <- vector("numeric", 32L)
for(i in 1:length(table3$count_2016)){
  pvalues3[i] <- print(fun1(table3$avg_2006_2015[i],table3$count_2016[i]))
}

pvalues4 <- vector("numeric", 29L)
for(i in 1:length(table4$count_2016)){
  pvalues4[i] <- print(fun1(table4$count_2015[i],table4$count_2016[i]))
}

table3 = cbind(pvalues3, table3)

table4 = cbind(pvalues4, table4)

table3 = table3 %>% 
  filter(pvalues3 < .05)

table4 = table4 %>% 
  filter(pvalues4 < .05)

sig_decrease = inner_join(table3,table4)

data %>% 
  filter(UCR.Description == "DUI") %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("red",10),"blue"))+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("DUI June-October by Year")

hmap2016DUI = data %>% 
  filter(UCR.Description == "DUI" & year == 2016)

hmappre2016DUI = data %>% 
  filter(UCR.Description == "DUI" & year != 2016)

hmap2015DUI = data %>% 
  filter(UCR.Description == "DUI" & year == 2015)

sm +
  stat_density2d(data = hmap2016DUI, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for DUI June-October 2016")

sm +
  stat_density2d(data = hmap2015DUI, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for DUI June-October 2015")

sm +
  stat_density2d(data = hmappre2016DUI, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for DUI June-October 2006-2015")

data %>% 
  filter(UCR.Description == "Public Intoxication") %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("red",10),"blue"))+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("Public Intoxication June-October by Year")

data %>% 
  filter(UCR.Description == "Vandalism") %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("red",10),"blue"))+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("Vandalism June-October by Year")

sig_increase <-  sig_increase[,c(2:4,6,1,5)]

colnames(sig_increase) <- c("Crime","2016 count","2006-2015 Avg. count",
                            "2015 count", 
                            "p value (diff btw 2016 count and 2006-2015 avg)",
                            "p value (diff btw 2016 and 2015 counts)")

sig_decrease <- sig_decrease[,c(2:4,6,1,5)]

colnames(sig_decrease) <- c("Crime","2016 count","2006-2015 Avg. count",
                            "2015 count", 
                            "p value (diff btw 2016 count and 2006-2015 avg)",
                            "p value (diff btw 2016 and 2015 counts)")

  



  




