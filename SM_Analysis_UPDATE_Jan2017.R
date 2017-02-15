library(lubridate)
library(ggplot2)
library(dplyr)
library(ggmap)
library(readxl)
library(RSocrata)

# read in data, filter to include only 5/20 to 12/31

# read in geographical data

data <- read.socrata("https://data.smgov.net/Public-Safety/Police-Incidents/kn6p-4y74")

data <-  na.omit(data)

sm <-  qmap("Santa Monica", zoom = 13, maptype = "road")

data <-  data %>% 
  mutate(day = day(data$Date.Occurred),
         month = month(data$Date.Occurred,T,F),
         year = year(data$Date.Occurred),
         weekday = wday(data$Date.Occurred,T,F))

data <- rbind(
  data %>% filter(month %in% c("June","July","August",
                               "September","October","November","December")),
  data %>% filter(month == "May" & day > 19)
)

# preliminary graphs

data %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("red",10),"blue"),col="black",alpha=.7)+
  scale_x_continuous(breaks = 2006:2016)+
  ylab("")+
  xlab("")+
  ggtitle("Number of Police Incidents 5/20 - 12/31 by Year")

sm +
  stat_density2d(data = data, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Number of Police Incidents 5/20 - 12/31 by Year")+
  facet_wrap(~year)

# find crimes which saw a significany increase

avgpre2016 <-  data %>% 
  filter(year != 2016) %>% 
  group_by(UCR.Description) %>% 
  summarise(avg_2006_2015 = round(n()/10))

count2016 <-  data %>% 
  filter(year == 2016) %>% 
  group_by(UCR.Description) %>% 
  summarise(count_2016 = n())

count2015 <-  data %>% 
  filter(year == 2015) %>% 
  group_by(UCR.Description) %>% 
  summarise(count_2015 = n())

table1 <-  inner_join(count2016,avgpre2016) %>% 
  filter(count_2016 > avg_2006_2015)

table2 <-  inner_join(count2016,count2015) %>% 
  filter(count_2016 > count_2015)

pvalues <-  mapply(function(x,y) poisson.test(c(x,y),
                                              alternative = "greater")$p.value,
table1$count_2016,table1$avg_2006_2015)

pvalues2 <-  mapply(function(x,y) poisson.test(c(x,y),
                                              alternative = "greater")$p.value,
                   table2$count_2016,table2$count_2015)

table1 <-  cbind(pvalues,table1)

table2 <-  cbind(pvalues2,table2)

table1 <-  table1 %>% 
  filter(pvalues < .05)

table2 <-  table2 %>% 
  filter(pvalues2 < .05)

sig_increase <- inner_join(table1,table2)

# plots demonstrating increase

data %>% 
  filter(UCR.Description == "Larceny - From Vehicle") %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("blue",10),"red"),col="black",alpha=.7)+
  scale_x_continuous(breaks = 2006:2016)+
  labs(x="",y="",title="Vehicular Larceny Incidents 5/20 - 12/31 by Year")


data %>% 
  filter(UCR.Description %in% c("Narco Sales-Synthetic","Narco Sales Opiate/Cocaine")) %>% 
  ggplot(aes(year))+
  geom_bar(fill = c(rep("blue",10),"red"),col="black",alpha=.7)+
  scale_x_continuous(breaks = 2006:2016)+
  labs(x="",y="",title="Narco Sales (Cocaine and Synthetic) 5/20 - 12/31 by Year")

hmap2016 <- data %>% 
  filter(UCR.Description == "Larceny - From Vehicle" & year == 2016)

hmappre2016 <-  data %>% 
  filter(UCR.Description == "Larceny - From Vehicle" & year != 2016)

hmap2015 <-  data %>% 
  filter(UCR.Description == "Larceny - From Vehicle" & year == 2015)

sm +
  stat_density2d(data = hmap2016, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for Vehicular 5/20/16 - 12/31/16")

sm +
  stat_density2d(data = hmappre2016, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for Vehicular Larceny 5/20 - 12/31, 2006 - 2015")

sm +
  stat_density2d(data = hmap2015, 
                 aes(Longitude,Latitude,fill=..level..),
                 geom = "polygon",
                 alpha = .5)+
  scale_fill_gradient(high = "red", low = "yellow")+
  ggtitle("Heatmap for Vehicular Larceny June-October 2015")

drugs2016 <-  data %>% 
  filter(UCR.Description %in% c("Narco Sales-Synthetic", "Narco Sales Opiate/Cocaine") &
           year == 2016)

drugs2015 <-  data %>% 
  filter(UCR.Description %in% c("Narco Sales-Synthetic", "Narco Sales Opiate/Cocaine") &
           year == 2015)

sm +
  geom_point(data = drugs2016, aes(Longitude,Latitude), 
             size = 6, alpha = .7, shape = 16, col = "red")+
  ggtitle("2016 (red) vs 2006-2015 (blue) Narco Sales June-October")

sm +
  geom_point(data = drugs2015, aes(Longitude,Latitude), 
             size = 6, alpha = .7, shape = 16, col = "red")+
  ggtitle("2016 (red) vs 2006-2015 (blue) Narco Sales June-October")

# find crimes which decreased

table3 <-  inner_join(count2016,avgpre2016) %>% 
  filter(count_2016 < avg_2006_2015)

table4 <-  inner_join(count2016,count2015) %>% 
  filter(count_2016 < count_2015)
  
pvalues3 <-  mapply(function(x,y) poisson.test(c(x,y),
                                              alternative = "greater")$p.value,
                    table3$avg_2006_2015,table3$count_2016)

pvalues4 <-  mapply(function(x,y) poisson.test(c(x,y),
                                               alternative = "greater")$p.value,
                    table4$count_2015,table4$count_2016)

table3 <- cbind(pvalues3, table3)

table4 <-  cbind(pvalues4, table4)

table3 <-  table3 %>% 
  filter(pvalues3 < .05)

table4 <- table4 %>% 
  filter(pvalues4 < .05)

sig_decrease <- inner_join(table3,table4)

# plots of decrease

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

# cosmetics to make tables look more cosmetic (may not work with new data)

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


## more analysis of vehicular larceny

vlarceny <- data %>%filter(UCR.Description=="Larceny - From Vehicle")

vlarceny$month <- month(vlarceny$Date.Occurred)

vlarceny <- vlarceny %>% 
  group_by(year,month,day) %>% 
  summarise(count=n()) %>% 
  mutate(day_index=sum(days_in_month(1:month))-(days_in_month(month)-day)-139) 

vlarceny %>% 
  group_by(year) %>% 
  mutate(cummy=cumsum(count)) %>% 
  ggplot(aes(day_index,cummy,col=factor(year)))+
  geom_line(size=1.1)+
  scale_color_manual("Year",values=c(rep("lightblue",10),"red"))+
  labs(x="Days after May 20", y="Total Number of Vehicular Larcenies",
       title="Total Number of Vehicular Larcenies over time by Year")

## more analysis of DUI

DUI <- data %>%filter(UCR.Description=="DUI")

DUI$month <- month(DUI$Date.Occurred)

DUI <- DUI %>% 
  group_by(year,month,day) %>% 
  summarise(count=n()) %>% 
  mutate(day_index=sum(days_in_month(1:month))-(days_in_month(month)-day)-139) 

DUI %>% 
  group_by(year) %>% 
  mutate(cummy=cumsum(count)) %>% 
  ggplot(aes(day_index,cummy,col=factor(year)))+
  geom_line(size=1.1)+
  scale_color_manual(values=c(rep("lightblue",10),"red"))+
  labs(x="Days after May 20", y="Total Number of Vehicular Larcenies",
       title="Total Number of Vehicular Larcenies over time by Year")

# more analysis of narco drugs

narco_sales <- data %>%filter(UCR.Description %in% 
                                c("Narco Sales-Synthetic", "Narco Sales Opiate/Cocaine"),
                              year %in% c(2009,2016))

narco_sales$month <- month(narco_sales$Date.Occurred)

narco_sales <- narco_sales %>% 
  group_by(year,month,day) %>% 
  summarise(count=n()) %>% 
  mutate(day_index=sum(days_in_month(1:month))-(days_in_month(month)-day)-139) 

test <- narco_sales %>% 
  group_by(year) %>% 
  mutate(cummy=cumsum(count)) %>% 
  ggplot(aes(day_index,cummy,col=factor(year)))+
  geom_line(size=1.1)+
  scale_color_manual(values=c("lightblue","red"))+
  labs(x="Days after May 20", y="Total Number of Vehicular Larcenies",
       title="Total Number of Vehicular Larcenies over time by Year")

  




