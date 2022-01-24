library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

#import data for 2021
jan2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202101-divvy-tripdata.csv")
feb2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202102-divvy-tripdata.csv")
mar2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202103-divvy-tripdata.csv")
apr2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202104-divvy-tripdata.csv")
may2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202105-divvy-tripdata.csv")
jun2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202106-divvy-tripdata.csv")
jul2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202107-divvy-tripdata.csv")
aug2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202108-divvy-tripdata.csv")
sep2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202109-divvy-tripdata.csv")
oct2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202110-divvy-tripdata.csv")
nov2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202111-divvy-tripdata.csv")
dec2021<-read_csv("C:/Users/yao.ge/OneDrive - csisolar/Documents/csv/202112-divvy-tripdata.csv")

#combine as 2021 data
data2021 <- rbind(jan2021, feb2021, mar2021, apr2021, may2021,jun2021,jul2021,aug2021,sep2021,oct2021,nov2021,dec2021)

#Add columns in 2021 data
data2021 <- data2021 %>%
  mutate (ride_length=difftime(ended_at, started_at,units='mins')) %>%
  mutate (day_of_week=wday(started_at, label=TRUE)) %>%
  mutate (month=month(started_at, label=TRUE)) 


#Analyze data2021
overview2021 <- data2021 %>%
  group_by(member_casual,rideable_type,month,day_of_week) %>%  
  summarise(count_of_rides = n()
            ,average_duration = mean(ride_length)
            ,max_duration=max(ride_length)) %>%
  arrange(member_casual,rideable_type,month,day_of_week)

View(overview2021)
#write.csv(overview2021, file = "overview.csv")

#Draw picture

ggplot(data=overview2021,aes(x=day_of_week,y=average_duration))+
  geom_bar(stat="identity", width=0.5,color="blue",fill='yellow')

ggplot(data=overview2021, aes(x="", y=average_duration, fill=member_casual)) +
  geom_bar(stat="identity",width=1) +
  coord_polar("y", start=0)
