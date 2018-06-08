library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(sqldf)
library(ggthemes)
library(plotrix)


#Flight count in pie3d:

Flight_count
fc <- round(Flight_count$n/sum(Flight_count$n)*100)
lbls <- c("Mon","Tue","Wed","Thr","Fri","Sat","Sun")
lbls <- paste(lbls,fc)
lbls <- paste(lbls,"%")
pie3D(Flight_count$n,labels = lbls,labelcex = 1,explode = 0.1,main = "Pie chart for number of flights")



#number of flights in time wise:

time_col = sqldf('select DepTime from read')
midNight = sqldf('select count(DepTime) from time_col where DepTime<400')
earlyMrng = sqldf('select count(DepTime) from time_col where DepTime >=400 and DepTime<800')
morning = sqldf('select count(DepTime) from time_col where DepTime >=800 and DepTime<1200')
noon = sqldf('select count(DepTime) from time_col where DepTime >=1200 and DepTime<1600')
evening = sqldf('select count(DepTime) from time_col where DepTime >=1600 and DepTime<2000')
nig = sqldf('select count(DepTime) from time_col where DepTime >=2000 and DepTime<2400')
time1 <- data.frame("00-4","4-8","8-12","12-16","16-20","20-24")
week_time <- data.frame(midNight,earlyMrng,morning,noon,evening,nig)
wk_time <- t(week_time)
time2 <- t(time1)
row.names(wk_time) <- 1:nrow(wk_time)
row.names(time2) <- 1:nrow(time2)
time2 <- as.character(time2)
time2 <- factor(time2,levels = c("00-4","4-8","8-12","12-16","16-20","20-24"))
final_col1 <- data.frame(time2,wk_time)
graph1 <- ggplot(data = final_col1, aes(x = final_col$time2, y=final_col1$wk_time))+
  theme_economist()
graph1 + geom_bar(fill ="blue",stat="identity",width = 0.8,aes())+ 
  ggtitle("Number of flights based on time catagory")+xlab("Timings")+ylab("Flight count")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(face = "bold"))+
  (theme(axis.text.x = element_text(face = "bold")))+
  (theme(axis.text.y = element_text(face = "bold")))

#types of delay
a <- mean(Carrier_Delay)
b <- mean(NAS_Delay)
c <- mean(Weather_Delay)
d <- mean(LateAircraft_Delay)
e <- mean(Security_Delay)

typesOfDelay <- c(a,b,c,d,e)
names(typesOfDelay) <- c("Carrier","NAS","Weather","Late aircraft","Security")
lab <- c("Carrier","NAS","Weather","Late Air craft","Security")
pct <- round(typesOfDelay/sum(typesOfDelay)*100)
lab <- paste(lab,pct)
lab <- paste(lab,"%",sep="")
pie3D(typesOfDelay,labelcex=1,labels = lab,explode = 0.1,main="Types of Delays")




#Depature delay with the delay

delay_col <- filter(read,ArrDelay > 0)
tot <- tapply(delay_col$ArrDelay,delay_col$DayOfWeek,mean,na.rm = T)
Carrier_Delay <- tapply(delay_col$CarrierDelay,delay_col$DayOfWeek,mean,na.rm = T)
NAS_Delay <- tapply(delay_col$NASDelay,delay_col$DayOfWeek,mean,na.rm=T)
Security_Delay <- tapply(delay_col$SecurityDelay,delay_col$DayOfWeek,mean,na.rm=T)
LateAircraft_Delay <- tapply(delay_col$LateAircraftDelay,delay_col$DayOfWeek,mean,na.rm=T)
Weather_Delay <- tapply(delay_col$WeatherDelay,delay_col$DayOfWeek,mean,na.rm=T)
read1 <- read %>% group_by(read$DayOfWeek) %>% tally
x <- data.frame(read1$`read$DayOfWeek`,Carrier_Delay,Weather_Delay,NAS_Delay,Security_Delay,LateAircraft_Delay)
values = c(Carrier_Delay,Weather_Delay,NAS_Delay,Security_Delay,LateAircraft_Delay)
DelayTypes = c(rep("Carrier_Delay",7),rep("Weather_Delay",7),rep("NAS_Delay",7),rep("Security_Delay",7),rep("LateAircraft_Delay",7))
x1 = data.frame(read1$`read$DayOfWeek`,values)
graph2 = ggplot(x1, aes(x = x1$read1..read.DayOfWeek., y=values))
graph2 + geom_bar(stat = "identity", width = 0.8,aes(fill=DelayTypes))+
  ggtitle("Delay in flights weekly analysis")+xlab("Day")+ylab("Delay")+
  theme_economist()+scale_x_continuous(breaks=seq(1,7,1))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.title = element_text(face = "bold"))+
  (theme(axis.text.x = element_text(face = "bold")))+
  (theme(axis.text.y = element_text(face = "bold")))+ 
  scale_x_discrete(limits = c("Mon","Tue","Wed","Thr","Fri","Sat","Sun"))




#Depature delay with weather delay:

values1 = c(Carrier_Delay,NAS_Delay,Security_Delay,LateAircraft_Delay)
type2 = c(rep("Carrier_Delay",7),rep("NAS_Delay",7),rep("Security_Delay",7),rep("LateAircraft_Delay",7))
x2 = data.frame(read1$`read$DayOfWeek`,values1)
graph3 = ggplot(x2, aes(x = x2$read1..read.DayOfWeek., y=values1))
graph3 + geom_bar(position = "dodge",stat = "identity",aes(fill = type2))+
  ggtitle("Delay in flights weekly analysis")+xlab("Day")+ylab("Delay")+
  theme_economist()+scale_x_continuous(breaks=seq(1,7,1))+
  theme(plot.title=element_text(hjust=0.5))






#Delay calculated in time wise:

new_col = select(read,DepTime,DepDelay)
mid_night = sqldf('select avg(DepDelay) from new_col where DepTime<400')
early_mrng = sqldf('select avg(DepDelay) from new_col where DepTime >=400 and DepTime<800')
mrng = sqldf('select avg(DepDelay) from new_col where DepTime >=800 and DepTime<1200')
afternoon = sqldf('select avg(DepDelay) from new_col where DepTime >=1200 and DepTime<1600')
eve = sqldf('select avg(DepDelay) from new_col where DepTime >=1600 and DepTime<2000')
night = sqldf('select avg(DepDelay) from new_col where DepTime >=2000 and DepTime<2400')
time1 <- data.frame("00-4","4-8","8-12","12-16","16-20","20-24")
time_df <- data.frame(mid_night,early_mrng,mrng,afternoon,eve,night)
mean_delay <- t(time_df)
time2 <- t(time1)
row.names(mean_delay) <- 1:nrow(mean_delay)
row.names(time2) <- 1:nrow(time2)
time2 <- as.character(time2)
time2 <- factor(time2,levels = c("00-4","4-8","8-12","12-16","16-20","20-24"))
final_col <- data.frame(time2,mean_delay)
graph4 <- ggplot(data = final_col, aes(x = final_col$time2, y=final_col$mean_delay))+
  theme_economist()
graph4 + geom_bar(fill = "royalblue4",stat="identity",width = 0.8,aes())+
  ggtitle("Time of the day with average delay")+xlab("Timings")+ylab("mean delay")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(face = "bold"))+
  (theme(axis.text.x = element_text(face = "bold")))+
  (theme(axis.text.y = element_text(face = "bold")))








#top 10 airports with more arrival delay:

col1 <- read %>% group_by(Origin) %>% mean(DepDelay)
col1 <- select(read,Origin,DepDelay)
Ori_delay <- sqldf('select Origin,avg(DepDelay) as AvgDelay from col1 group by Origin order by AvgDelay desc')
plot_value <- head(Ori_delay,10)
graph_origin <- ggplot(plot_value,aes(x = reorder(Origin,-AvgDelay),y=AvgDelay))+theme_economist()
graph_origin + geom_bar(fill = "royalblue4",stat="identity",width = 0.8,aes())+
  ggtitle("Top 10 Airports with more number of Departure delay")+
  xlab("Airports")+ylab("Avg. Delay")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(face = "bold"))+
  (theme(axis.text.x = element_text(face = "bold")))+
  (theme(axis.text.y = element_text(face = "bold")))






#top 10 airports with more depature delay:

col2 <- select(read,Dest,ArrDelay)
Des_delay <- sqldf('select Dest,avg(ArrDelay) as AvgDelay1 from col2 group by Dest order by AvgDelay1 desc')
plot_value1 <- head(Des_delay,10)
plot_value1
graph_dest <- ggplot(plot_value1,aes(x = reorder(Dest,-AvgDelay1),AvgDelay1))+
  theme_economist()
graph_dest+ geom_bar(fill = "royalblue4",stat="identity",width = 0.8,aes())+
  ggtitle("Top 10 Airports with more number of Arrival delay")+xlab("Airports")+
  ylab("Avg. Delay")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(face = "bold"))+
  (theme(axis.text.x = element_text(face = "bold")))+
  (theme(axis.text.y = element_text(face = "bold")))             




#Top 10 combination of airports with minimum arrival delay:

read1 <- (read)
final6 <- select(read,Origin,Dest,ArrDelay)
combination_col <- sqldf('select Origin,Dest,avg(ArrDelay) as mean_delay from final6 group by Origin,Dest order by mean_delay asc')
col8 <- filter(combination_col,mean_delay > -27)
head(col8,10)
draw1 <- within(col8,cv <-paste(Origin,'-',Dest))
final9 <- head(draw1,10)
min_com <- ggplot(final9,aes(x = reorder(cv,mean_delay),mean_delay))+theme_economist()+coord_flip()
min_com+ geom_bar(fill = "royalblue4",position = "dodge",stat="identity",width = 0.8,aes())+ ggtitle("Top 10 Combination of Airports with minimum Arrival delay")+xlab("Airports")+ylab("Avg. Delay")+theme(plot.title = element_text(hjust = 0.5))+theme(axis.title = element_text(face = "bold"))+(theme(axis.text.x = element_text(face = "bold")))+(theme(axis.text.y = element_text(face = "bold")))             



#Top 10 combination of airports with maximum arrival delay:

final8 <- tail(draw1,10)
max_com <- ggplot(final8,aes(x = reorder(cv,mean_delay),mean_delay))+
  theme_economist()+coord_flip()
max_com+ geom_bar(fill = "royalblue4" ,position = "dodge",stat="identity",width = 0.8,aes())+ 
  ggtitle("Top 10 Combination of Airports with maximum Arrival delay")+
  xlab("Airports")+ylab("Avg. Delay")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title = element_text(face = "bold"))+
  (theme(axis.text.x = element_text(face = "bold")))+
  (theme(axis.text.y = element_text(face = "bold")))             

