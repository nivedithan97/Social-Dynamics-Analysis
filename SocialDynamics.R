setwd("C:/local/Niveditha Nagasubramanian/DATA ANALYTICS/ASSIGNMENT 1")
webforum=read.csv("webforum.csv")

#Research 1
library(lattice)
install.packages("ggplot2")
library(ggplot2)

#remove rows where AuthorID = -1 because we don't want to include posts with unknown authors in our analysis
webforum[webforum==-1] = NA
webforum=na.omit(webforum)

#remove rows where WC=0 because we don't want to include posts with 0 word count
webforum= webforum[which(webforum$WC!= 0),] 

#creating a new column called "Year"
install.packages("lubridate")
library(lubridate)
webforum$Year=year(webforum$Date)
attach(webforum)

swapped_webforum=webforum[,c(2,1,3:33)] #swap the columns PostID and ThreadID
attach(swapped_webforum)

newwebforum=swapped_webforum[order(ThreadID),] #sort the rows in ascending ThreadID
attach(newwebforum)

install.packages('plyr')
library(plyr)

freq_of_posts=as.data.frame(count(newwebforum,c('Year','ThreadID'))) #frequency of posts in each ThreadID and Year
#find the ThreadID which has the maximum number of posts for each year
inter_table=by(freq_of_posts,freq_of_posts[1],function(df) df[which.max(df[,3]),])
freqpostsyr=do.call(rbind,inter_table)

#merge newwebforum dataframe and freqpostsyr dataframe by ThreadID and Year
activethread=merge(newwebforum,freqpostsyr,by = c('ThreadID','Year'))
attach(activethread)
#remove the 'freq' column in activethread dataframe
activethread=activethread[,1:33] #remove freq column


#find the frequency of each AuthorID in each ThreadID
author_in_each_thread=as.data.frame(count(activethread,c('Year','ThreadID','AuthorID')))
authorfreq=by(activethread,activethread$AuthorID,function(df) count(df))
freq_of_authors=do.call(rbind,authorfreq)
attach(freq_of_authors)

#plotting the frequency of each AuthorID in each ThreadID to find the most active threads
ggplot(author_in_each_thread,aes(x=AuthorID,y=freq)) + geom_point() + facet_wrap(Year~ThreadID,scales="free") + labs(title="Plot for frequency of posts for each author",y="No.of Posts")
library(reshape2)
attach(activethread)
median_active_threads = aggregate(activethread[c("Analytic","Clout","Authentic","Tone")], by = list(activethread$Year), median)
colnames(median_active_threads)[1] = 'Year'
reshaped_active_thread = melt(median_active_threads, id = c("Year"))
colnames(reshaped_active_thread)[2] = "Language"
ggplot(reshaped_active_thread, aes(x=reshaped_active_thread$Year, y = reshaped_active_thread$value))+ geom_line(aes(color = Language)) + geom_point(aes(color = reshaped_active_thread$Language)) + labs(title = "Language change of 10 Active Threads(In median)", x = "Year", y = "Proportion of Language %")


#####
library(reshape2)
attach(newwebforum)
thread_145223_task2 = newwebforum[which(ThreadID == 145223),c(1:2, 4:5, 7:10)]
thread_145223_task2$DateTime = with(thread_145223_task2, as.POSIXct(paste(thread_145223_task2$Date, thread_145223_task2$Time), format = "%Y-%m-%d %H:%M"))
thread_145223_task2 = melt(thread_145223_task2, id = c("ThreadID","PostID","Date","Time","DateTime"))
colnames(thread_145223_task2)[6] = "Language"
ggplot(thread_145223_task2, aes(x =thread_145223_task2$DateTime, y = thread_145223_task2$value))+  geom_line(aes(color = 'red')) + geom_point() + facet_wrap(~thread_145223_task2$Language, scales = "free")+ geom_smooth(method = 'lm') + labs(title = "Language change of ThreadID: 145223", x = "DateTime", y = "Value") + theme(legend.position = "none")

thread_252620_task2 = newwebforum[which(ThreadID == 252620),c(1:2, 4:5, 7:10)]
thread_252620_task2$DateTime = with(thread_252620_task2, as.POSIXct(paste(thread_252620_task2$Date, thread_252620_task2$Time), format = "%Y-%m-%d %H:%M"))
thread_252620_task2 = melt(thread_252620_task2, id = c("ThreadID","PostID","Date","Time","DateTime"))
colnames(thread_252620_task2)[6] = "Language"
ggplot(thread_252620_task2, aes(x =thread_252620_task2$DateTime, y = thread_252620_task2$value))+  geom_line(aes(color = 'red')) + geom_point() + facet_wrap(~thread_252620_task2$Language, scales = "free")+ geom_smooth(method = 'lm') +  labs(title = "Language change of ThreadID: 252620", x = "DateTime", y = "Value") + theme(legend.position = "none")

thread_283958_task2 = newwebforum[which(ThreadID == 283958),c(1:2, 4:5, 7:10)]
thread_283958_task2$DateTime = with(thread_283958_task2, as.POSIXct(paste(thread_283958_task2$Date, thread_283958_task2$Time), format = "%Y-%m-%d %H:%M"))
thread_283958_task2 = melt(thread_283958_task2, id = c("ThreadID","PostID","Date","Time","DateTime"))
colnames(thread_283958_task2)[6] = "Language"
ggplot(thread_283958_task2, aes(x =thread_283958_task2$DateTime, y = thread_283958_task2$value))+  geom_line(aes(color = 'red')) + geom_point() + facet_wrap(~thread_283958_task2$Language, scales = "free")+ geom_smooth(method = 'lm') + labs(title = "Language change of ThreadID: 283958", x = "DateTime", y = "Value") + theme(legend.position = "none")

thread_385942_task2 = newwebforum[which(ThreadID == 385942),c(1:2, 4:5, 7:10)]
thread_385942_task2$DateTime = with(thread_385942_task2, as.POSIXct(paste(thread_385942_task2$Date, thread_385942_task2$Time), format = "%Y-%m-%d %H:%M"))
thread_385942_task2 = melt(thread_385942_task2, id = c("ThreadID","PostID","Date","Time","DateTime"))
colnames(thread_385942_task2)[6] = "Language"
ggplot(thread_385942_task2, aes(x =thread_385942_task2$DateTime, y = thread_385942_task2$value))+ geom_line(aes(color = 'red')) + geom_point() + facet_wrap(~thread_385942_task2$Language, scales = "free")+ geom_smooth(method = 'lm') + labs(title = "Language change of ThreadID: 385942", x = "DateTime", y = "Value") + theme(legend.position = "none")

thread_127115_task2 = newwebforum[which(ThreadID == 127115),c(1:2, 4:5, 7:10)]
thread_127115_task2$DateTime = with(thread_127115_task2, as.POSIXct(paste(thread_127115_task2$Date, thread_127115_task2$Time), format = "%Y-%m-%d %H:%M"))
thread_127115_task2 = melt(thread_127115_task2, id = c("ThreadID","PostID","Date","Time","DateTime"))
colnames(thread_127115_task2)[6] = "Language"
ggplot(thread_127115_task2, aes(x =thread_127115_task2$DateTime, y = thread_127115_task2$value))+ geom_line(aes(color = 'red')) + geom_point() + facet_wrap(~thread_127115_task2$Language, scales = "free")+ geom_smooth(method = 'lm') + labs(title = "Language change of ThreadID: 127115", x = "DateTime", y = "Value") + theme(legend.position = "none")


detach(newWebforum)




########################








attach(freq_of_authors)

Thread_127115= freq_of_authors[which(freq_of_authors$ThreadID== 127115),] 
Thread_145223= freq_of_authors[which(freq_of_authors$ThreadID== 145223),] 
Thread_252620= freq_of_authors[which(freq_of_authors$ThreadID== 252620),] 
Thread_283958= freq_of_authors[which(freq_of_authors$ThreadID== 283958),] 
Thread_385942= freq_of_authors[which(freq_of_authors$ThreadID== 385942),] 
Thread_472752_2008= freq_of_authors[which(freq_of_authors$ThreadID== 472752 & freq_of_authors$Year==2008 ),] 
Thread_472752_2009= freq_of_authors[which(freq_of_authors$ThreadID== 472752 & freq_of_authors$Year==2009 ),] 
Thread_10133= freq_of_authors[which(freq_of_authors$ThreadID==10133),]
Thread_62619= freq_of_authors[which(freq_of_authors$ThreadID==62619),]
Thread_773564= freq_of_authors[which(freq_of_authors$ThreadID==773564),]


#mean of all active threads - in total there are 10 active threads and 7 interactive threads
summary_freq_of_authors= freq_of_authors[,c(1:2,8:11)]
attach(summary_freq_of_authors)
mean_Threads=aggregate(summary_freq_of_authors[c('ThreadID','Analytic','Clout','Authentic','Tone')],by=list(Year),mean)
colnames(mean_Threads)[1]="Year"

#mean of non active threads (which is basically threads that are not active)
#RECALL: active threads --> max no of posts for each year (10 active threads)
#Interactive threads --> freq of author participation in each thread in each year (7 interactive threads)
attach(newwebforum)
posts_from_2002 = newwebforum[which(Year == 2002 & ThreadID != 10133),c(1:2,7:10,33)]
posts_from_2003 = newwebforum[which(Year == 2003 & ThreadID != 62619),c(1:2,7:10,33)]
posts_from_2004 = newwebforum[which(Year == 2004 & ThreadID != 145223),c(1:2,7:10,33)]
posts_from_2005 = newwebforum[which(Year == 2005 & ThreadID != 252620),c(1:2,7:10,33)]
posts_from_2006 = newwebforum[which(Year == 2006 & ThreadID != 283958),c(1:2,7:10,33)]
posts_from_2007 = newwebforum[which(Year == 2007 & ThreadID != 385942),c(1:2,7:10,33)]
posts_from_2008 = newwebforum[which(Year == 2008 & ThreadID != 472752),c(1:2,7:10,33)]
posts_from_2009 = newwebforum[which(Year == 2009 & ThreadID != 472752),c(1:2,7:10,33)]
posts_from_2010 = newwebforum[which(Year == 2010 & ThreadID != 127115),c(1:2,7:10,33)]
posts_from_2011 = newwebforum[which(Year == 2011 & ThreadID != 773564),c(1:2,7:10,33)]

posts_2002to2011 = rbind(posts_from_2002,posts_from_2003,posts_from_2004,posts_from_2005,posts_from_2006,posts_from_2007,posts_from_2008,posts_from_2009,posts_from_2010,posts_from_2011)
attach(posts_2002to2011)

mean_posts2002to2011 = aggregate(posts_2002to2011,by=list(Year),mean)
colnames(mean_posts2002to2011)[1]= 'Year'

#Hypothesis testing
# H0: mean of active threads in year x == mean of all the threads year x excluding the active thread
# H1: mean of active threads in year x != mean of all the threads year x excluding the active thread

##YEAR 2002
#using automation (for loop) to find out p-values for Analytic,Clout,Authentic,Tone
for(i in 8:11)
{
  a=t.test(Thread_10133[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2002],"two.sided",conf.level = 0.95)
  print(a)
}
#(ANALYTIC)since p-value=0.03266 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to mean of inactive threads in the year 2002
#(CLOUT)since p-value=0.05352 > 0.05, we donot reject the null hypothesis. So the mean of active threads is equal to mean of inactive threads in the year 2002
#(AUTHENTIC)since p-value=0.59 > 0.05, we donot reject the null hypothesis. So the mean of active threads is equal to mean of inactive threads in the year 2002
#(TONE)since p-value=0.04578 <= 0.05, we reject the null hypothesis that the mean of active threads is not equal to mean of inactive threads in the year 2002



###DO BOXPLOT FOR 2002 --> use median to check if language used changes over time

subset_2002=  newwebforum[which(newwebforum$Year == 2002),c(1:2,7:10,33)]
subset_2002 = subset_2002[,c(1:3,5,4,6:7)]

subset02= melt(subset_2002,id=c('ThreadID','PostID','Year'))
colnames(subset02)[4] = 'Language'


attach(subset02)
ggplot(subset02,aes(subset02$ThreadID=='10133',y=subset02$value)) + geom_boxplot(aes(fill=Language)) + scale_x_discrete(labels=c("Other Non active threads",'Thread 10133')) + xlab("Active Thread vs Non Active Threads") + ylab("Proportion of language(%)") + ggtitle("Thread 10133 vs Non Active Threads in Year 2002")



##YEAR 2003

for(i in 8:11)
{
  a=t.test(Thread_62619[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2003],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=2.2e-16 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to mean of inactive threads in the year 2003
#(CLOUT)since p-value=2.384e-12 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to mean of inactive threads in the year 2003
#(AUTHENTIC)since p-value=2.833e-07 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to mean of inactive threads in the year 2003
#(TONE)since p-value=3.455e-07 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to mean of inactive threads in the year 2003



##YEAR 2004

for(i in 8:11)
{
  a=t.test(Thread_145223[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2004],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=0.03991 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to mean of inactive threads in the year 2004
#(CLOUT)since p-value=0.1265 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2004
#(AUTHENTIC)since p-value=0.0001678 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to mean of inactive threads in the year 2004
#(TONE)since p-value=0.01011 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2004



##YEAR 2005

for(i in 8:11)
{
  a=t.test(Thread_252620[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2005],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=1.313e-05 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2005
#(CLOUT)since p-value=0.0006886 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads
#(AUTHENTIC)since p-value=0.7229 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2005
#(TONE)since p-value=0.0005101 <= 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads




##YEAR 2006

for(i in 8:11)
{
  a=t.test(Thread_283958[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2006],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=0.1416 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2006
#(CLOUT)since p-value=0.06002 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2006 
#(AUTHENTIC)since p-value=1.192e-06 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2006
#(TONE)since p-value=8.745e-05 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2006



###DO BOXPLOT FOR 2006 --> use median to check if language used changes over time

subset_2006=  newwebforum[which(newwebforum$Year == 2006),c(1:2,7:10,33)]
subset_2006 = subset_2006[,c(1:3,5,4,6:7)]

subset06= melt(subset_2006,id=c('ThreadID','PostID','Year'))
colnames(subset06)[4] = 'Language'



ggplot(subset06,aes(subset06$ThreadID=='283958',y=subset06$value)) + geom_boxplot(aes(fill=Language)) + scale_x_discrete(labels=c("Other Non active threads",'Thread 283958'))+ xlab("Active Thread vs Non Active Threads") + ylab("Proportion of language(%)") + ggtitle("Thread283958 vs NonActive Threads in Year 2006")




##YEAR 2007

for(i in 8:11)
{
  a=t.test(Thread_385942[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2007],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=0.09837 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2007
#(CLOUT)since p-value=0.8086 > 0.05,  we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2007
#(AUTHENTIC)since p-value=0.0001594 < 0.05,  we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2007
#(TONE)since p-value=0.001024 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2007


###DO BOXPLOT FOR 2007 --> use median to check if language used changes over time

subset_2007=  newwebforum[which(newwebforum$Year == 2007),c(1:2,7:10,33)]
subset_2007 = subset_2007[,c(1:3,5,4,6:7)]

subset07= melt(subset_2007,id=c('ThreadID','PostID','Year'))
colnames(subset07)[4] = 'Language'


ggplot(subset07,aes(subset07$ThreadID=='385942',y=subset07$value)) + geom_boxplot(aes(fill=Language)) + scale_x_discrete(labels=c("Other Non active threads",'Thread 385942')) +  xlab("Active Thread vs Non Active Threads") + ylab("Proportion of language(%)") + ggtitle("Thread385942 vs NonActive Threads in Year 2007")




##YEAR 2008

for(i in 8:11)
{
  a=t.test(Thread_472752_2008[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2008],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=0.002495 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2008
#(CLOUT)since p-value=0.4623 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2008
#(AUTHENTIC)since p-value=1.023e-05 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2008
#(TONE)since p-value=5.983e-08 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2008




##YEAR 2009

for(i in 8:11)
{
  a=t.test(Thread_472752_2009[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2009],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=0.02292 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2009
#(CLOUT)since p-value=0.1912 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2009
#(AUTHENTIC)since p-value=5.686e-05 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2009
#(TONE)since p-value=1.837e-12 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2009




##YEAR 2010

for(i in 8:11)
{
  a=t.test(Thread_127115[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2010],"two.sided",conf.level = 0.95)
  print(a)
}
#(ANALYTIC)since p-value=1.731e-11 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2010
#(CLOUT)since p-value=0.397 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2010
#(AUTHENTIC)since p-value=0.1295 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2010
#(TONE)since p-value=0.1825 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2010



##YEAR 2011

for(i in 8:11)
{
  a=t.test(Thread_773564[,i],posts_2002to2011[,i-5][posts_2002to2011$Year==2011],"two.sided",conf.level = 0.95)
  print(a)
}

#(ANALYTIC)since p-value=0.001587 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2011
#(CLOUT)since p-value=0.3773 > 0.05, we donot reject the null hypothesis so the mean of active threads is equal to the mean of inactive threads in the year 2011
#(AUTHENTIC)since p-value=2.449e-06 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2011
#(TONE)since p-value=6.017e-15 < 0.05, we reject the null hypothesis so the mean of active threads is not equal to the mean of inactive threads in the year 2011







