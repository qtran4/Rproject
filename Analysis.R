#Graphing------------------------------
#Reading in data
allData<-read.csv(file = "allData.csv", header = TRUE)
library(ggplot2)
ggplot()

#The data had strangely high ages that were likely errors; this corrects them
removed_strange_ages<-allData[allData$age <110,]

#finding entries with a marker present; removes any entries that weren't infected
sick<-subset(removed_strange_ages, marker01!=0 | marker02!=0 |marker03!=0 |marker04!=0 |marker05!=0 |marker06!=0 |marker07!=0 |marker08!=0 | marker09!=0 |marker10!=0)

#ggplotting cases as a function of time
#sub-setting and adjusting data
Xdata<- sick[sick$country=="X",]
Ydata<-sick[sick$country=="Y",]

Yno<-seq_along(Ydata$dayofYear)
Xno<-seq_along(Xdata$country)
sick$Casenum<-append(Xno,Yno, after = length(Xno))

#Plot Cumulative cases in each country against time
ggplot(data = sick,
       aes(x=sick$dayofYear, y=sick$Casenum, colour = country))+
  geom_smooth()+
  xlab("Day of the Year") +
  ylab("Running Case Total")+
  theme_classic()

#Plot new cases against time
ggplot(data = sick,
       aes(x=dayofYear))+
  geom_bar(position="dodge", aes( fill = country), show.legend = FALSE)+
  xlab("Day of the Year") +
  ylab("New Cases Per Day")+
  facet_grid(.~country)+
  theme_classic()

#Marker comparison by country
#subset data
XmarkerData<-data.frame( MarkerNum = colnames(Xdata)[3:12],
                         MarkerSum = colSums(Xdata[,3:12]),
                         Country = Xdata[1:10,13])
rownames(XmarkerData)<-1:nrow(XmarkerData)
YmarkerData<-data.frame( MarkerNum = colnames(Ydata)[3:12],
                         MarkerSum = colSums(Ydata[,3:12]),
                         Country = Ydata[1:10,13])
rownames(YmarkerData)<-1:nrow(YmarkerData)
MarkerforPlot<- rbind(XmarkerData,YmarkerData)
#plot
ggplot(data = MarkerforPlot,
       aes(x=Country, y=MarkerSum, fill=MarkerNum))+
  geom_bar(stat="identity", position="dodge")+
  xlab("Country") +
  ylab("Marker Abundance")+
  theme_minimal()












