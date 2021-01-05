library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(rworldmap)
library(repr)
library(dplyr)
library(plyr)


covidCases=read.csv('Total_CovidCase_Per_Country.csv')

head(covidCases)

covidCases = as.data.frame(covidCases)

head(covidCases)
View(covidCases)
colnames(covidCases) <- c('Country', 'Total_Cases_Count')



#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

#options(repr.plot.width=6, repr.plot.height=6)
#world <- map_data(map="world")
#world <- world[world$region != "Antarctica",] # 
#y=ddply(covidCases, .(Country,Total_Cases_Count))

#sPDF <- joinCountryData2Map( y,joinCode = "ISO3"
                             #,nameJoinColumn = "Country")

#mapCountryData(sPDF, nameColumnToPlot='Total_Cases_Count')




library(rworldmap)

#create a map-shaped window
mapDevice('x11')

#join to a coarse resolution map
spdf <- joinCountryData2Map(covidCases, joinCode="NAME", nameJoinColumn="Country")

mapCountryData(spdf, nameColumnToPlot="Total_Cases_Count", catMethod="fixedWidth")


covidWeekCases=read.csv('week_data_countries.csv')
covidWeekCases = as.data.frame(covidWeekCases)
colnames(covidWeekCases) <- c('Country', 'Week', 'ConcateData', 'TotalCasesWeek')
head(covidWeekCases)
covidCasesFilter=head(covidCases,n=5)
topCountryFilter=covidWeekCases[covidWeekCases$Country %in% covidCasesFilter$Country,] 


options(repr.plot.width=15, repr.plot.height=30)
x=ddply(topCountryFilter, .(Country,Week), numcolwise(sum))
sp <- ggplot(x,aes(Week,TotalCasesWeek,color=Country,group=Country))+geom_point()+geom_line()
sp + expand_limits(x=c(4,17))
##############################

allCovidWeekCases=read.csv('top_10_country.csv')
allCovidWeekCases = as.data.frame(allCovidWeekCases)
colnames(allCovidWeekCases) <- c('Country', 'Week', 'Combine', 'TotalCasesWeek')
head(allCovidWeekCases)

topCovidWeekCases = head(allCovidWeekCases, 171)
#options(repr.plot.width=6, repr.plot.height=6)
y=ddply(topCovidWeekCases, .(Country,Week), numcolwise(sum))
ggplot(y,aes(x=reorder(Country,Week),y=TotalCasesWeek,fill=Week,group=Week))+geom_bar(stat='identity') +coord_flip()

barplot(y[,3])

y1 = as.table(y)

cloud(y1, d, panel.3d.cloud=panel.3dbars, col.facet='grey', 
      xbase=0.4, ybase=0.4, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")))

# Stacked Bar Plot with Colors and Legend
counts <- table(allCovidWeekCases$TotalCasesWeek, allCovidWeekCases$Week)
barplot(allCovidWeekCases$TotalCasesWeek, main="Covid Cases Week Wise Top Countries",
        xlab="Number of Gears", col=c(rainbow(10)),
        names.arg = c("United States", "Germany", "Spain", "Turkey", "RUssia", "Italy", "China", "United Kingdom", "Iran", "France"),
        beside=TRUE, cex.names = names.arg)


ggplot(y,aes(x=reorder(Country,Week),y=TotalCasesWeek,fill=Week,group=Week, color=Country))+geom_bar(stat='identity') +coord_flip() + scale_fill_manual(values = rainbow(10))
#####################################


world_data =read.csv('world_data.csv')



LinearModel = lm(Week~Total_New_Cases, data = world_data)
summary(LinearModel)
confint(LinearModel)

plot(Total_New_Cases~Week, data = world_data, xlab = "Week No", ylab = "New Cases Registered", col = "red", pch = 20, type = 'b')
abline(a= 6.000e+00, b=1.858e-05, col = "green", lwd = 2)
lines(smooth.spline(world_data$Total_New_Cases, predict(LinearModel)), col= "blue",lwd=2, lty=2)
legend(100, 40, legend=c("Linear", "Polynomial"),
       col=c("green", "blue"), lty=1:2, cex=0.8)

plot(Total_New_Cases~Week, data = world_data)

predict(LinearModel,list(Week = c(20, 25, 30, 35)))

class(world_data$Total_New_Cases)

