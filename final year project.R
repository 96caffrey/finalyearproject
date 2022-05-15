library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra) 
library(GGally) 
library(plotly) 
library(tidyverse)
library(rstatix)
library(ggpubr)
library(tseries)
library(forecast)
library(plotrix)
library(animation)
library(mclust)
library(cluster)
library(factoextra)
library(writexl)
library(ggthemes)
library(data.table)
library(ggfortify)

setwd("/Users/bcaff/Desktop/Year 4/Software Project/datasets/")

offences <- read.csv("offences.csv", header=T, na.strings=c(""), stringsAsFactors = T)
gardaEmployment  <- read.csv("Employment of Garda per region 2008 - 2020.csv", header=T, na.strings=c(""), stringsAsFactors = T)
gardaEmployment <- subset(gardaEmployment, select = -c(UNIT))


#Convert Garda Station to Location
names(offences)
names(offences)[1] <- "Statistic"
names(offences)[2] <- "Year"
names(offences)[3] <- "Location"
names(offences)[4] <- "Type.of.Offence"
names(offences)[5] <- "VALUE"
str(offences)
summary(offences)



#OFFENCES.CSV REFORMATTED IN EXCEL TO CREATE OFFENCESPERGARDA
#write.csv(offences, "/Users/bcaff/Desktop/Year 4/Software Project/datasets/offencesPerGarda.csv", row.names = FALSE)


#offences data with Garda Numbers added
offencesPerGardai <- read.csv("/Users/bcaff/Desktop/Year 4/Software Project/datasets/offencesPerGarda.csv", header=T, na.strings=c(""), stringsAsFactors = T)
names(offencesPerGardai)
names(offencesPerGardai)[1] <- "Statistic"
names(offencesPerGardai)[2] <- "Year"
names(offencesPerGardai)[3] <- "Location"
names(offencesPerGardai)[4] <- "Offence Type"
names(offencesPerGardai)[5] <- "No of Gardai"
names(offencesPerGardai)[6] <- "Offence Occurrence"


#EXPLORATORY ANALYSS
#1275891
totaloffences_committed<- sum(offencesPerGardai$`Offence Occurrence`[offencesPerGardai$Statistic == "Recorded Crime Offences Under Reservation"])

totalgarda_employed <-  sum(offencesPerGardai$`No of Gardai`)/12



fingal <- c("63101 Balbriggan, D.M.R. Northern Division",
             "63102 Garristown, D.M.R. Northern Division",
             "63103 Lusk, D.M.R. Northern Division",
             "63105 Skerries, D.M.R. Northern Division",
             "63202 Dublin Airport, D.M.R. Northern Division",
             "63302 Malahide, D.M.R. Northern Division",
             "63303 Swords, D.M.R. Northern Division",
             "63402 Howth, D.M.R. Northern Division",
             "66101 Blanchardstown, D.M.R. Western Division")
             
             dublinCity <- c("61302 Kilmainham, D.M.R. South Central Division",
                             "61301 Kevin Street, D.M.R. South Central Division",
                             "61202 Pearse Street, D.M.R. South Central Division",
                             "62203 Mountjoy, D.M.R. North Central Division",
                             "64302 Terenure, D.M.R. Southern Division",
                             "66102 Cabra, D.M.R. Western Division",
                             "66103 Finglas, D.M.R. Western Division",
                             "63203 Santry, D.M.R. Northern Division",
                             "63403 Raheny, D.M.R. Northern Division",
                             "66201 Ballyfermot, D.M.R. Western Division",
                             "62101 Bridewell Dublin,D.M.R. North Central Division",
                             "62202 Fitzgibbon Street,D.M.R. North Central Division",
                             "62301 Store Street, D.M.R. North Central Division",
                             "61102 Irishtown, D.M.R. South Central Division",
                             "63401 Clontarf, D.M.R. Northern Division",
                             '63201 Ballymun, D.M.R. Northern Division',
                             '63301 Coolock, D.M.R. Northern Division')
                             

southDublin <- c("64202 Tallaght, D.M.R. Southern Division",
             "64101 Crumlin, D.M.R. Southern Division",
             "61101 Donnybrook, D.M.R. South Central Division",
             "66301 Lucan, D.M.R. Western Division",
             "66302 Ronanstown, D.M.R. Western Division",
             "66203 Rathcoole, D.M.R. Western Division",
             "66202 Clondalkin, D.M.R. Western Division")
            
          
dunRath <- c("65203 Dun Laoghaire, D.M.R. Eastern Division",
             "65201 Cabinteely, D.M.R. Eastern Division",
             "65102 Dundrum, D.M.R. Eastern Division",
             "65101 Blackrock, Co Dublin, D.M.R. Eastern Division",
             "64301 Rathmines, D.M.R. Southern Division",
             "64201 Rathfarnham, D.M.R. Southern Division",
             "64102 Sundrive Road, D.M.R. Southern Division",
             '65205 Shankill, D.M.R. Eastern Division')

#create data for each region of dublin
offences_fingal <- offencesPerGardai[offencesPerGardai$Location %in% fingal, ]
offences_southDublin <- offencesPerGardai[offencesPerGardai$Location %in% southDublin,]
offences_dublinCity <- offencesPerGardai[offencesPerGardai$Location %in% dublinCity, ]
offences_dunRath <- offencesPerGardai[offencesPerGardai$Location %in% dunRath,]

#exploratory analysis - number of offences and garda per region
#dividing by twelve as the number of guards are for that location
#fingal
sum(offences_fingal$`Offence Occurrence`[offences_fingal$Statistic == "Recorded Crime Offences Under Reservation"])
sum(offences_fingal$`No of Gardai`)/12

#dublin city
sum(offences_dublinCity$`Offence Occurrence`[offences_dublinCity$Statistic == "Recorded Crime Offences Under Reservation"])
sum(offences_dublinCity$`No of Gardai`)/12

#south dublin
sum(offences_southDublin$`Offence Occurrence`[offences_southDublin$Statistic == "Recorded Crime Offences Under Reservation"])
sum(offences_southDublin$`No of Gardai`)/12

#dun laoighre - rathdown
sum(offences_dunRath$`Offence Occurrence`[offences_dunRath$Statistic == "Recorded Crime Offences Under Reservation"])
sum(offences_dunRath$`No of Gardai`)/12

#total amount of offences per garda station for each region
#fingal
totalOffences_fingal <- aggregate(offences_fingal$`Offence Occurrence`, by=list(offences_fingal$Location), FUN=sum)
names(totalOffences_fingal)[1] <-  'Garda Station'
names(totalOffences_fingal)[2] <-  '#Offences'

#dublin city
totalOffences_dublinCity <- aggregate(offences_dublinCity$`Offence Occurrence`, by=list(offences_dublinCity$Location), FUN=sum)
names(totalOffences_dublinCity)[1] <-  'Garda Station'
names(totalOffences_dublinCity)[2] <-  '#Offences'

#south dublin
totalOffences_southDublin <- aggregate(offences_southDublin$`Offence Occurrence`, by=list(offences_southDublin$Location), FUN=sum)
names(totalOffences_southDublin)[1] <-  'Garda Station'
names(totalOffences_southDublin)[2] <-  '#Offences'

#dun laoighre - rathdown
totalOffences_dunRath <- aggregate(offences_dunRath$`Offence Occurrence`, by=list(offences_dunRath$Location), FUN=sum)
names(totalOffences_dunRath)[1] <-  'Garda Station'
names(totalOffences_dunRath)[2] <-  '#Offences'

#number of offences per year
totalOffences_fingal_yearly <- aggregate(offences_fingal$`Offence Occurrence`, by = list(offences_fingal$Year), FUN = sum)
totalOffences_southDublin_yearly <- aggregate(offences_southDublin$`Offence Occurrence`, by = list(offences_southDublin$Year), FUN = sum)
totalOffences_dublinCity_yearly <- aggregate(offences_dublinCity$`Offence Occurrence`, by = list(offences_dublinCity$Year), FUN = sum)
totalOffences_dunRath_yearly <- aggregate(offences_dunRath$`Offence Occurrence`, by = list(offences_dunRath$Year), FUN = sum)

#change names 
#fingal
names(totalOffences_fingal_yearly)
names(totalOffences_fingal_yearly)[1] <- "Year"
names(totalOffences_fingal_yearly)[2] <- "TotalOffences"

#dublin city
names(totalOffences_dublinCity_yearly)
names(totalOffences_dublinCity_yearly)[1] <- "Year"
names(totalOffences_dublinCity_yearly)[2] <- "TotalOffences"

#southdublin
names(totalOffences_southDublin_yearly)
names(totalOffences_southDublin_yearly)[1] <- "Year"
names(totalOffences_southDublin_yearly)[2] <- "TotalOffences"


#dun laoighre - rathdown
names(totalOffences_dunRath_yearly)
names(totalOffences_dunRath_yearly)[1] <- "Year"
names(totalOffences_dunRath_yearly)[2] <- "TotalOffences"

#Add Location to Differentiate Values
totalOffences_fingal_yearly$Location <- c("Fingal", "Fingal", "Fingal", "Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal")
totalOffences_dublinCity_yearly$Location <- c("Dublin City", "Dublin City", "Dublin City", "Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City")
totalOffences_southDublin_yearly$Location <- c("South Dublin", "South Dublin", "South Dublin", "South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin")
totalOffences_dunRath_yearly$Location <- c("Dun-Laoighre - Rathdown", "Dun-Laoighre - Rathdown", "Dun-Laoighre - Rathdown", "Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown")

#offences for all regions
totalOffences_all_regions <- rbind(totalOffences_fingal_yearly,totalOffences_dublinCity_yearly, totalOffences_southDublin_yearly, totalOffences_dunRath_yearly)

#plot showing offences for each region
ggplot(totalOffences_all_regions, aes(x = Year, y = TotalOffences, color = Location)) +
  geom_point()+
  xlim(2007.5 , 2020.5)+
  xlab("Time (Years)")+
  ylab("#Offences")+
  ggtitle('Number of Yearly Offences per Region ')


#predictions of offences per region
#fingal
fingal_offences <- ts(totalOffences_fingal_yearly$TotalOffences, 
                start=c(2008), end=c(2020), frequency=1)   
fit_fingal <- auto.arima(fingal_offences, trace = T)

forecast_fingal_offences <-forecast(fit_fingal, 1) 

print(fingal_offences)
print(fit_fingal)

#shows forecasted values
forecast_fingal_offences


#dublin city
dublincity_offences<- ts(totalOffences_dublinCity_yearly$TotalOffences, 
                    start=c(2008), end=c(2020), frequency=1)   
fit_dublincity <- auto.arima(dublincity_offences, trace = T)
forecast_dublincity_offences <- forecast(fit_dublincity, 1) 
print(dublincity_offences)
print(fit_dublincity)

forecast_dublincity_offences


#south dublin
southdublin_offences<- ts(totalOffences_southDublin_yearly$TotalOffences, 
                 start=c(2008), end=c(2020), frequency=1)   
fit_southdublin <- auto.arima(southdublin_offences, trace = T)
forecast_southdublin_offences <- forecast(fit_southdublin, 1) 
print(southdublin_offences)
print(fit_southdublin)

forecast_southdublin_offences


#Dun Laoighre - Rathdown
dunrath_offences<- ts(totalOffences_dunRath_yearly$TotalOffences, 
                    start=c(2008), end=c(2020), frequency=1)   
fit_dunrath <- auto.arima(dunrath_offences, trace = T)
forecast_dunrath_offences <- forecast(fit_dunrath, 1) 
print(dunrath_offences)
print(fit_dunrath)

forecast_dunrath_offences

# plots
#fingal
plot(forecast(fit_fingal, 1), xlab ="Yearly Number of Offences", 
     ylab ="Total Offences", type = "o",
     main ="Forecast of Fingal Total Offences from 2008 - 2021", col.main ="black") 

#dublin city
plot(forecast(fit_dublincity, 1), xlab ="Yearly Number of Offences", 
     ylab ="Total Offences", type = "o",
     main ="Forecast of Dublin City Total Offences from 2008 - 2021", col.main ="black")

#southdublin
plot(forecast(fit_southdublin, 1), xlab ="Yearly Number of Offences", 
     ylab ="Total Offences", type = "o",
     main ="Forecast of SouthDublin Total Offences from 2008 - 2021", col.main ="black") 


#dun laoighre - rathdown
plot(forecast(fit_dunrath, 1), xlab ="Yearly Number of Offences", 
     ylab ="Total Offences", type = "o",
     main ="Forecast of Dun Laoighre- Rathdown Total Offences from 2008 - 2021", col.main ="black")


#combine 
forecasted_offences_values_all_regions <- rbind(forecast_fingal_offences$mean, forecast_dublincity_offences$mean, forecast_southdublin_offences$mean, forecast_dunrath_offences$mean)
forecasted_offences_values_all_regions <- as.data.frame(forecasted_offences_values_all_regions)
names(forecasted_offences_values_all_regions)[1] <- '2021Prediction'

forecasted_offences_values_all_regions$Location[1] <- 'Fingal'
forecasted_offences_values_all_regions$Location[2] <- 'Dublin City'
forecasted_offences_values_all_regions$Location[3] <- 'South Dublin'
forecasted_offences_values_all_regions$Location[4] <- 'Dun Laoighre - Rathdown'


#fingal
autoplot(fingal_offences) +
  autolayer(forecast(fit_fingal, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Offences")+
  ggtitle('Forecast of Fingal Offences')

#dublin city
autoplot(dublincity_offences) +
  autolayer(forecast(fit_dublincity, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Offences")+
  ggtitle('Forecast of Dublin City Offences')

#south dublin
autoplot(southdublin_offences) +
  autolayer(forecast(fit_southdublin, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Offences")+
  ggtitle('Forecast of South Dublin Offences')


#dunlaoire - rathdown
autoplot(dunrath_offences) +
  autolayer(forecast(fit_dunrath, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Offences")+
  ggtitle('Forecast of Dun Laoghaire - Rathdown Offences')


totalOffences <- cbind(fingal_offences, dublincity_offences, southdublin_offences, dunrath_offences)

autoplot(totalOffences, main = "Predicted Number of Offences") +
  autolayer(forecast(fit_fingal, 1), alpha = 0.9, color = 'red') +
  autolayer(forecast(fit_dublincity, 1),  alpha = 0.9, color = 'green') +
  autolayer(forecast(fit_southdublin, 1), alpha = 0.9, color = "blue") +
  autolayer(forecast(fit_dunrath, 1),  alpha = 0.9, color = "purple") +
  guides(colour = guide_legend("Location"))







#GARDA


#find yearly number of gardai
#fingal
totalGarda_fingal_yearly <- aggregate(offences_fingal$`No of Gardai`, by = list(offences_fingal$Year), FUN = mean)
names(totalGarda_fingal_yearly)[1] <- 'Year'
names(totalGarda_fingal_yearly)[2] <- 'Garda'
totalGarda_fingal_yearly$Location <- c("Fingal", "Fingal", "Fingal", "Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal","Fingal")

#round average garda
totalGarda_fingal_yearly$Garda<- round(totalGarda_fingal_yearly$Garda)

#dublin city
totalGarda_dublinCity_yearly <- aggregate(offences_dublinCity$`No of Gardai`, by = list(offences_dublinCity$Year), FUN = mean)
names(totalGarda_dublinCity_yearly)[1] <- 'Year'
names(totalGarda_dublinCity_yearly)[2] <- 'Garda'
totalGarda_dublinCity_yearly$Location <- c("Dublin City", "Dublin City", "Dublin City", "Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City","Dublin City")

#round average garda
totalGarda_dublinCity_yearly$Garda<- round(totalGarda_dublinCity_yearly$Garda)


#south dublin
totalGarda_southDublin_yearly <- aggregate(offences_southDublin$`No of Gardai`, by = list(offences_southDublin$Year), FUN = mean)
names(totalGarda_southDublin_yearly)[1] <- 'Year'
names(totalGarda_southDublin_yearly)[2] <- 'Garda'
totalGarda_southDublin_yearly$Location <- c("South Dublin", "South Dublin", "South Dublin", "South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin","South Dublin")

#round average garda
totalGarda_southDublin_yearly$Garda<- round(totalGarda_southDublin_yearly$Garda)


#dun laoighre - rathdown
totalGarda_dunRath_yearly <- aggregate(offences_dunRath$`No of Gardai`, by = list(offences_dunRath$Year), FUN = mean)
names(totalGarda_dunRath_yearly)[1] <- 'Year'
names(totalGarda_dunRath_yearly)[2] <- 'Garda'
totalGarda_dunRath_yearly$Location <- c("Dun-Laoighre - Rathdown", "Dun-Laoighre - Rathdown", "Dun-Laoighre - Rathdown", "Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown","Dun-Laoighre - Rathdown")

#round average garda
totalGarda_dunRath_yearly$Garda<- round(totalGarda_dunRath_yearly$Garda)


garda_all_regions <- rbind(totalGarda_fingal_yearly,totalGarda_dublinCity_yearly, totalGarda_southDublin_yearly,totalGarda_dunRath_yearly)

#plot of gardai in each of the different areas
ggplot(garda_all_regions, aes(x = Year, y = Garda, color = Location))+
  geom_point()+
  xlim(2007.5, 2020)+
  ggtitle('Yearly Number of Garda per Region')


#predictions of garda
#fingal
garda_fingal<- ts(totalGarda_fingal_yearly$Garda, 
                    start=c(2008), end=c(2020), frequency=1)   
fit_garda_fingal <- auto.arima(garda_fingal)
forecast_garda_fingal <- forecast(fit_garda_fingal, 1) 
print(garda_fingal)
print(fit_garda_fingal)

forecast_garda_fingal

#dublin city
garda_dublincity<- ts(totalGarda_dublinCity_yearly$Garda, 
                      start=c(2008), end=c(2020), frequency=1)   
fit_garda_dublincity <- auto.arima(garda_dublincity)
forecast_garda_dublincity <- forecast(fit_garda_dublincity, 1) 
print(garda_dublincity)
print(fit_garda_dublincity)

forecast_garda_dublincity


#south dublin
garda_southdublin<- ts(totalGarda_southDublin_yearly$Garda, 
                          start=c(2008), end=c(2020), frequency=1)   
fit_garda_southdublin <- auto.arima(garda_southdublin)
forecast_garda_southdublin <- forecast(fit_garda_southdublin, 1) 
print(garda_southdublin)
print(fit_garda_southdublin)

forecast_garda_southdublin

#Dun Laoighre - Rathdown
garda_dunrath<- ts(totalGarda_dunRath_yearly$Garda, 
                           start=c(2008), end=c(2020), frequency=1)   
fit_garda_dunrath <- auto.arima(garda_dunrath)
forecast_garda_dunrath <- forecast(fit_garda_dunrath, 1) 
print(garda_dunrath)
print(fit_garda_dunrath)

forecast_garda_dunrath

#Plots of forecasted values for Garda
#fingal
plot(forecast(fit_garda_fingal, 1), xlab ="Yearly Number of Garda", 
     ylab ="Total Garda", type = "o",
     main ="Forecast of Fingal Total Garda from 2008 - 2021", col.main ="black") 

#dublin city
plot(forecast(fit_garda_dublincity, 1), xlab ="Yearly Number of Garda", 
     ylab ="Total Garda", type = "o",
     main ="Forecast of Dublin City Total Garda from 2008 - 2021", col.main ="black")

#southdublin
plot(forecast(fit_garda_southdublin, 1), xlab ="Yearly Number of Garda", 
     ylab ="Total Offences", type = "o",
     main ="Forecast of SouthDublin Total Garda from 2008 - 2021", col.main ="black") 


#dun laoighre - rathdown
plot(forecast(fit_garda_dunrath, 1), xlab ="Yearly Number of Garda", 
     ylab ="Total Garda", type = "o",
     main ="Forecast of Dun Laoighre- Rathdown Total Garda from 2008 - 2021", col.main ="black")


forecasted_garda_values_all_regions <- rbind(forecast_garda_fingal$mean, forecast_garda_dublincity$mean, forecast_garda_southdublin$mean, forecast_garda_dunrath$mean)
forecasted_garda_values_all_regions <- as.data.frame(forecasted_garda_values_all_regions)
names(forecasted_garda_values_all_regions)[1] <- '2021Prediction'

forecasted_garda_values_all_regions$Location[1] <- 'Fingal'
forecasted_garda_values_all_regions$Location[2] <- 'Dublin City'
forecasted_garda_values_all_regions$Location[3] <- 'South Dublin'
forecasted_garda_values_all_regions$Location[4] <- 'Dun Laoighre - Rathdown'

#fingal
autoplot(garda_fingal) +
  autolayer(forecast(fit_garda_fingal, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Garda")+
  ggtitle('Predicted Level of Garda Personnel - Fingal')

#dublin city
autoplot(garda_dublincity) +
  autolayer(forecast(fit_garda_dublincity, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Garda")+
  ggtitle('Predicted Level of Garda Personnel - Dublin City')

#south dublin
autoplot(garda_southdublin) +
  autolayer(forecast(fit_garda_southdublin, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Garda")+
  ggtitle('Predicted Level of Garda Personnel - South Dublin')


#dunlaoire - rathdown
autoplot(garda_dunrath) +
  autolayer(forecast(fit_garda_dunrath, 1), alpha = 0.9, color = "blue") +
  guides(colour = guide_legend("Location"))+
  xlab("Time (Years)")+
  ylab("#Garda")+
  ggtitle('Predicted Level of Garda Personnel - Dun Laoighre - Rathdown')

totalGarda_all_regions <- cbind(garda_fingal, garda_dublincity, garda_southdublin, garda_dunrath)

autoplot(totalGarda_all_regions, main = "Predicted Level of Garda Personnel") +
  autolayer(forecast(fit_garda_fingal, 1), alpha = 0.9, color = 'red') +
  autolayer(forecast(fit_garda_dublincity, 1),  alpha = 0.9, color = 'green') +
  autolayer(forecast(fit_garda_southdublin, 1), alpha = 0.9, color = "blue") +
  autolayer(forecast(fit_garda_dunrath, 1),  alpha = 0.9, color = "purple") +
  guides(colour = guide_legend("Location"))


#getting crime trend figures for each area
#values suggest the amount of crimes commited per garda in the area
fingal_offences_per_garda <- totalOffences_fingal_yearly$TotalOffences/totalGarda_fingal_yearly$Garda
southdublin_offences_per_garda <- totalOffences_southDublin_yearly$TotalOffences/totalGarda_southDublin_yearly$Garda
dublincity_offences_per_garda <- totalOffences_dublinCity_yearly$TotalOffences/totalGarda_dublinCity_yearly$Garda
dunrath_offences_per_garda <- totalOffences_dunRath_yearly$TotalOffences/totalGarda_dunRath_yearly$Garda


#combine to have a dataframe that consists of offences and garda for all regions
offences_garda_all_regions <- cbind(garda_all_regions, totalOffences_all_regions)

#remove duplicated columns
offences_garda_all_regions <- offences_garda_all_regions %>% select(-4, -6)

#Plot showing the number of garda vs the number of offences per region
ggplot(offences_garda_all_regions, aes(x = Garda, y = TotalOffences, color = Location))+
  geom_point()+
  xlab("#Garda")+
  ylab("#Offences")+
  ggtitle('The Number of Garda vs The Number of Offences per Region')+
  geom_smooth(method = 'lm')
 

#offences/garda
fingal_offences_per_garda <- as.data.frame(fingal_offences_per_garda)
dublincity_offences_per_garda <- as.data.frame(dublincity_offences_per_garda)
southdublin_offences_per_garda <- as.data.frame(southdublin_offences_per_garda)
dunrath_offences_per_garda <- as.data.frame(dunrath_offences_per_garda)

#fingal
names(fingal_offences_per_garda)[1] <- 'Trend'
fingal_offences_per_garda$Location <-  c('Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal')
fingal_offences_per_garda$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

#dublincity
names(dublincity_offences_per_garda)[1] <- 'Trend'
dublincity_offences_per_garda$Location <-  c('Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City')
dublincity_offences_per_garda$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

#southdublin
names(southdublin_offences_per_garda)[1] <- 'Trend'
southdublin_offences_per_garda$Location <-  c('South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin')
southdublin_offences_per_garda$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

#dun laoighre - rathdown
names(dunrath_offences_per_garda)[1] <- 'Trend'
dunrath_offences_per_garda$Location <-  c('Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown')
dunrath_offences_per_garda$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

offences_per_garda_all_regions <- rbind(fingal_offences_per_garda,dublincity_offences_per_garda, southdublin_offences_per_garda, dunrath_offences_per_garda )


#########################INCOME##################################
income_poverty <- read.csv("Income&PovertyRates.csv", header=T, na.strings=c(""), stringsAsFactors = T)


#Exploratory Data Analysis
glimpse(income_poverty)
summary(income_poverty)


#changing dataframe column names
names(income_poverty)[1] <- "Statistic"
names(income_poverty)[3] <- "Age_Range"
names(income_poverty)[4] <- "Unit"
names(income_poverty)[5] <- "Value"

#getting mean real real disposable income from full data frame
income <- data.frame(ageGroup = c(income_poverty$Age_Range), year = c(income_poverty$Year),meanRealDispInc = c(income_poverty$Statistic == "Mean Real Household Disposable Income"),income_poverty$Value)

#filtering data to new data frame based on the values that came up as TRUE
income_filter<- filter(income, meanRealDispInc == "TRUE")

#removes logical(containing TRUE) column from data frame
income_filtered = subset(income_filter, select = -meanRealDispInc)

#changing dataframe names
names(income_filtered)[1] <- "Age_Group"
names(income_filtered)[2] <- "Year"
names(income_filtered)[3] <- "Value"

#removes age group to have only values for clustering
income_cluster = subset(income_filtered, select = -Age_Group)

#avg income by year - all ages(no age group)
#remove age group 0-17 as it contains NA's
average_income <- income_filtered[-c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46),]

#imputation for na values in 0-17 years
income_filtered$Value[is.na(income_filtered$Value)] <- mean(income_filtered$Value[!is.na(income_filtered$Value)]) 
income_filtered <- income_filtered[-c(1:12), ]

#gets mean of values by year for people 18+
average_income <- aggregate(average_income$Value, by=list(average_income$Year), FUN=mean)
names(average_income)[1] = "Year"
names(average_income)[2] = "Value"
average_income <- round(average_income)

ggplot(average_income, aes(x = Year, y = Value)) +
  geom_point()+
  ggtitle('Yearly Average Income for Dublin')

#time series forecast for income
averageIncome <- ts(average_income$Value, 
                    start=c(2004), end=c(2019), frequency=1)   
fitAverageIncome <- auto.arima(averageIncome)
forecast_averageIncome <- forecast(fitAverageIncome, 1) 
print(averageIncome)
print(fitAverageIncome)

forecast_averageIncome

#average yearly earning in 2020 was 43,940 - dublin.ie, we are 90% accurate
43940/48471.2 * 100


#plot of forecast
autoplot(averageIncome) +
  autolayer(forecast(fitAverageIncome, 1),  alpha = 0.9, color = "blue") +
  xlab("Time (Years)")+
  ylab("Average Income")+
  ggtitle('Forecasted Value of Average Income - Dublin')



#adding forecasted income value for 2020 to orignal dataframe
forecasted_average_value <- forecast_averageIncome$mean
forecasted_average_value <- as.data.frame(forecasted_average_value)
names(forecasted_average_value)[1] <- 'Value'
forecasted_average_value$Year <- c("2020")
forecasted_average_value <- forecasted_average_value[,c("Year", "Value")]
average_income <- rbind(average_income, forecasted_average_value)
average_income$Value <- round(average_income$Value)

#removing rows average income dataframe to combine all variables into one dataframe with same time
#remove 2nd through 4th row
average_income <- average_income[-c(1:4), ]


#VISUALISATIONS#
#AGE GROUP INCOME BY YEAR
#Imputation for 0-17 years
ggplot(income_filtered, aes(x = Year, y = Value, color = Age_Group)) +
  geom_line(size=1) + geom_point(size=2) +
  labs(title = "Age Group Mean Household Disposable Income",
       subtitle = "       by Year",
       xlab = "Year",
       ylab = "Income",
       color = "Age"
  ) +
  scale_x_continuous(breaks=seq(2008, 2020,2))+
  scale_y_continuous(breaks=seq(20000, 60000,5000))+
  theme_fivethirtyeight() +
  theme(axis.title = element_text())

#plot of income
ggplot(average_income, aes(x = Year, y = Value)) +
  geom_line(size=1, color=7)+ geom_point(size=2,color=4)+
  labs( title = "Average Household Disposable Income",
        subtitle = "       by Year",
        x = "Year",  
        y = "Income"
  )


##################EDUCATION##################################

education <- read.csv("pupils in secondary school 2008-2020.csv", header=T, na.strings=c(""), stringsAsFactors = T)


names(education)
names(education)[1] <- 'Statistic'
names(education)[3] <- 'Location'
names(education)[6] <- 'Value'

education <- subset(education, select = -c(UNIT))
education <- subset(education, select = -c(Nationality.of.Pupil))

summary(education)

fingal <- ('Fingal') 
dublin_city <- ("Dublin City")
southDub <- ("South Dublin")
dun_rath<-("Dún Laoghaire-Rathdown")

#specify regions of dublin
dublin_city <- education[education$Location %in% dublin_city, ]
southDub <- education[education$Location %in% southDub, ]
fingal <- education[education$Location %in% fingal, ]
dun_rath <- education[education$Location %in% dun_rath, ]

#total amount in secondary education Fingal per year (2008 - 2020)
fingal_total_educated_yearly <- aggregate(fingal$Value, by = list(fingal$Year), FUN = sum) 
names(fingal_total_educated_yearly)[1] <- 'Year'
names(fingal_total_educated_yearly)[2] <- 'totalEducated'

#total amount in secondary education North per year (2008-2020)
dublin_city_total_educated_yearly <- aggregate(dublin_city$Value, by = list(dublin_city$Year), FUN = sum) 
names(dublin_city_total_educated_yearly)[1] <- 'Year'
names(dublin_city_total_educated_yearly)[2] <- 'totalEducated'

#total amount in secondary education South Dublin per year (2008 - 2020)
southDub_total_educated_yearly <- aggregate(southDub$Value, by = list(southDub$Year), FUN = sum) 
names(southDub_total_educated_yearly)[1] <- 'Year'
names(southDub_total_educated_yearly)[2] <- 'totalEducated'


#total amount in secondary education DunLaoighre - Rathdown per year (2008 - 2020)
dun_rath_total_educated_yearly <- aggregate(dun_rath$Value, by = list(dun_rath$Year), FUN = sum) 
names(dun_rath_total_educated_yearly)[1] <- 'Year'
names(dun_rath_total_educated_yearly)[2] <- 'totalEducated'


#plot of yearlt educated values
plot(fingal_total_educated_yearly)
plot(dublin_city_total_educated_yearly)
plot(dun_rath_total_educated_yearly)
plot(southDub_total_educated_yearly)


fingal_total_educated_yearly$Location <- c('Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal')
dublin_city_total_educated_yearly$Location <- c('Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City')
dun_rath_total_educated_yearly$Location <- c('Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown')
southDub_total_educated_yearly$Location <- c('South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin')

#Plot of Educated
dublin_educated <- rbind(fingal_total_educated_yearly, dublin_city_total_educated_yearly,dun_rath_total_educated_yearly,southDub_total_educated_yearly )
plot(dublin_educated)


#GGplots
#dublin educated
ggplot(dublin_educated, aes(x = Year , y = totalEducated, color = Location))+
  geom_point() +
  xlim(2007.5, 2020)+
  ggtitle('Yearly Number in Second Level Education per Region')

#fingal
ggplot(fingal_total_educated_yearly, aes(x = Year , y = totalEducated, color = Location))+
  geom_point() +
  xlim(2007.5, 2020)+
  ggtitle('Yearly Number in Second Level Education: Fingal')


#dublin city
ggplot(dublin_city_total_educated_yearly, aes(x = Year , y = totalEducated, color = Location))+
  geom_point() +
  xlim(2007.5, 2020)+
ggtitle('Yearly Number in Second Level Education: Dublin City')



#south dublin
ggplot(southDub_total_educated_yearly, aes(x = Year , y = totalEducated, color = Location))+
  geom_point() +
  xlim(2007.5, 2020)+
  ggtitle('Yearly Number in Second Level Education: South Dublin')


#dunlaoighre - rathdown
ggplot(dun_rath_total_educated_yearly, aes(x = Year , y = totalEducated, color = Location))+
  geom_point() +
  xlim(2007.5, 2020)+
  ggtitle('Yearly Number in Second Level Education: Dun Laoighre - Rathdown')





#normalise percentage of population per year
dublinregions_population <- read.csv("population of dublin regions 2016.csv", header=T, na.strings=c(""), stringsAsFactors = T)



#total population split 
#2016 census 
#apply that split for rest of years as an assumption

#dublin population for 2016 = 1347359
#dun laoighre-rathdown = 218018
#fingal =  296020 
#south dublin = 278767
#dublin city = 554554
#visualise this in a piechart

#2016
dublinpop <- 1347359

#22% of people in fingal 
percentage_fingalpopulation <- 296020 / 1347359
percentage_fingalpopulation <- round(percentage_fingalpopulation, digits = 2)


#41% of people in dublin city
percentage_dublincitypopulation <- 554554/1347359
percentage_dublincitypopulation <- round(percentage_dublincitypopulation, digits = 2)


#21% of people in southdublin
percentage_southdublinpopulation <- 278767/1347359
percentage_southdublinpopulation <- round(percentage_southdublinpopulation, digits = 2)


#16% of people in dun-laoighre
percentage_dunrathpopulation <- 218018/1347359
percentage_dunrathpopulation <- round(percentage_dunrathpopulation, digits = 2)


percentage <- c(percentage_fingalpopulation, percentage_dublincitypopulation, percentage_southdublinpopulation, percentage_dunrathpopulation)
regions <- c("Fingal", "Dublin City", "South Dublin", "Dun Laoighre - Rathdown")
piepercent<- round(100*percentage/sum(percentage), 1)

pie(percentage, labels = piepercent, main = "Percentage of Population in Regions of Dublin",col = rainbow(length(percentage)))
legend("topright", c("Fingal","Dublin City","South Dublin","Dun Laoighre - Rathdown"), cex = 0.8,
       fill = rainbow(length(percentage)))

#find population of each region from 2008 - 2020 using 2016 as an assumption
#population used for 2008-2020 are estimates from worldpopulationreview except 2016(cencus)
#fingal
fingal_pop_2008 <- 1073906 * percentage_fingalpopulation
fingal_pop_2009 <- 1086983 * percentage_fingalpopulation
fingal_pop_2010 <- 1100238 * percentage_fingalpopulation
fingal_pop_2011 <- 1113343 * percentage_fingalpopulation
fingal_pop_2012 <- 1125529 * percentage_fingalpopulation
fingal_pop_2013 <- 1137814 * percentage_fingalpopulation
fingal_pop_2014 <- 1150251 * percentage_fingalpopulation
fingal_pop_2015 <- 1162823 * percentage_fingalpopulation
fingal_pop_2016 <- 1347359 * percentage_fingalpopulation
fingal_pop_2017 <- 1188418 * percentage_fingalpopulation
fingal_pop_2018 <- 1201426 * percentage_fingalpopulation
fingal_pop_2019 <- 1214666 * percentage_fingalpopulation
fingal_pop_2020 <- 1228179 * percentage_fingalpopulation


#dublin city
dublincity_pop_2008 <- 1073906 * percentage_dublincitypopulation
dublincity_pop_2009 <- 1086983 * percentage_dublincitypopulation
dublincity_pop_2010 <- 1100238 * percentage_dublincitypopulation
dublincity_pop_2011 <- 1113343 * percentage_dublincitypopulation
dublincity_pop_2012 <- 1125529 * percentage_dublincitypopulation
dublincity_pop_2013 <- 1137814 * percentage_dublincitypopulation
dublincity_pop_2014 <- 1150251 * percentage_dublincitypopulation
dublincity_pop_2015 <- 1162823 * percentage_dublincitypopulation
dublincity_pop_2016 <- 1347359 * percentage_dublincitypopulation
dublincity_pop_2017 <- 1188418 * percentage_dublincitypopulation
dublincity_pop_2018 <- 1201426 * percentage_dublincitypopulation
dublincity_pop_2019 <- 1214666 * percentage_dublincitypopulation
dublincity_pop_2020 <- 1228179 * percentage_dublincitypopulation


#south dublin
southdublin_pop_2008 <- 1073906 * percentage_southdublinpopulation
southdublin_pop_2009 <- 1086983 * percentage_southdublinpopulation
southdublin_pop_2010 <- 1100238 * percentage_southdublinpopulation
southdublin_pop_2011 <- 1113343 * percentage_southdublinpopulation
southdublin_pop_2012 <- 1125529 * percentage_southdublinpopulation
southdublin_pop_2013 <- 1137814 * percentage_southdublinpopulation
southdublin_pop_2014 <- 1150251 * percentage_southdublinpopulation
southdublin_pop_2015 <- 1162823 * percentage_southdublinpopulation
southdublin_pop_2016 <- 1347359 * percentage_southdublinpopulation
southdublin_pop_2017 <- 1188418 * percentage_southdublinpopulation
southdublin_pop_2018 <- 1201426 * percentage_southdublinpopulation
southdublin_pop_2019 <- 1214666 * percentage_southdublinpopulation
southdublin_pop_2020 <- 1228179 * percentage_southdublinpopulation

#dun laoighre - rathdown
dun_rathdown_pop_2008 <- 1073906 * percentage_dunrathpopulation
dun_rathdown_pop_2009 <- 1086983 * percentage_dunrathpopulation
dun_rathdown_pop_2010 <- 1100238 * percentage_dunrathpopulation
dun_rathdown_pop_2011 <- 1113343 * percentage_dunrathpopulation
dun_rathdown_pop_2012 <- 1125529  * percentage_dunrathpopulation
dun_rathdown_pop_2013 <- 1137814  * percentage_dunrathpopulation
dun_rathdown_pop_2014 <- 1150251  * percentage_dunrathpopulation
dun_rathdown_pop_2015 <- 1162823  * percentage_dunrathpopulation
dun_rathdown_pop_2016 <- 1347359  * percentage_dunrathpopulation
dun_rathdown_pop_2017 <- 1188418  * percentage_dunrathpopulation
dun_rathdown_pop_2018 <- 1201426  * percentage_dunrathpopulation
dun_rathdown_pop_2019 <- 1214666  * percentage_dunrathpopulation
dun_rathdown_pop_2020 <- 1228179  * percentage_dunrathpopulation


#combine to form one 

FingalPopulation <- rbind(fingal_pop_2008,
                          fingal_pop_2009,
                          fingal_pop_2010,
                          fingal_pop_2011,
                          fingal_pop_2012,
                          fingal_pop_2013,
                          fingal_pop_2014,
                          fingal_pop_2015,
                          fingal_pop_2016,
                          fingal_pop_2017,
                          fingal_pop_2018,
                          fingal_pop_2019,
                          fingal_pop_2020)
FingalPopulation <- round(FingalPopulation)

DublinCityPopulation <- rbind(dublincity_pop_2008,
                              dublincity_pop_2009,
                              dublincity_pop_2010,
                              dublincity_pop_2011,
                              dublincity_pop_2012,
                              dublincity_pop_2013,
                              dublincity_pop_2014,
                              dublincity_pop_2015,
                              dublincity_pop_2016,
                              dublincity_pop_2017,
                              dublincity_pop_2018,
                              dublincity_pop_2019,
                              dublincity_pop_2020)
DublinCityPopulation <- round(DublinCityPopulation)

SouthDublinPopulation <- rbind(southdublin_pop_2008,
                               southdublin_pop_2009,
                               southdublin_pop_2010,
                               southdublin_pop_2011,
                               southdublin_pop_2012,
                               southdublin_pop_2013,
                               southdublin_pop_2014,
                               southdublin_pop_2015,
                               southdublin_pop_2016,
                               southdublin_pop_2017,
                               southdublin_pop_2018,
                               southdublin_pop_2019,
                               southdublin_pop_2020)
SouthDublinPopulation <- round(SouthDublinPopulation)

Dun_RathPopulation <- rbind(dun_rathdown_pop_2008,
                            dun_rathdown_pop_2009,
                            dun_rathdown_pop_2010,
                            dun_rathdown_pop_2011,
                            dun_rathdown_pop_2012,
                            dun_rathdown_pop_2013,
                            dun_rathdown_pop_2014,
                            dun_rathdown_pop_2015,
                            dun_rathdown_pop_2016,
                            dun_rathdown_pop_2017,
                            dun_rathdown_pop_2018,
                            dun_rathdown_pop_2019,
                            dun_rathdown_pop_2020)
Dun_RathPopulation <- round(Dun_RathPopulation)



#Prediction of total Educated from 2008 - 2020
#Fingal
FingalEducate <- ts(fingal_total_educated_yearly$totalEducated, 
                    start=c(2008), end=c(2020), frequency=1)
fitFingalEducate <- auto.arima(FingalEducate)
forecast_education_fingal <- forecast(fitFingalEducate, 1) 
print(FingalEducate)
print(fitFingalEducate)

forecast_education_fingal

#plot of fingal education forecast 2021
plot(forecast(fitFingalEducate,1))



#Dublin City
DublinCityEducate <- ts(dublin_city_total_educated_yearly$totalEducated, 
                        start=c(2008), end=c(2020), frequency=1)
fitDublinCityEducate <- auto.arima(DublinCityEducate)
forecast_education_dublincity <- forecast(fitDublinCityEducate, 1) 
print(DublinCityEducate)
print(fitDublinCityEducate)

forecast_education_dublincity

#plot of dublin city education forecast 2021
plot(forecast(fitDublinCityEducate,1))

#Prediction of total Educated from 2008 - 2022 for South Dublin
SouthDubEducate <- ts(southDub_total_educated_yearly$totalEducated, 
                      start=c(2008), end=c(2020), frequency=1)
fitSouthDubEducate <- auto.arima(SouthDubEducate)
forecast_education_southdublin <- forecast(fitSouthDubEducate, 1) 
print(SouthDubEducate)
print(fitSouthDubEducate)

#plot of south dublin education forecast 2021
plot(forecast(fitSouthDubEducate,1))

forecast_education_southdublin

#Prediction of total Educated from 2008 - 2022 for DunLaoighre - Rathdown
DunRathEducate <- ts(dun_rath_total_educated_yearly$totalEducated, 
                     start=c(2008), end=c(2020), frequency=1)
fitDunRathEducate <- auto.arima(DunRathEducate)
forecast_education_dunrath <- forecast(fitDunRathEducate, 1) 
print(DunRathEducate)
print(fitDunRathEducate)

forecast_education_dunrath

#plot of dun laoighre - rathdown education forecast 2021
plot(forecast(fitDunRathEducate,1))

#forecasted education values
forecasted_education_values_all_regions <- rbind(forecast_education_fingal$mean, forecast_education_dublincity$mean, forecast_education_southdublin$mean, forecast_education_dunrath$mean)
forecasted_education_values_all_regions <- as.data.frame(forecasted_education_values_all_regions)
names(forecasted_education_values_all_regions)[1] <- '2021Prediction'

forecasted_education_values_all_regions$Location[1] <- 'Fingal'
forecasted_education_values_all_regions$Location[2] <- 'Dublin City'
forecasted_education_values_all_regions$Location[3] <- 'South Dublin'
forecasted_education_values_all_regions$Location[4] <- 'Dun Laoighre - Rathdown'
forecasted_education_values_all_regions$`2021Prediction` <- round(forecasted_education_values_all_regions$`2021Prediction`)


#Combine to compare in plot
totalEducate <- cbind(DublinCityEducate, DunRathEducate, SouthDubEducate, FingalEducate)



#Plot of Education Forecast
totalEducate <- as.ts(totalEducate) # for plot purposes
autoplot(totalEducate, main = "Predicted Amount in Second Level Education") +
  autolayer(forecast(fitFingalEducate, 1), alpha = 0.9, color = 'purple')+
  autolayer(forecast(fitDublinCityEducate, 1), alpha = 0.9, color = "red") +
  autolayer(forecast(fitSouthDubEducate, 1),  alpha = 0.9, color = 'blue') +
  autolayer(forecast(fitDunRathEducate, 1),  alpha = 0.9, color = "green") +
  guides(colour = guide_legend("Location"))


#changing each to dataframe in order to add population column to each
FingalEducate <- as.data.frame(FingalEducate)
DublinCityEducate <- as.data.frame(DublinCityEducate)
SouthDubEducate <- as.data.frame(SouthDubEducate)
DunRathEducate <- as.data.frame(DunRathEducate)


#add population + year column to each 
#fingal
names(FingalEducate)[1] <- 'Education'
FingalEducate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
FingalEducate$Population <- FingalPopulation
FingalEducate$Location <- c('Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal')
FingalEducate$Income <- average_income$Value



#dublin city
names(DublinCityEducate)[1] <- 'Education'
DublinCityEducate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
DublinCityEducate$Population <- DublinCityPopulation
DublinCityEducate$Location <- c('Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City')
DublinCityEducate$Income <- average_income$Value


#south dublin
names(SouthDubEducate)[1] <- 'Education'
SouthDubEducate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
SouthDubEducate$Population <- SouthDublinPopulation
SouthDubEducate$Location <- c('South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin')
SouthDubEducate$Income <- average_income$Value


#dun laoighre - rathdown
names(DunRathEducate)[1] <- 'Education'
DunRathEducate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
DunRathEducate$Population <- Dun_RathPopulation
DunRathEducate$Location <- c('Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown')
DunRathEducate$Income <- average_income$Value



#rearrange columns
FingalEducate <- FingalEducate %>% select(Year, Population, Education, Location, Income)
DublinCityEducate <- DublinCityEducate %>% select(Year, Population, Education, Location, Income)
SouthDubEducate <- SouthDubEducate %>% select(Year, Population, Education, Location, Income)
DunRathEducate <- DunRathEducate %>% select(Year, Population, Education, Location, Income)

#visualisations
#fingal
ggplot(FingalEducate, aes(Population, Education, colour = Population)) + 
  geom_point()+
  geom_text_repel(aes(label = Year))+
  ggtitle('Yearly Education vs Population :Fingal')


#dublin city
ggplot(DublinCityEducate, aes(Population, Education, colour = Population)) + 
  geom_point()+
  geom_text_repel(aes(label = Year))+
  ggtitle('Yearly Education vs Population :Dublin City')



#southdublin
ggplot(SouthDubEducate, aes(Population, Education, colour = Population)) + 
  geom_point()+
  geom_text_repel(aes(label = Year))+
  ggtitle('Yearly Education vs Population :South Dublin')



#dun loaighre - rathdown
ggplot(DunRathEducate, aes(Population, Education, colour = Population)) + 
  geom_point()+
  geom_text_repel(aes(label = Year))+
  ggtitle('Yearly Education vs Population :Dun Laoighre - Rathdown ')



#remove location from offences_garda_all_regions
#combine education - each region
#combine to have garda/education/offences in one dataframe
#changing each to dataframe in order to add population column to each
FingalEducate <- as.matrix(FingalEducate)
DublinCityEducate <- as.matrix(DublinCityEducate)
SouthDubEducate <- as.matrix(SouthDubEducate)
DunRathEducate <- as.matrix(DunRathEducate)




education_all_regions <- rbind(FingalEducate, DublinCityEducate, SouthDubEducate, DunRathEducate)
education_all_regions <- as.data.frame(education_all_regions)



#combine education with garda+offences
offences_garda_education_income_all_regions <- cbind(offences_garda_all_regions, education_all_regions)

#convert population and education to numeric
offences_garda_education_income_all_regions$Population <- as.numeric(offences_garda_education_income_all_regions$Population)
offences_garda_education_income_all_regions$Education <- as.numeric(offences_garda_education_income_all_regions$Education)

#remove duplicated columns
offences_garda_education_income_all_regions <- offences_garda_education_income_all_regions %>% select(-5, -8,) 



#filter out each region from offences_garda_education_all_regions
#fingal
fingal_offences_garda_education_income <- filter(
  offences_garda_education_income_all_regions,Location == "Fingal")

#dublin city
dublincity_offences_garda_education_income <- filter(
  offences_garda_education_income_all_regions,Location == "Dublin City")

#south dublin
southdublin_offences_garda_education_income <- filter(
  offences_garda_education_income_all_regions,Location == "South Dublin")

#dun laoighre - rathdown
dunrath_offences_garda_education_income <- filter(
  offences_garda_education_income_all_regions,Location == "Dun-Laoighre - Rathdown")





#TRENDS#
#offences/education
#fingal
fingal_offences_per_education <- totalOffences_fingal_yearly$TotalOffences/fingal_offences_garda_education_income$Education

#dublincity
dublincity_offences_per_education <- totalOffences_dublinCity_yearly$TotalOffences/dublincity_offences_garda_education_income$Education

#south dublin
southdublin_offences_per_education <- totalOffences_southDublin_yearly$TotalOffences/southdublin_offences_garda_education_income$Education

#dun laoighre - rathdown
dunrath_offences_per_education <- totalOffences_dunRath_yearly$TotalOffences/dunrath_offences_garda_education_income$Education

#convert each offences_per_education to dataframe
fingal_offences_per_education <- as.data.frame(fingal_offences_per_education)
dublincity_offences_per_education <- as.data.frame(dublincity_offences_per_education)
southdublin_offences_per_education <- as.data.frame(southdublin_offences_per_education)
dunrath_offences_per_education <- as.data.frame(dunrath_offences_per_education)

#round trend figures to two decimal places
fingal_offences_per_education <- round(fingal_offences_per_education$fingal_offences_per_education, digits = 2)
dublincity_offences_per_education <- round(dublincity_offences_per_education$dublincity_offences_per_education, digits = 2)
southdublin_offences_per_education <- round(southdublin_offences_per_education$southdublin_offences_per_education, digits = 2)
dunrath_offences_per_education <- round(dunrath_offences_per_education$dunrath_offences_per_education, digits = 2)

#convert each offences_per_education to dataframe
fingal_offences_per_education <- as.data.frame(fingal_offences_per_education)
dublincity_offences_per_education <- as.data.frame(dublincity_offences_per_education)
southdublin_offences_per_education <- as.data.frame(southdublin_offences_per_education)
dunrath_offences_per_education <- as.data.frame(dunrath_offences_per_education)


#change names of each 
names(fingal_offences_per_education)[1] <- 'Trend'
names(dublincity_offences_per_education)[1] <- 'Trend'
names(southdublin_offences_per_education)[1] <- 'Trend'
names(dunrath_offences_per_education)[1] <- 'Trend'


#Add location + year to each 
fingal_offences_per_education$Location <-  c('Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal')
fingal_offences_per_education$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

dublincity_offences_per_education$Location <- c('Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City')
dublincity_offences_per_education$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

southdublin_offences_per_education$Location <- c('South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin')
southdublin_offences_per_education$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

dunrath_offences_per_education$Location <- c('Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown')
dunrath_offences_per_education$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)



offences_per_education_trends <- rbind(fingal_offences_per_education,dublincity_offences_per_education, southdublin_offences_per_education, dunrath_offences_per_education )


#TRENDS#
#Find the crime rate per region - offences/population
#fingal
fingal_crime_rate <- fingal_offences_garda_education_income$TotalOffences/fingal_offences_garda_education_income$Population * 100

fingal_crime_rate <- as.data.frame(fingal_crime_rate)
names(fingal_crime_rate)[1] <- 'Trend'
fingal_crime_rate$Location <-  c('Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal', 'Fingal')
fingal_crime_rate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
row.names(fingal_crime_rate) <- 1:nrow(fingal_crime_rate)

#dublin city
dublincity_crime_rate <- dublincity_offences_garda_education_income$TotalOffences/dublincity_offences_garda_education_income$Population * 100

dublincity_crime_rate <- as.data.frame(dublincity_crime_rate)
names(dublincity_crime_rate)[1] <- 'Trend'
dublincity_crime_rate$Location <-   c('Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City', 'Dublin City')
dublincity_crime_rate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
row.names(dublincity_crime_rate) <- 1:nrow(fingal_crime_rate)

#south dublin
southdublin_crime_rate <- southdublin_offences_garda_education_income$TotalOffences/southdublin_offences_garda_education_income$Population * 100

southdublin_crime_rate <- as.data.frame(southdublin_crime_rate)
names(southdublin_crime_rate)[1] <- 'Trend'
southdublin_crime_rate$Location <-   c('South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin', 'South Dublin')
southdublin_crime_rate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
row.names(southdublin_crime_rate) <- 1:nrow(southdublin_crime_rate)

#dun-laoighre - rathdown
dunrath_crime_rate <- dunrath_offences_garda_education_income$TotalOffences/dunrath_offences_garda_education_income$Population * 100

dunrath_crime_rate <- as.data.frame(dunrath_crime_rate)
names(dunrath_crime_rate)[1] <- 'Trend'
dunrath_crime_rate$Location <-   c('Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown', 'Dun Laoighre - Rathdown')
dunrath_crime_rate$Year <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
row.names(dunrath_crime_rate) <- 1:nrow(dunrath_crime_rate)

crime_rate_all_regions <- rbind(fingal_crime_rate, dublincity_crime_rate, southdublin_crime_rate, dunrath_crime_rate)

#Find Education Attainment Rate
#Fingal
fingal_education_attainment_rate <- fingal_offences_garda_education_income$Education/fingal_offences_garda_education_income$Population * 100
fingal_education_attainment_rate <- as.data.frame(fingal_education_attainment_rate)
names(fingal_education_attainment_rate)[1] <- 'Trend'
row.names(fingal_education_attainment_rate) <- 1:nrow(fingal_education_attainment_rate)

#dublincity
dublincity_education_attainment_rate <- dublincity_offences_garda_education_income$Education/dublincity_offences_garda_education_income$Population * 100
dublincity_education_attainment_rate <- as.data.frame(dublincity_education_attainment_rate)
names(dublincity_education_attainment_rate)[1] <- 'Trend'
row.names(dublincity_education_attainment_rate) <- 1:nrow(dublincity_education_attainment_rate)

#southdublin
southdublin_education_attainment_rate <- southdublin_offences_garda_education_income$Education/southdublin_offences_garda_education_income$Population * 100
southdublin_education_attainment_rate <- as.data.frame(southdublin_education_attainment_rate)
names(southdublin_education_attainment_rate)[1] <- 'Trend'
row.names(southdublin_education_attainment_rate) <- 1:nrow(southdublin_education_attainment_rate)

#dunlaoighre-rathdown
dunrath_education_attainment_rate <- dunrath_offences_garda_education_income$Education/dunrath_offences_garda_education_income$Population * 100
dunrath_education_attainment_rate <- as.data.frame(dunrath_education_attainment_rate)
names(dunrath_education_attainment_rate)[1] <- 'Trend'
row.names(dunrath_education_attainment_rate) <- 1:nrow(dunrath_education_attainment_rate)

education_attainment_all_regions <- rbind(fingal_education_attainment_rate, dublincity_education_attainment_rate, southdublin_education_attainment_rate, dunrath_education_attainment_rate)






#join crime rate and education
education_crime_rates <- cbind(crime_rate_all_regions, education_attainment_all_regions)
names(education_crime_rates)[1] <- 'CrimeRate'
names(education_crime_rates)[4] <- 'EducationRate'
education_crime_rates$CrimeRate <- round(education_crime_rates$CrimeRate , digits = 2)
education_crime_rates$EducationRate <- round(education_crime_rates$EducationRate , digits = 2)


#add crime rate and education rate to dataframe with all variables
offences_garda_education_income_all_regions <- cbind(offences_garda_education_income_all_regions, education_crime_rates)
offences_garda_education_income_all_regions <- offences_garda_education_income_all_regions %>% select(-9, -10,) 

  



#ViSUALs / RESULTS
#Plot showing the trend of education attainment 
ggplot(offences_garda_education_income_all_regions, aes( x=Year, y=EducationRate, colour=Location, group=Location )) + 
  geom_line()+
  ggtitle('Education Rate Trends per Region')


#crime rate trend plot
ggplot(offences_garda_education_income_all_regions, aes( x=Year, y=CrimeRate, colour=Location ),) + 
  geom_line()+
  ggtitle('Crime Rate Trends per Region')


#Plot showing the number of garda vs the number of offences for each region
ggplot(offences_garda_education_income_all_regions, aes( x=Garda, y=TotalOffences, colour=Location )) + 
  geom_point()+
  ggtitle('The Number of Garda vs The Number of Offences per Region')+
  geom_smooth(method = 'lm')

##Plot showing the number in education vs the number of offences for each region
ggplot(offences_garda_education_income_all_regions, aes( x=Education, y=TotalOffences, colour=Location )) + 
  geom_point()+
  ggtitle('The Number in Second Level Education vs The Number of Offences per Region')+
  geom_smooth(method = 'lm')

#plot showing the number of offence vs population
ggplot(offences_garda_education_income_all_regions, aes( x=Population, y=TotalOffences, colour=Location )) + 
  geom_point()+
  ggtitle('The Population vs The Number of Offences per Region')+
  geom_smooth(method='lm')


#Crime rate vs Garda
ggplot(offences_garda_education_income_all_regions, aes( x=CrimeRate, y=Garda, colour=Location )) + 
  geom_point()+
  ggtitle('Crime Rate vs Garda per Region')+
  geom_smooth(method='lm')


#income vs offences
ggplot(offences_garda_education_income_all_regions, aes( x=Income, y=TotalOffences, colour=Location )) + 
  geom_point()+
  ggtitle('The Average Income in Dublin vs The Number of Offences per Region')+
  geom_smooth(method='lm')


#education vs crime rate
ggplot(offences_garda_education_income_all_regions, aes( x=EducationRate, y=CrimeRate, colour=Location )) + 
  geom_point()+
  ggtitle('The Education Rate vs Crime Rate per Region')+
  geom_smooth(method='lm')


#education rate vs number of offences
ggplot(offences_garda_education_income_all_regions, aes( x=EducationRate, y=TotalOffences, colour=Location )) + 
  geom_point()+
  ggtitle('The Education Rate vs Number of Offences per Region')+
  geom_smooth(method='lm')


#Income vs number of offences
offences_garda_education_income_all_regions$Income <- as.numeric(offences_garda_education_income_all_regions$Income)
ggplot(offences_garda_education_income_all_regions, aes( x=Income, y=TotalOffences, colour=Location )) + 
  geom_point()+
  xlim(30000, 50000)+
  ggtitle('The Average Income of Dublin vs Offences per Region')+
  geom_smooth(method='lm')








#############Clustering#################
#change year to numeric for clustering
average_income$Year <- as.numeric(average_income$Year)

#number of clusters vs tss
fviz_nbclust(average_income, kmeans, method = "wss")


#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 2 clusters
km <- kmeans(average_income, centers = 2, nstart = 25)

#view results
km

fviz_cluster(km, data = average_income, main = "Average Income Cluster")








