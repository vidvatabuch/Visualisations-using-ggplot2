#Set the Working Directory
getwd()
#setwd("//data//")

#Import the csv dataset
data <- read.csv("World Trends.csv")

#Explore the data
data
head(data)      #check top 6 rows
tail(data, 7) #check bottom 7 rows
structure(data)      #check the structure of the data frame
summary(data)      #check the summary of the data

#Did you pick up that there is more than one year in the data?
#From the challenge we know that there are two: 1960 and 2013

#Filter the dataframes
data1960 <- data[data$Year==1960,]
data2013 <- data[data$Year==2013,]

#Check row counts
nrow(data1960) #187 rows
nrow(data2013) #187 rows. Equal split.

#Create the additional dataframes
add1960 <- data.frame(Code=Country_Code, Life.Exp=Life_Expectancy_At_Birth_1960)
add2013 <- data.frame(Code=Country_Code, Life.Exp=Life_Expectancy_At_Birth_2013)

#Check summaries
summary(add1960)
summary(add2013)

#Merge the pairs of dataframes  
merged1960 <- merge(data1960, add1960, by.x="Country.Code", by.y = "Code")
merged2013 <- merge(data2013, add2013, by.x="Country.Code", by.y = "Code")

#Check the new structures
structure(merged1960)
structure(merged2013)

#We can see an obsolete column in each of the merged dataframes
#Column "Year" is no longer required. Let's remove it
merged1960$Year <- NULL
merged2013$Year <- NULL

#Check structures again
structure(merged1960)
structure(merged2013)

#Visualization time
library(ggplot2)

#Visualize the 1960 dataset
qplot(data=merged1960, x=Fertility.Rate, y=Life.Exp,
      color=Region,                               #colour
      size=I(5), 
      alpha=I(0.6),                               #transparency
      main="Life Expectancy vs Fertility (1960)" #title
   )

#Visualize the 2013 dataset
qplot(data=merged2013, x=Fertility.Rate, y=Life.Exp,
      color= Region,
      size=I(5),
      alpha=I(0.6),
      main = "Life Expectancy vs Fertility (2013)"
   )

#Visualise regions and countries

plt <- ggplot(data = merged2013, aes(Region))
plt + geom_bar() 


#Visualise which region has highest 

plt <- ggplot(data = merged1960, aes(Region, Fertility.Rate))
plt + geom_point() + labs(title="Fertility rate per Region 1960")

plt <- ggplot(data = merged2013, aes(Region, Fertility.Rate))
plt + geom_point() + labs(title="Fertility rate per Region 2013")


#Life expectancy
plt <- ggplot(data = merged1960, aes(Region, Life.Exp))
plt + geom_point() + labs(title="Life Expectancy per Region 1960")

plt <- ggplot(data = merged2013, aes(Region, Life.Exp))
plt + geom_point() + labs(title="Life Expectancy per Region 2013")


# Region wise Fertility Rate
# what region contirbutes in Fertility rate

plt <- ggplot(data=merged1960, aes(x="", y=Fertility.Rate, fill=Region))
plt + geom_bar(stat='identity',width=1) + coord_polar("y",start=0) +
   theme_void() + labs(title="Fertility Rate and Regions 2013")

plt <- ggplot(data=merged2013, aes(x="", y=Fertility.Rate, fill=Region))
plt + geom_bar(stat='identity',width=1) + coord_polar("y",start=0) +
   theme_void() + labs(title="Fertility Rate and Regions 2013")

#life expectancy and region
plt <- ggplot(data=merged1960, aes(x="", y=Life.Exp, fill=Region))
plt + geom_bar(stat='identity',width=1) + coord_polar("y",start=0) +
   theme_void() + labs(title="Life expectancy and Regions 2013")

plt <- ggplot(data=merged2013, aes(x="", y=Life.Exp, fill=Region))
plt + geom_bar(stat='identity',width=1) + coord_polar("y",start=0) +
   theme_void() + labs(title="Life expectancy and Regions 2013")


# Life Expectancy and fertility Rate
plt <- ggplot(data = merged1960, aes(Fertility.Rate, Life.Exp, color=Region))
plt + geom_point(size=3, alpha = I(0.6)) + facet_grid(.~Region) + 
   labs(title="Fertility Rate - Life Expectancy per Region 1960",x="Fertility Rate ",y= "Life Expectancy")

plt <- ggplot(data = merged2013, aes(Fertility.Rate, Life.Exp, color=Region))
plt + geom_point(size=3, alpha = I(0.6)) + facet_grid(.~Region) + 
   labs(title="Fertility Rate - Life Expectancy per Region 1960",x="Fertility Rate ",y= "Life Expectancy")

# Histogram
plt <- ggplot(data = merged1960, aes(Fertility.Rate, fill=Region))
plt +  geom_histogram() + labs(title="Fertility Rate Histogram 1960") + facet_grid(Region~.)


#--------------
library(rworldmap)

#create a map-shaped window
#mapDevice('x11')
#join to a coarse resolution map

#1960
country1960 <- joinCountryData2Map(merged1960, joinCode="ISO3", nameJoinColumn="Country.Code")

mapCountryData(country1960, nameColumnToPlot="Fertility.Rate", catMethod="fixedWidth" ,mapTitle='Fertility Rate 1960',colourPalette='rainbow',oceanCol='lightblue')

#2013

country2013 <- joinCountryData2Map(merged2013, joinCode="ISO3", nameJoinColumn="Country.Code")
mapCountryData(country2013, nameColumnToPlot="Fertility.Rate", catMethod="fixedWidth" ,mapTitle='Fertility Rate 2013',colourPalette='rainbow',oceanCol='lightblue')


#Life Expectancy
#1960

mapCountryData(country1960, nameColumnToPlot="Life.Exp", catMethod="fixedWidth" ,mapTitle='Life Expectancy 1960',colourPalette='rainbow',oceanCol='lightblue')

#2013

mapCountryData(country2013, nameColumnToPlot="Life.Exp", catMethod="fixedWidth" ,mapTitle='Life Expectancy 2013',colourPalette='rainbow',oceanCol='lightblue')
