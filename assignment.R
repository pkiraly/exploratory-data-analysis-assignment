setwd('~/Documents/data-science-specialization/exploratory-data-analysis/assignment2')

rm(list=ls())
library(plyr)
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# set year as factor
NEI$year <- factor(NEI$year)
# set type as factor
NEI$type <- factor(NEI$type)

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission from all
# sources for each of the years 1999, 2002, 2005, and 2008.

# preparing data frame as final data set for plotting
yearlyDF <- ddply(NEI, .(year), summarize, emission=sum(Emissions))

# creating plot
png("plot1.png")
plot(yearlyDF$year, yearlyDF$emission,
     type="h", boxwex=0.01,
     xlab = "Years", ylab = "Yearly PM2.5 emission",
     main = "Changing of PM2.5 emission between 1999 and 2008")
lines(yearlyDF$year, yearlyDF$emission)
dev.off()

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# filter by Baltimore
baltimore <- NEI[NEI$fips=="24510",]

# preparing data frame as final data set for plotting
yearlyDF <- ddply(baltimore, .(year), summarize, emission=sum(Emissions))

# creating plot
png("plot2.png")
plot(yearlyDF$year, yearlyDF$emission,
     type="h", boxwex=0.01,
     xlab = "Years", ylab = "Yearly PM2.5 emission",
     main = "Changing of PM2.5 emission in Baltimore")
lines(yearlyDF$year, yearlyDF$emission)
dev.off()

# 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
# variable, which of these four sources have seen decreases in emissions from 1999–2008
# for Baltimore City? Which have seen increases in emissions from 1999–2008?
# Use the ggplot2 plotting system to make a plot answer this question.

# filter by Baltimore
baltimore <- NEI[NEI$fips=="24510",]

# preparing data frame as final data set for plotting
yearlyPerTypeDF <- ddply(baltimore, .(year, type), summarize, emission=sum(Emissions))

# creating plot
png("plot3.png")
gplot <- ggplot(yearlyPerTypeDF, aes(year, emission))
gplot + geom_point() + facet_grid(.~type) + labs(
  x = "Years", y = "Yearly PM2.5 emission",
  title = "PM2.5 emission in Baltimore by types of sources")
dev.off()

# 4. Across the United States, how have emissions from coal combustion-related sources
# changed from 1999–2008?

# filter by combustion
combustion <- subset(SCC, grepl("combustion", SCC.Level.One, ignore.case=T))
# filter by coal
coalCombustion <- subset(combustion, grepl("coal", SCC.Level.Three, ignore.case=T), "SCC")
# filter NEI by SCC numbers for coal combustion types
filteredNEI <- subset(NEI, SCC %in% coalCombustion$SCC)

# preparing data frame as final data set for plotting
yearlyDF <- ddply(filteredNEI, .(year), summarize, emissions=sum(Emissions))

# creating plot
png("plot4.png")
plot(yearlyDF$year, yearlyDF$emission, type="h", boxwex=0.01,
     xlab="Years", ylab="Yearly PM2.5 emission",
     main = "PM2.5 emission from coal combustion-related sources")
lines(yearlyDF$year, yearlyDF$emission)
dev.off()

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# filter by Baltimore
baltimore <- NEI[NEI$fips=="24510", ]

# get vehicle codes
vehicles <- subset(SCC, grepl("vehicles", SCC.Level.Two, ignore.case=T), "SCC")

# filter observations by vehicles
vehiclesInBaltimore <- merge(vehicles, baltimore, by="SCC")

# preparing data frame as final data set for plotting
yearlyDF <- ddply(vehiclesInBaltimore, .(year), summarize, emission=sum(Emissions))

# creating plot
png("plot5.png")
plot(yearlyDF$year, yearlyDF$emission, type="h", boxwex=0.01,
     xlab="Years", ylab="Yearly PM2.5 emission",
     main = "PM2.5 emission from motor vehicle sources in Baltimore")
lines(yearlyDF$year, yearlyDF$emission)
dev.off()

# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions
# from motor vehicle sources in Los Angeles County, California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?

# filter by cities
baltimoreAndLA <- NEI[NEI$fips=="24510"|NEI$fips=="06037", ]

# get vehicle code 
vehicles <- subset(SCC, grepl("vehicles", SCC.Level.Two, ignore.case=T), "SCC")

# filter observations by vehicle codes and cities
filteredNEI <- merge(vehicles, baltimoreAndLA, by="SCC")

# new variable: city = name of cities
filteredNEI$city[filteredNEI$fips=="24510"] <- "Baltimore"
filteredNEI$city[filteredNEI$fips=="06037"] <- "Los Angeles"

# preparing data frame as final data set for plotting
yearlyDF <- ddply(filteredNEI, .(year, city), summarize, emission=sum(Emissions))

# creating plot
png("plot6.png")
gplot <- ggplot(yearlyDF, aes(year, emission))
gplot + geom_point(aes(color=city),size=4) + labs(
  x = "Years", y = "Yearly PM2.5 emission",
  title = "Motor vehicle PM2.5 emission in Baltimore and Los Angeles")
dev.off()
