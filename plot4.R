# Downloading in current working directory (CWD) and Loading data sets

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url, "exdata-data-NEI_data.zip", mode="wb")
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot IV
#-------------------------------------------------------------------------------------------------------
# Subsetting data SCC to store in SCC4.1 observations pretaining only to Coal.
# subsetting data SCC4.1 to store to SCC4.2 observations pretaining only to Combustion
# SCC column of SCC4.2 data is used to selected from NEI dataset only data 
# related to coal combustion emission of PM 2.5 and store it in dataint1
# Loading plyr library to use ddply for summing amount of PM2.5 emission in tons
# by year. data obtained is stored in new dataset called data4. 
# whose first column shows the year and second the total emission of above 
# pollutant that year. Columns are labeled accordingly.

library(plyr)
SCC4.1 <- SCC[grep("Coal", SCC$Short.Name),]
SCC4.2 <- SCC4.1[grep("Fuel Comb", SCC4.1$EI.Sector),]
selectr <- (NEI$SCC %in% SCC4.2$SCC)
dataint1 <- NEI[selectr,]

data4 <- ddply(dataint1, .(as.factor(year)), summarize,tot=sum(as.numeric(Emissions)))
names(data4)[1] <- "Year"

# scipen option is used here to avoid scientif expressions in plot. plot 1 uses
# base plotting system and plots a titled bar plot with duely labeled x - and y -
# axis. A comment box is included on the plot. The plot is then saved in CWD under
# png file named plot1.png. With a subtitle.

png("./plot4.png")
options(scipen=5)
plot1 <- barplot(data4$tot, ylab = expression("Total Emissions in Tons from PM"[2.5]), xlab = "Years")
axis(1,at= plot1, labels=data4$Year)
title(main = expression("Total Emissions in the United States from 1999 to 2008 from PM"[2.5]), sub = list("only from coal combustion-related sources", cex=.8, col="red"))
usr <- par( "usr" )
text(usr[2],usr[4],expression("Observing for year 1999, 2002, 2005 and 2008"),adj = c(1,1),col="blue",cex=.8)
dev.off()