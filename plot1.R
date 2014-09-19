# Downloading in current working directory (CWD) and Loading data sets

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url, "exdata-data-NEI_data.zip", mode="wb")
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Plot I
#-------------------------------------------------------------------------------------------------------
# Loading plyr library to use ddply for summing amount of PM2.5 emission in tons
# by year. data obtained is stored in new dataset called data1 whose first column
# shows the year and second total emission of above pollutant that year. Columns
# are labeled accordingly.

library(plyr)
data1 <- ddply(NEI, .(as.factor(year)), summarize,tot=sum(as.numeric(Emissions)))
names(data1)[1] <- "Year"

# scipen option is used here to avoid scientif expressions in plot. plot 1 uses
# base plotting system and plots a titled bar plot with duely labeled x - and y -
# axis. A comment box is included on the plot. The plot is then saved in CWD under
# png file named plot1.png.

png("./plot1.png")
options(scipen=5)
plot1 <- barplot(data1$tot, ylab = expression("Total Emissions in Tons from PM"[2.5]), xlab = "Years")
axis(1,at= plot1, labels=data1$Year)
title(main = expression("Total Emissions in the United States from 1999 to 2008 from PM"[2.5]))
usr <- par( "usr" )
text(usr[2],usr[4],expression("Observing for year 1999, 2002, 2005 and 2008"),adj = c(1,1),col="blue",cex=.8)
dev.off()
