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

# Plot II
#-------------------------------------------------------------------------------------------------------
# Subsetting data NEI to store in dataint observations pretaining only to Baltimore city.
# Loading plyr library to use ddply for summing amount of PM2.5 emission in tons
# by year. data obtained is stored in new dataset called data2 whose first column
# shows the year and second total emission of above pollutant that year. Columns
# are labeled accordingly.

library(plyr)
dataint <- subset(NEI, as.factor(NEI$fips) == 24510)
data2 <- ddply(dataint, .(as.factor(year)), summarize,tot=sum(as.numeric(Emissions)))
names(data1)[2] <- "Year"

# scipen option is used here to avoid scientif expressions in plot. plot2 uses
# base plotting system and plots a titled bar plot with duely labeled x - and y -
# axis. A comment box is included on the plot. The plot is then saved in CWD under
# png file named plot2.png.

png("./plot2.png")
options(scipen=5)
plot2 <- barplot(data2$tot, ylab = expression("Total Emissions in Tons from PM"[2.5]), xlab = "Years")
axis(1,at= plot2, labels=data1$Year)
title(main = expression("Total Emissions in Baltimore City from 1999 to 2008 from PM"[2.5]))
usr <- par( "usr" )
text(usr[2],usr[4],expression("Observing for year 1999, 2002, 2005 and 2008"),adj = c(1,1),col="blue",cex=.8)
dev.off()

# Plot III
#-------------------------------------------------------------------------------------------------------
# Loading plyr library to use ddply for summing amount of PM2.5 emission in tons
# by year and by source type. data obtained is stored in new dataset called data3 
# whose first column shows the year, second type and last total emission of above 
# pollutant that year. Columns are labeled accordingly.

library(plyr)
library(ggplot2)
data3 <- ddply(dataint, .(as.factor(year),as.factor(type)), summarize,tot=sum(as.numeric(Emissions)))
names(data3)[1] <- "Year"
names(data3)[2] <- "Type"


# plot3 uses ggplot plotting system and plots a titled lines plot with duely labeled x - and y -
# axis. A legend is included on the plot. The plot is then saved in CWD under
# png file named plot3.png.

png("./plot3.png")
plot.title="Total Emissions in the United States from 1999 to 2008, from PM"
plot.subtitle="by type of source"
plot3 <- ggplot(data3, aes(Year, tot, group = Type))
plot3 <- plot3 + geom_line(aes(color = Type))  + labs(y=expression("Total Emissions in Tons from PM"[2.5]))
plot3 <- plot3 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + labs(x="Years")
plot3
dev.off()


