# Downloading in current working directory (CWD) and Loading data sets

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url, "exdata-data-NEI_data.zip", mode="wb")
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(plyr)
library(ggplot2)
dataint <- subset(NEI, as.factor(NEI$fips) == 24510)
data3 <- ddply(dataint, .(as.factor(year),as.factor(type)), summarize,tot=sum(as.numeric(Emissions)))
names(data3)[1] <- "Year"
names(data3)[2] <- "Type"


# plot3 uses ggplot plotting system and plots a titled lines plot with duely labeled x - and y -
# axis. A legend is included on the plot. The plot is then saved in CWD under
# png file named plot3.png.

png("./plot3.png")
plot.title="Total Emissions in Baltimore City from 1999 to 2008 from PM"
plot.subtitle="by type of source"
plot3 <- ggplot(data3, aes(Year, tot, group = Type))
plot3 <- plot3 + geom_line(aes(color = Type))  + labs(y=expression("Total Emissions in Tons from PM"[2.5]))
plot3 <- plot3 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + labs(x="Years")
plot3
dev.off()