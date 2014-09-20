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
axis(1,at= plot2, labels=data2$Year)
title(main = expression("Total Emissions in Baltimore City from 1999 to 2008 from PM"[2.5]))
usr <- par( "usr" )
text(usr[2],usr[4],expression("Observing for year 1999, 2002, 2005 and 2008"),adj = c(1,1),col="blue",cex=.8)
dev.off()

# Plot III
#-------------------------------------------------------------------------------------------------------

# Subsetting data related only to Baltimore city and saving it to dataint
# Loading plyr library to use ddply for summing amount of PM2.5 emission in tons
# by year and type of source. data obtained is stored in new dataset called data2 
# whose first column shows the year, the second type of source  and last total 
# emission of above pollutant that year. Columns are labeled accordingly.

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

# Plot V
#-------------------------------------------------------------------------------------------------------
# Subsetting data related only to Baltimore city, and to ON-ROAD type of source 
# s0 from motor vehicules and saving it to dataint2
# Loading plyr library to use ddply for summing amount of PM2.5 emission in tons
# by year. data obtained is stored in new dataset called data5 
# whose first column shows the year and the second  total 
# emission of above pollutant that year. Columns are labeled accordingly.

library(plyr)
dataint2 <- subset(NEI, type=="ON-ROAD" & as.factor(NEI$fips) == 24510)
data5 <- ddply(dataint2, .(as.factor(year)), summarize,tot=sum(as.numeric(Emissions)))
names(data5)[1] <- "Year"

# scipen option is used here to avoid scientif expressions in plot. plot 1 uses
# base plotting system and plots a titled bar plot with duely labeled x - and y -
# axis. A comment box is included on the plot. The plot is then saved in CWD under
# png file named plot1.png. With a subtitle.

png("./plot5.png")
options(scipen=5)
plot1 <- barplot(data5$tot, ylab = expression("Total Emissions in Tons from PM"[2.5]), xlab = "Years")
axis(1,at= plot1, labels=data5$Year)
title(main = expression("Total Emissions in Baltimore City from 1999 to 2008 from PM"[2.5]), sub = list("only from motor vehicle sources", cex=.8, col="red"))
usr <- par( "usr" )
text(usr[2],usr[4],expression("Observing for year 1999, 2002, 2005 and 2008"),adj = c(1,1),col="blue",cex=.8)
dev.off()

# Plot VI
#-------------------------------------------------------------------------------------------------------
# Subsetting data related only to Baltimore city and LA and to ON-ROAD type of source 
# s0 from motor vehicules and saving it to dataint2
# Loading plyr library to use ddply for summing amount of PM2.5 emission in tons
# by year and by county. data obtained is stored in new dataset called data5 
# whose first column shows the year and the second  total 
# emission of above pollutant that year. Columns are labeled accordingly.
library(plyr)
library(ggplot2)

dataint3 <- subset(NEI, type=="ON-ROAD" & (NEI$fips == "24510"|NEI$fips =="06037"))
data6 <- ddply(dataint3, .(as.factor(year),fips), summarize,tot=sum(as.numeric(Emissions)))
data6$fips <- factor(data6$fips, levels=c("24510","06037"), labels=c("Baltimore City","Los Angeles County"))

# calculating change in percentiles of emissions from PM 2.5 for bith counties
# stored in la and ba for los angeles and baltimore respectively.
# binding info on county and year with changes
# binding info of two cities together. plotting changes using ggplot2

changela = rep(0, times=3)
for(i in 2:4){ sub = subset(data6, data6$fips=="Los Angeles County") 
               changela[i] = round(((sub$tot[i]-sub$tot[i-1])/sub$tot[i-1]), 3)
}
changeba = rep(0, times=3)
for(i in 2:4){ sub = subset(data6, data6$fips=="Baltimore City") 
               changeba[i] = round(((sub$tot[i]-sub$tot[i-1])/sub$tot[i-1]), 3)
}

la <- cbind(changela, rep("Los Angeles", times = 4), c("1999","2002","2005","2008"))
ba <- cbind(changeba, rep("Baltimore City", times = 4), c("1999","2002","2005","2008"))
data7 <- data.frame(rbind(la,ba))
colnames(data7) <- c("change","county","year")

png("./plot6.png")
plot.title="Change in % in Total Emissions from motor vehicle sources from 1999 to 2008 from PM"
plot.subtitle="Baltimore City against Los Angeles County"
plot3 <- ggplot(data7, aes(year, change, group = county))
plot3 <- plot3 + geom_line(aes(color = county))  + labs(y=expression("Change in % in Total Emissions from PM"[2.5]))
plot3 <- plot3 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + labs(x="Years")
plot3
dev.off()
