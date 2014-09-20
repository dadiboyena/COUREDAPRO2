# Downloading in current working directory (CWD) and Loading data sets

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url, "exdata-data-NEI_data.zip", mode="wb")
unzip("exdata-data-NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

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
plot.title="Change in % in Total Emissions from 1999 to 2008 from PM"
plot.subtitle="only from motor vehicle sources (Baltimore City against Los Angeles County)"
plot3 <- ggplot(data7, aes(year, change, group = county))
plot3 <- plot3 + geom_line(aes(color = county))  + labs(y=expression("Change in % in Total Emissions from PM"[2.5]))
plot3 <- plot3 + ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), "")))) + labs(x="Years")
plot3
dev.off()